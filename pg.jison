%code imports {
	const _ = require('./src');
	const { ereport, errcode, errmsg, parser_errposition } = require('./src/common/errors');
	const NULL = null;
	const NIL = null;
	const yyscanner = null;
}

%{
function makeRawStmt(stmt, stmt_location) {
	return {
		stmt,
		stmt_location,
		stmt_len: 0				/* might get changed later */
	}
}

/* Adjust a RawStmt to reflect that it doesn't run to the end of the string */
function updateRawStmtEnd(rs, end_location) {
	/*
	 * If we already set the length, don't change it.  This is for situations
	 * like "select foo ;; select bar" where the same statement will be last
	 * in the string for more than one semicolon.
	 */
	if (rs.stmt_len > 0)
		return;

	/* OK, update length of RawStmt */
	rs.stmt_len = end_location - rs.stmt_location;
}

function IsA(arg, nodeTag) {
	return typeof arg === 'object' && arg.type === 'T_' + nodeTag;
}

%}
/*
 * Non-keyword token types.  These are hard-wired into the "flex" lexer.
 * They must be listed first so that their numeric codes do not depend on
 * the set of keywords.  PL/pgSQL depends on this so that it can share the
 * same lexer.  If you add/change tokens here, fix PL/pgSQL to match!
 *
 * DOT_DOT is unused in the core SQL grammar, and so will always provoke
 * parse errors.  It is needed by PL/pgSQL.
 */
%token IDENT FCONST SCONST BCONST XCONST Op
%token ICONST PARAM
%token TYPECAST DOT_DOT COLON_EQUALS EQUALS_GREATER
%token LESS_EQUALS GREATER_EQUALS NOT_EQUALS

/*
 * If you want to make any keyword changes, update the keyword table in
 * src/include/parser/kwlist.h and add new keywords to the appropriate one
 * of the reserved-or-not-so-reserved keyword lists, below; search
 * this file for "Keyword category lists".
 */

/* ordinary key words in alphabetical order */
%token ABORT_P ABSOLUTE_P ACCESS ACTION ADD_P ADMIN AFTER
	AGGREGATE ALL ALSO ALTER ALWAYS ANALYSE ANALYZE AND ANY ARRAY AS ASC
	ASSERTION ASSIGNMENT ASYMMETRIC AT ATTACH ATTRIBUTE AUTHORIZATION

	BACKWARD BEFORE BEGIN_P BETWEEN BIGINT BINARY BIT
	BOOLEAN_P BOTH BY

	CACHE CALL CALLED CASCADE CASCADED CASE CAST CATALOG_P CHAIN CHAR_P
	CHARACTER CHARACTERISTICS CHECK CHECKPOINT CLASS CLOSE
	CLUSTER COALESCE COLLATE COLLATION COLUMN COLUMNS COMMENT COMMENTS COMMIT
	COMMITTED CONCURRENTLY CONFIGURATION CONFLICT CONNECTION CONSTRAINT
	CONSTRAINTS CONTENT_P CONTINUE_P CONVERSION_P COPY COST CREATE
	CROSS CSV CUBE CURRENT_P
	CURRENT_CATALOG CURRENT_DATE CURRENT_ROLE CURRENT_SCHEMA
	CURRENT_TIME CURRENT_TIMESTAMP CURRENT_USER CURSOR CYCLE

	DATA_P DATABASE DAY_P DEALLOCATE DEC DECIMAL_P DECLARE DEFAULT DEFAULTS
	DEFERRABLE DEFERRED DEFINER DELETE_P DELIMITER DELIMITERS DEPENDS DESC
	DETACH DICTIONARY DISABLE_P DISCARD DISTINCT DO DOCUMENT_P DOMAIN_P
	DOUBLE_P DROP

	EACH ELSE ENABLE_P ENCODING ENCRYPTED END_P ENUM_P ESCAPE EVENT EXCEPT
	EXCLUDE EXCLUDING EXCLUSIVE EXECUTE EXISTS EXPLAIN
	EXTENSION EXTERNAL EXTRACT

	FALSE_P FAMILY FETCH FILTER FIRST_P FLOAT_P FOLLOWING FOR
	FORCE FOREIGN FORWARD FREEZE FROM FULL FUNCTION FUNCTIONS

	GENERATED GLOBAL GRANT GRANTED GREATEST GROUP_P GROUPING GROUPS

	HANDLER HAVING HEADER_P HOLD HOUR_P

	IDENTITY_P IF_P ILIKE IMMEDIATE IMMUTABLE IMPLICIT_P IMPORT_P IN_P INCLUDE
	INCLUDING INCREMENT INDEX INDEXES INHERIT INHERITS INITIALLY INLINE_P
	INNER_P INOUT INPUT_P INSENSITIVE INSERT INSTEAD INT_P INTEGER
	INTERSECT INTERVAL INTO INVOKER IS ISNULL ISOLATION

	JOIN

	KEY

	LABEL LANGUAGE LARGE_P LAST_P LATERAL_P
	LEADING LEAKPROOF LEAST LEFT LEVEL LIKE LIMIT LISTEN LOAD LOCAL
	LOCALTIME LOCALTIMESTAMP LOCATION LOCK_P LOCKED LOGGED

	MAPPING MATCH MATERIALIZED MAXVALUE METHOD MINUTE_P MINVALUE MODE MONTH_P MOVE

	NAME_P NAMES NATIONAL NATURAL NCHAR NEW NEXT NO NONE
	NOT NOTHING NOTIFY NOTNULL NOWAIT NULL_P NULLIF
	NULLS_P NUMERIC

	OBJECT_P OF OFF OFFSET OIDS OLD ON ONLY OPERATOR OPTION OPTIONS OR
	ORDER ORDINALITY OTHERS OUT_P OUTER_P
	OVER OVERLAPS OVERLAY OVERRIDING OWNED OWNER

	PARALLEL PARSER PARTIAL PARTITION PASSING PASSWORD PLACING PLANS POLICY
	POSITION PRECEDING PRECISION PRESERVE PREPARE PREPARED PRIMARY
	PRIOR PRIVILEGES PROCEDURAL PROCEDURE PROCEDURES PROGRAM PUBLICATION

	QUOTE

	RANGE READ REAL REASSIGN RECHECK RECURSIVE REF REFERENCES REFERENCING
	REFRESH REINDEX RELATIVE_P RELEASE RENAME REPEATABLE REPLACE REPLICA
	RESET RESTART RESTRICT RETURNING RETURNS REVOKE RIGHT ROLE ROLLBACK ROLLUP
	ROUTINE ROUTINES ROW ROWS RULE

	SAVEPOINT SCHEMA SCHEMAS SCROLL SEARCH SECOND_P SECURITY SELECT SEQUENCE SEQUENCES
	SERIALIZABLE SERVER SESSION SESSION_USER SET SETS SETOF SHARE SHOW
	SIMILAR SIMPLE SKIP SMALLINT SNAPSHOT SOME SQL_P STABLE STANDALONE_P
	START STATEMENT STATISTICS STDIN STDOUT STORAGE STORED STRICT_P STRIP_P
	SUBSCRIPTION SUBSTRING SUPPORT SYMMETRIC SYSID SYSTEM_P

	TABLE TABLES TABLESAMPLE TABLESPACE TEMP TEMPLATE TEMPORARY TEXT_P THEN
	TIES TIME TIMESTAMP TO TRAILING TRANSACTION TRANSFORM
	TREAT TRIGGER TRIM TRUE_P
	TRUNCATE TRUSTED TYPE_P TYPES_P

	UNBOUNDED UNCOMMITTED UNENCRYPTED UNION UNIQUE UNKNOWN UNLISTEN UNLOGGED
	UNTIL UPDATE USER USING

	VACUUM VALID VALIDATE VALIDATOR VALUE_P VALUES VARCHAR VARIADIC VARYING
	VERBOSE VERSION_P VIEW VIEWS VOLATILE

	WHEN WHERE WHITESPACE_P WINDOW WITH WITHIN WITHOUT WORK WRAPPER WRITE

	XML_P XMLATTRIBUTES XMLCONCAT XMLELEMENT XMLEXISTS XMLFOREST XMLNAMESPACES
	XMLPARSE XMLPI XMLROOT XMLSERIALIZE XMLTABLE

	YEAR_P YES_P

	ZONE

/*
 * The grammar thinks these are keywords, but they are not in the kwlist.h
 * list and so can never be entered directly.  The filter in parser.c
 * creates these tokens when required (based on looking one token ahead).
 *
 * NOT_LA exists so that productions such as NOT LIKE can be given the same
 * precedence as LIKE; otherwise they'd effectively have the same precedence
 * as NOT, at least with respect to their left-hand subexpression.
 * NULLS_LA and WITH_LA are needed to make the grammar LALR(1).
 */
%token		NOT_LA NULLS_LA WITH_LA

%left		UNION EXCEPT
%left		INTERSECT
%left		OR
%left		AND
%right		NOT
%nonassoc	IS ISNULL NOTNULL	/* IS sets precedence for IS NULL, etc */
%nonassoc	'<' '>' '=' LESS_EQUALS GREATER_EQUALS NOT_EQUALS
%nonassoc	BETWEEN IN_P LIKE ILIKE SIMILAR NOT_LA
%nonassoc	ESCAPE			/* ESCAPE must be just above LIKE/ILIKE/SIMILAR */
%left		POSTFIXOP		/* dummy for postfix Op rules */
%nonassoc	UNBOUNDED		/* ideally should have same precedence as IDENT */
%nonassoc	IDENT GENERATED NULL_P PARTITION RANGE ROWS GROUPS PRECEDING FOLLOWING CUBE ROLLUP
%left		Op OPERATOR		/* multi-character ops and user-defined operators */
%left		'+' '-'
%left		'*' '/' '%'
%left		'^'
/* Unary Operators */
%left		AT				/* sets precedence for AT TIME ZONE */
%left		COLLATE
%right		UMINUS
%left		'[' ']'
%left		'(' ')'
%left		TYPECAST
%left		'.'

%start stmtblock

%%

unreserved_keyword:
			  ABORT_P
			| ABSOLUTE_P
			| ACCESS
			| ACTION
			| ADD_P
			| ADMIN
			| AFTER
			| AGGREGATE
			| ALSO
			| ALTER
			| ALWAYS
			| ASSERTION
			| ASSIGNMENT
			| AT
			| ATTACH
			| ATTRIBUTE
			| BACKWARD
			| BEFORE
			| BEGIN_P
			| BY
			| CACHE
			| CALL
			| CALLED
			| CASCADE
			| CASCADED
			| CATALOG_P
			| CHAIN
			| CHARACTERISTICS
			| CHECKPOINT
			| CLASS
			| CLOSE
			| CLUSTER
			| COLUMNS
			| COMMENT
			| COMMENTS
			| COMMIT
			| COMMITTED
			| CONFIGURATION
			| CONFLICT
			| CONNECTION
			| CONSTRAINTS
			| CONTENT_P
			| CONTINUE_P
			| CONVERSION_P
			| COPY
			| COST
			| CSV
			| CUBE
			| CURRENT_P
			| CURSOR
			| CYCLE
			| DATA_P
			| DATABASE
			| DAY_P
			| DEALLOCATE
			| DECLARE
			| DEFAULTS
			| DEFERRED
			| DEFINER
			| DELETE_P
			| DELIMITER
			| DELIMITERS
			| DEPENDS
			| DETACH
			| DICTIONARY
			| DISABLE_P
			| DISCARD
			| DOCUMENT_P
			| DOMAIN_P
			| DOUBLE_P
			| DROP
			| EACH
			| ENABLE_P
			| ENCODING
			| ENCRYPTED
			| ENUM_P
			| ESCAPE
			| EVENT
			| EXCLUDE
			| EXCLUDING
			| EXCLUSIVE
			| EXECUTE
			| EXPLAIN
			| EXTENSION
			| EXTERNAL
			| FAMILY
			| FILTER
			| FIRST_P
			| FOLLOWING
			| FORCE
			| FORWARD
			| FUNCTION
			| FUNCTIONS
			| GENERATED
			| GLOBAL
			| GRANTED
			| GROUPS
			| HANDLER
			| HEADER_P
			| HOLD
			| HOUR_P
			| IDENTITY_P
			| IF_P
			| IMMEDIATE
			| IMMUTABLE
			| IMPLICIT_P
			| IMPORT_P
			| INCLUDE
			| INCLUDING
			| INCREMENT
			| INDEX
			| INDEXES
			| INHERIT
			| INHERITS
			| INLINE_P
			| INPUT_P
			| INSENSITIVE
			| INSERT
			| INSTEAD
			| INVOKER
			| ISOLATION
			| KEY
			| LABEL
			| LANGUAGE
			| LARGE_P
			| LAST_P
			| LEAKPROOF
			| LEVEL
			| LISTEN
			| LOAD
			| LOCAL
			| LOCATION
			| LOCK_P
			| LOCKED
			| LOGGED
			| MAPPING
			| MATCH
			| MATERIALIZED
			| MAXVALUE
			| METHOD
			| MINUTE_P
			| MINVALUE
			| MODE
			| MONTH_P
			| MOVE
			| NAME_P
			| NAMES
			| NEW
			| NEXT
			| NO
			| NOTHING
			| NOTIFY
			| NOWAIT
			| NULLS_P
			| OBJECT_P
			| OF
			| OFF
			| OIDS
			| OLD
			| OPERATOR
			| OPTION
			| OPTIONS
			| ORDINALITY
			| OTHERS
			| OVER
			| OVERRIDING
			| OWNED
			| OWNER
			| PARALLEL
			| PARSER
			| PARTIAL
			| PARTITION
			| PASSING
			| PASSWORD
			| PLANS
			| POLICY
			| PRECEDING
			| PREPARE
			| PREPARED
			| PRESERVE
			| PRIOR
			| PRIVILEGES
			| PROCEDURAL
			| PROCEDURE
			| PROCEDURES
			| PROGRAM
			| PUBLICATION
			| QUOTE
			| RANGE
			| READ
			| REASSIGN
			| RECHECK
			| RECURSIVE
			| REF
			| REFERENCING
			| REFRESH
			| REINDEX
			| RELATIVE_P
			| RELEASE
			| RENAME
			| REPEATABLE
			| REPLACE
			| REPLICA
			| RESET
			| RESTART
			| RESTRICT
			| RETURNS
			| REVOKE
			| ROLE
			| ROLLBACK
			| ROLLUP
			| ROUTINE
			| ROUTINES
			| ROWS
			| RULE
			| SAVEPOINT
			| SCHEMA
			| SCHEMAS
			| SCROLL
			| SEARCH
			| SECOND_P
			| SECURITY
			| SEQUENCE
			| SEQUENCES
			| SERIALIZABLE
			| SERVER
			| SESSION
			| SET
			| SETS
			| SHARE
			| SHOW
			| SIMPLE
			| SKIP
			| SNAPSHOT
			| SQL_P
			| STABLE
			| STANDALONE_P
			| START
			| STATEMENT
			| STATISTICS
			| STDIN
			| STDOUT
			| STORAGE
			| STORED
			| STRICT_P
			| STRIP_P
			| SUBSCRIPTION
			| SUPPORT
			| SYSID
			| SYSTEM_P
			| TABLES
			| TABLESPACE
			| TEMP
			| TEMPLATE
			| TEMPORARY
			| TEXT_P
			| TIES
			| TRANSACTION
			| TRANSFORM
			| TRIGGER
			| TRUNCATE
			| TRUSTED
			| TYPE_P
			| TYPES_P
			| UNBOUNDED
			| UNCOMMITTED
			| UNENCRYPTED
			| UNKNOWN
			| UNLISTEN
			| UNLOGGED
			| UNTIL
			| UPDATE
			| VACUUM
			| VALID
			| VALIDATE
			| VALIDATOR
			| VALUE_P
			| VARYING
			| VERSION_P
			| VIEW
			| VIEWS
			| VOLATILE
			| WHITESPACE_P
			| WITHIN
			| WITHOUT
			| WORK
			| WRAPPER
			| WRITE
			| XML_P
			| YEAR_P
			| YES_P
			| ZONE
		;


/* Column identifier --- keywords that can be column, table, etc names.
 *
 * Many of these keywords will in fact be recognized as type or function
 * names too; but they have special productions for the purpose, and so
 * can't be treated as "generic" type or function names.
 *
 * The type names appearing here are not usable as function names
 * because they can be followed by '(' in typename productions, which
 * looks too much like a function call for an LR(1) parser.
 */
col_name_keyword:
			  BETWEEN
			| BIGINT
			| BIT
			| BOOLEAN_P
			| CHAR_P
			| CHARACTER
			| COALESCE
			| DEC
			| DECIMAL_P
			| EXISTS
			| EXTRACT
			| FLOAT_P
			| GREATEST
			| GROUPING
			| INOUT
			| INT_P
			| INTEGER
			| INTERVAL
			| LEAST
			| NATIONAL
			| NCHAR
			| NONE
			| NULLIF
			| NUMERIC
			| OUT_P
			| OVERLAY
			| POSITION
			| PRECISION
			| REAL
			| ROW
			| SETOF
			| SMALLINT
			| SUBSTRING
			| TIME
			| TIMESTAMP
			| TREAT
			| TRIM
			| VALUES
			| VARCHAR
			| XMLATTRIBUTES
			| XMLCONCAT
			| XMLELEMENT
			| XMLEXISTS
			| XMLFOREST
			| XMLNAMESPACES
			| XMLPARSE
			| XMLPI
			| XMLROOT
			| XMLSERIALIZE
			| XMLTABLE
		;

stmtblock:	stmtmulti       { return $1; }
            | stmtmulti EOF { return $1; }
        ;

stmtmulti:	stmtmulti ';' stmt
				{
					if ($1 != null)
					{
						/* update length of previous stmt */
						updateRawStmtEnd($1[$1.length - 1], @2);
					}
					if ($3 != null) {
						$1.push(makeRawStmt($3, @2));
						$$ = $1;
					} else
						$$ = $1;
				}
			| stmt
				{
					if ($1 != null)
						$$ = [makeRawStmt($1, null)];
					else
						$$ = null;
				}
		;

stmt:
	SelectStmt
	| /* EMPTY */ { $$ = null };

PreparableStmt:
			SelectStmt
		;

/* A complete SELECT statement looks like this.
 *
 * The rule returns either a single SelectStmt node or a tree of them,
 * representing a set-operation tree.
 *
 * There is an ambiguity when a sub-SELECT is within an a_expr and there
 * are excess parentheses: do the parentheses belong to the sub-SELECT or
 * to the surrounding a_expr?  We don't really care, but bison wants to know.
 * To resolve the ambiguity, we are careful to define the grammar so that
 * the decision is staved off as long as possible: as long as we can keep
 * absorbing parentheses into the sub-SELECT, we will do so, and only when
 * it's no longer possible to do that will we decide that parens belong to
 * the expression.	For example, in "SELECT (((SELECT 2)) + 3)" the extra
 * parentheses are treated as part of the sub-select.  The necessity of doing
 * it that way is shown by "SELECT (((SELECT 2)) UNION SELECT 2)".	Had we
 * parsed "((SELECT 2))" as an a_expr, it'd be too late to go back to the
 * SELECT viewpoint when we see the UNION.
 *
 * This approach is implemented by defining a nonterminal select_with_parens,
 * which represents a SELECT with at least one outer layer of parentheses,
 * and being careful to use select_with_parens, never '(' SelectStmt ')',
 * in the expression grammar.  We will then have shift-reduce conflicts
 * which we can resolve in favor of always treating '(' <select> ')' as
 * a select_with_parens.  To resolve the conflicts, the productions that
 * conflict with the select_with_parens productions are manually given
 * precedences lower than the precedence of ')', thereby ensuring that we
 * shift ')' (and then reduce to select_with_parens) rather than trying to
 * reduce the inner <select> nonterminal to something else.  We use UMINUS
 * precedence for this, which is a fairly arbitrary choice.
 *
 * To be able to define select_with_parens itself without ambiguity, we need
 * a nonterminal select_no_parens that represents a SELECT structure with no
 * outermost parentheses.  This is a little bit tedious, but it works.
 *
 * In non-expression contexts, we use SelectStmt which can represent a SELECT
 * with or without outer parentheses.
 */

SelectStmt: select_no_parens			%prec UMINUS
			| select_with_parens		%prec UMINUS
		;

select_with_parens:
			'(' select_no_parens ')'				{ $$ = $2; }
			| '(' select_with_parens ')'			{ $$ = $2; }
		;

/*
 * This rule parses the equivalent of the standard's <query expression>.
 * The duplicative productions are annoying, but hard to get rid of without
 * creating shift/reduce conflicts.
 *
 *	The locking clause (FOR UPDATE etc) may be before or after LIMIT/OFFSET.
 *	In <=7.2.X, LIMIT/OFFSET had to be after FOR UPDATE
 *	We now support both orderings, but prefer LIMIT/OFFSET before the locking
 * clause.
 *	2002-08-28 bjm
 */
select_no_parens:
			simple_select						{ $$ = $1; }
			| select_clause sort_clause
				{
					_.insertSelectOptions($1, $2, null,
										null, null, null,
										yyscanner);
					$$ = $1;
				}
			| select_clause opt_sort_clause for_locking_clause opt_select_limit
				{
					_.insertSelectOptions($1, $2, $3,
										$4[0], $4[1],
										null,
										yyscanner);
					$$ = $1;
				}
			| select_clause opt_sort_clause select_limit opt_for_locking_clause
				{
					_.insertSelectOptions($1, $2, $4,
										$3[0], $3[1],
										null,
										yyscanner);
					$$ = $1;
				}
			| with_clause select_clause
				{
					_.insertSelectOptions($2, null, null,
										null, null,
										$1,
										yyscanner);
					$$ = $2;
				}
			| with_clause select_clause sort_clause
				{
					_.insertSelectOptions($2, $3, null,
										null, null,
										$1,
										yyscanner);
					$$ = $2;
				}
			| with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
				{
					_.insertSelectOptions($2, $3, $4,
										$5[0], $5[1],
										$1,
										yyscanner);
					$$ = $2;
				}
			| with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
				{
					_.insertSelectOptions($2, $3, $5,
										$4[0], $4[1],
										$1,
										yyscanner);
					$$ = $2;
				}
		;

select_clause:
			simple_select						{ $$ = $1; }
			| select_with_parens				{ $$ = $1; }
		;

/*
 * This rule parses SELECT statements that can appear within set operations,
 * including UNION, INTERSECT and EXCEPT.  '(' and ')' can be used to specify
 * the ordering of the set operations.	Without '(' and ')' we want the
 * operations to be ordered per the precedence specs at the head of this file.
 *
 * As with select_no_parens, simple_select cannot have outer parentheses,
 * but can have parenthesized subclauses.
 *
 * Note that sort clauses cannot be included at this level --- SQL requires
 *		SELECT foo UNION SELECT bar ORDER BY baz
 * to be parsed as
 *		(SELECT foo UNION SELECT bar) ORDER BY baz
 * not
 *		SELECT foo UNION (SELECT bar ORDER BY baz)
 * Likewise for WITH, FOR UPDATE and LIMIT.  Therefore, those clauses are
 * described as part of the select_no_parens production, not simple_select.
 * This does not limit functionality, because you can reintroduce these
 * clauses inside parentheses.
 *
 * NOTE: only the leftmost component SelectStmt should have INTO.
 * However, this is not checked by the grammar; parse analysis must check it.
 */
simple_select:
			SELECT opt_all_clause opt_target_list
			into_clause from_clause where_clause
			group_clause having_clause window_clause
				{
					$$ = {
						type: _.NodeTag.T_SelectStmt,
						targetList: $3,
						intoClause: $4,
						fromClause: $5,
						whereClause: $6,
						groupClause: $7,
						havingClause: $8,
						windowClause: $9
					};
				}
			| SELECT distinct_clause target_list
			into_clause from_clause where_clause
			group_clause having_clause window_clause
				{
					$$ = {
						type: _.NodeTag.T_SelectStmt,
						distinctClause: $2,
						targetList: $3,
						intoClause: $4,
						fromClause: $5,
						whereClause: $6,
						groupClause: $7,
						havingClause: $8,
						windowClause: $9
					};
				}
			| values_clause							{ $$ = $1; }
			| TABLE relation_expr
				{
					/* same as SELECT * FROM relation_expr */
					var cr = {
						type: _.NodeTag.T_ColumnRef,
						fields: [{
							type: _.NodeTag.T_A_Star,
							location: -1
						}]
					};

					var rt = {
						type: _.NodeTag.T_ResTarget,
						name: null,
						indirection: null,
						val: cr,
						location: -1
					};

					$$ = {
						type: _.NodeTag.T_SelectStmt,
						targetList: [rt],
						fromClause: [$2]
					};
				}
			| select_clause UNION all_or_distinct select_clause
				{
					$$ = _.makeSetOp(_.SetOperation.SETOP_UNION, $3, $1, $4);
				}
			| select_clause INTERSECT all_or_distinct select_clause
				{
					$$ = _.makeSetOp(_.SetOperation.SETOP_INTERSECT, $3, $1, $4);
				}
			| select_clause EXCEPT all_or_distinct select_clause
				{
					$$ = _.makeSetOp(_.SetOperation.SETOP_EXCEPT, $3, $1, $4);
				}
		;

/*
 * SQL standard WITH clause looks like:
 *
 * WITH [ RECURSIVE ] <query name> [ (<column>,...) ]
 *		AS (query) [ SEARCH or CYCLE clause ]
 *
 * We don't currently support the SEARCH or CYCLE clause.
 *
 * Recognizing WITH_LA here allows a CTE to be named TIME or ORDINALITY.
 */
with_clause:
		WITH cte_list
			{
				$$ = {
					type: _.NodeTag.T_WithClause,
					ctes: $2,
					recursive: false,
					location: @1
				};
			}
		| WITH_LA cte_list
			{
				$$ = {
					type: _.NodeTag.T_WithClause,
					ctes: $2,
					recursive: false,
					location: @1
				};
			}
		| WITH RECURSIVE cte_list
			{
				$$ = {
					type: _.NodeTag.T_WithClause,
					ctes: $3,
					recursive: true,
					location: @1
				};
			}
		;

cte_list:
		common_table_expr						{ $$ = [$1]; }
		| cte_list ',' common_table_expr		{ $$ = $1; $1.push($3); }
		;

common_table_expr:  name opt_name_list AS opt_materialized '(' PreparableStmt ')'
			{
				$$ = {
					type: _.NodeTag.T_CommonTableExpr,
					ctename: $1,
					aliascolnames: $2,
					ctematerialized: $4,
					ctequery: $6,
					location: @1
				};
			}
		;

opt_materialized:
		MATERIALIZED							{ $$ = _.CTEMaterialize.CTEMaterializeAlways; }
		| NOT MATERIALIZED						{ $$ = _.CTEMaterialize.CTEMaterializeNever; }
		| /*EMPTY*/								{ $$ = _.CTEMaterialize.CTEMaterializeDefault; }
		;

opt_with_clause:
		with_clause								{ $$ = $1; }
		| /*EMPTY*/								{ $$ = null; }
		;

into_clause:
			INTO OptTempTableName
				{
					$$ = {
						type: _.NodeTag.T_IntoClause,
						rel: $2,
						colNames: null,
						options: null,
						onCommit: ONCOMMIT_NOOP,
						tableSpaceName: null,
						viewQuery: null,
						skipData: false,
					};
				}
			| /*EMPTY*/
				{ $$ = null; }
		;

/*
 * This syntax for group_clause tries to follow the spec quite closely.
 * However, the spec allows only column references, not expressions,
 * which introduces an ambiguity between implicit row constructors
 * (a,b) and lists of column references.
 *
 * We handle this by using the a_expr production for what the spec calls
 * <ordinary grouping set>, which in the spec represents either one column
 * reference or a parenthesized list of column references. Then, we check the
 * top node of the a_expr to see if it's an implicit RowExpr, and if so, just
 * grab and use the list, discarding the node. (this is done in parse analysis,
 * not here)
 *
 * (we abuse the row_format field of RowExpr to distinguish implicit and
 * explicit row constructors; it's debatable if anyone sanely wants to use them
 * in a group clause, but if they have a reason to, we make it possible.)
 *
 * Each item in the group_clause list is either an expression tree or a
 * GroupingSet node of some type.
 */
group_clause:
			GROUP_P BY group_by_list				{ $$ = $3; }
			| /*EMPTY*/								{ $$ = null; }
		;

group_by_list:
			group_by_item							{ $$ = [$1]; }
			| group_by_list ',' group_by_item		{ $$ = $1; $1.push($3); }
		;

group_by_item:
			a_expr									{ $$ = $1; }
			| empty_grouping_set					{ $$ = $1; }
			| cube_clause							{ $$ = $1; }
			| rollup_clause							{ $$ = $1; }
			| grouping_sets_clause					{ $$ = $1; }
		;

empty_grouping_set:
			'(' ')'
				{
					$$ = _.makeGroupingSet(_.GroupingSetKind.GROUPING_SET_EMPTY, null, @1);
				}
		;

/*
 * These hacks rely on setting precedence of CUBE and ROLLUP below that of '(',
 * so that they shift in these rules rather than reducing the conflicting
 * unreserved_keyword rule.
 */

rollup_clause:
			ROLLUP '(' expr_list ')'
				{
					$$ = _.makeGroupingSet(_.GroupingSetKind.GROUPING_SET_ROLLUP, $3, @1);
				}
		;

cube_clause:
			CUBE '(' expr_list ')'
				{
					$$ = _.makeGroupingSet(_.GroupingSetKind.GROUPING_SET_CUBE, $3, @1);
				}
		;

grouping_sets_clause:
			GROUPING SETS '(' group_by_list ')'
				{
					$$ = _.makeGroupingSet(_.GroupingSetKind.GROUPING_SET_SETS, $4, @1);
				}
		;

having_clause:
			HAVING a_expr							{ $$ = $2; }
			| /*EMPTY*/								{ $$ = null; }
		;

for_locking_clause:
			for_locking_items						{ $$ = $1; }
			| FOR READ ONLY							{ $$ = null; }
		;

opt_for_locking_clause:
			for_locking_clause						{ $$ = $1; }
			| /* EMPTY */							{ $$ = null; }
		;

for_locking_items:
			for_locking_item						{ $$ = [$1]; }
			| for_locking_items for_locking_item	{ $$ = $1; $1.push($2); }
		;

for_locking_item:
			for_locking_strength locked_rels_list opt_nowait_or_skip
				{
					$$ = {
						type: _.NodeTag.T_LockingClause,
						lockedRels: $2,
						strength: $1,
						waitPolicy: $3
					};
				}
		;

for_locking_strength:
			FOR UPDATE 							{ $$ = _.LockClauseStrength.LCS_FORUPDATE; }
			| FOR NO KEY UPDATE 				{ $$ = _.LockClauseStrength.LCS_FORNOKEYUPDATE; }
			| FOR SHARE 						{ $$ = _.LockClauseStrength.LCS_FORSHARE; }
			| FOR KEY SHARE 					{ $$ = _.LockClauseStrength.LCS_FORKEYSHARE; }
		;

locked_rels_list:
			OF qualified_name_list					{ $$ = $2; }
			| /* EMPTY */							{ $$ = null; }
		;


/*
 * We should allow ROW '(' expr_list ')' too, but that seems to require
 * making VALUES a fully reserved word, which will probably break more apps
 * than allowing the noise-word is worth.
 */
values_clause:
			VALUES '(' expr_list ')'
				{
					$$ = {
						type: _.NodeTag.T_SelectStmt,
						valuesLists: [$3]
					};
				}
			| values_clause ',' '(' expr_list ')'
				{
					$$ = $1;
					$1.valuesLists = $1.valuesLists.concat($4);
				}
		;


/*****************************************************************************
 *
 *		QUERY:
 *				LOCK TABLE
 *
 *****************************************************************************/

LockStmt:	LOCK_P opt_table relation_expr_list opt_lock opt_nowait
				{
					$$ = {
						type: _.NodeTag.T_LockStmt,
						relations: $3,
						mode: $4,
						nowait: $5
					};
				}
		;

opt_lock:	IN_P lock_type MODE				{ $$ = $2; }
			| /*EMPTY*/						{ $$ = _.AccessExclusiveLock; }
		;

lock_type:	ACCESS SHARE					{ $$ = _.AccessShareLock; }
			| ROW SHARE						{ $$ = _.RowShareLock; }
			| ROW EXCLUSIVE					{ $$ = _.RowExclusiveLock; }
			| SHARE UPDATE EXCLUSIVE		{ $$ = _.ShareUpdateExclusiveLock; }
			| SHARE							{ $$ = _.ShareLock; }
			| SHARE ROW EXCLUSIVE			{ $$ = _.ShareRowExclusiveLock; }
			| EXCLUSIVE						{ $$ = _.ExclusiveLock; }
			| ACCESS EXCLUSIVE				{ $$ = _.AccessExclusiveLock; }
		;

opt_nowait:	NOWAIT							{ $$ = true; }
			| /*EMPTY*/						{ $$ = false; }
		;

opt_nowait_or_skip:
			NOWAIT							{ $$ = _.LockWaitPolicy.LockWaitError; }
			| SKIP LOCKED					{ $$ = _.LockWaitPolicy.LockWaitSkip; }
			| /*EMPTY*/						{ $$ = _.LockWaitPolicy.LockWaitBlock; }
		;


/*****************************************************************************
 *
 *		QUERY:
 *				UpdateStmt (UPDATE)
 *
 *****************************************************************************/

UpdateStmt: opt_with_clause UPDATE relation_expr_opt_alias
			SET set_clause_list
			from_clause
			where_or_current_clause
			returning_clause
				{
					$$ = {
						type: _.NodeTag.T_UpdateStmt,
						relation: $3,
						targetList: $5,
						fromClause: $6,
						whereClause: $7,
						returningList: $8,
						withClause: $1
					};
				}
		;

set_clause_list:
			set_clause							{ $$ = $1; }
			| set_clause_list ',' set_clause	{ $$ = $1.concat($3 || []); }
		;

set_target_list:
			set_target								{ $$ = [$1]; }
			| set_target_list ',' set_target		{ $$ = $1; $1.push($3); }
		;


/*****************************************************************************
 *
 *	clauses common to all Optimizable Stmts:
 *		from_clause		- allow list of both JOIN expressions and table names
 *		where_clause	- qualifications for joins or restrictions
 *
 *****************************************************************************/

from_clause:
			FROM from_list							{ $$ = $2; }
			| /*EMPTY*/								{ $$ = null; }
		;

from_list:
			table_ref								{ $$ = [$1]; }
			| from_list ',' table_ref				{ $$ = $1; $1.push($3); }
		;

/*
 * table_ref is where an alias clause can be attached.
 */
table_ref:	relation_expr opt_alias_clause
				{
					$1.alias = $2;
					$$ = $1;
				}
			| relation_expr opt_alias_clause tablesample_clause
				{
					$1.alias = $2;
					$$ = {
						...$3,
						/* relation_expr goes inside the RangeTableSample node */
						relation: $1
					};
				}
			| func_table func_alias_clause
				{
					$$ = {
						...$1,
						alias: $2[0],
						coldeflist: $2[1]
					};
				}
			| LATERAL_P func_table func_alias_clause
				{
					$$ = {
						...$2,
						lateral: true,
						alias: $3[0],
						coldeflist: $3[1]
					};
				}
			| xmltable opt_alias_clause
				{
					$$ = {
						...$1,
						alias: $2
					};
				}
			| LATERAL_P xmltable opt_alias_clause
				{
					$$ = {
						...$2,
						lateral: true,
						alias: $3,
					};
				}
			| select_with_parens opt_alias_clause
				{
					$$ = {
						type: _.NodeTag.T_RangeSubselect,
						lateral: false,
						subquery: $1,
						alias: $2
					};
					/*
					 * The SQL spec does not permit a subselect
					 * (<derived_table>) without an alias clause,
					 * so we don't either.  This avoids the problem
					 * of needing to invent a unique refname for it.
					 * That could be surmounted if there's sufficient
					 * popular demand, but for now let's just implement
					 * the spec and see if anyone complains.
					 * However, it does seem like a good idea to emit
					 * an error message that's better than "syntax error".
					 */
					if ($2 == null)
					{
						if (_.IsA($1, 'SelectStmt') &&
							$1.valuesLists)
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("VALUES in FROM must have an alias"),
									 errhint("For example, FROM (VALUES ...) [AS] foo."),
									 parser_errposition(@1)));
						else
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("subquery in FROM must have an alias"),
									 errhint("For example, FROM (SELECT ...) [AS] foo."),
									 parser_errposition(@1)));
					}
				}
			| LATERAL_P select_with_parens opt_alias_clause
				{
					$$ = {
						type: _.NodeTag.T_RangeSubselect,
						lateral: true,
						subquery: $2,
						alias: $3,
					};
					/* same comment as above */
					if ($3 == null)
					{
						if (_.IsA($2, 'SelectStmt') &&
							$2.valuesLists)
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("VALUES in FROM must have an alias"),
									 errhint("For example, FROM (VALUES ...) [AS] foo."),
									 parser_errposition(@2)));
						else
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("subquery in FROM must have an alias"),
									 errhint("For example, FROM (SELECT ...) [AS] foo."),
									 parser_errposition(@2)));
					}
				}
			| joined_table
				{
					$$ = $1;
				}
			| '(' joined_table ')' alias_clause
				{
					$2.alias = $4;
					$$ = $2;
				}
		;


/*
 * It may seem silly to separate joined_table from table_ref, but there is
 * method in SQL's madness: if you don't do it this way you get reduce-
 * reduce conflicts, because it's not clear to the parser generator whether
 * to expect alias_clause after ')' or not.  For the same reason we must
 * treat 'JOIN' and 'join_type JOIN' separately, rather than allowing
 * join_type to expand to empty; if we try it, the parser generator can't
 * figure out when to reduce an empty join_type right after table_ref.
 *
 * Note that a CROSS JOIN is the same as an unqualified
 * INNER JOIN, and an INNER JOIN/ON has the same shape
 * but a qualification expression to limit membership.
 * A NATURAL JOIN implicitly matches column names between
 * tables and the shape is determined by which columns are
 * in common. We'll collect columns during the later transformations.
 */

joined_table:
			'(' joined_table ')'
				{
					$$ = $2;
				}
			| table_ref CROSS JOIN table_ref
				{
					/* CROSS JOIN is same as unqualified inner join */
					$$ = {
						type: _.NodeTag.T_JoinExpr,
						jointype: _.JoinType.JOIN_INNER,
						isNatural: false,
						larg: $1,
						rarg: $4,
						usingClause: null,
						quals: null
					};
				}
			| table_ref join_type JOIN table_ref join_qual
				{
					$$ = {
						type: _.NodeTag.T_JoinExpr,
						jointype: $2,
						isNatural: false,
						larg: $1,
						rarg: $4,
					};
					if (Array.isArray($5))
						$$.usingClause = $5; /* USING clause */
					else
						$$.quals = $5; /* ON clause */
				}
			| table_ref JOIN table_ref join_qual
				{
					/* letting join_type reduce to empty doesn't work */
					$$ = {
						type: _.NodeTag.T_JoinExpr,
						jointype: _.JoinType.JOIN_INNER,
						isNatural: false,
						larg: $1,
						rarg: $3,
					};
					if (Array.isArray($4))
						$$.usingClause = $4; /* USING clause */
					else
						$$.quals = $4; /* ON clause */
				}
			| table_ref NATURAL join_type JOIN table_ref
				{
					$$ = {
						type: _.NodeTag.T_JoinExpr,
						jointype: $3,
						isNatural: true,
						larg: $1,
						rarg: $5,
						usingClause: null, /* figure out which columns later... */
						quals: null, /* fill later */
					};
				}
			| table_ref NATURAL JOIN table_ref
				{
					/* letting join_type reduce to empty doesn't work */
					$$ = {
						type: _.NodeTag.T_JoinExpr,
						jointype: _.JoinType.JOIN_INNER,
						isNatural: true,
						larg: $1,
						rarg: $4,
						usingClause: null, /* figure out which columns later... */
						quals: null, /* fill later */
					};
				}
		;

alias_clause:
			AS ColId '(' name_list ')'
				{
					$$ = {
						type: _.NodeTag.T_Alias,
						aliasname: $2,
						colnames: $4
					};
				}
			| AS ColId
				{
					$$ = {
						type: _.NodeTag.T_Alias,
						aliasname: $2
					};
				}
			| ColId '(' name_list ')'
				{
					$$ = {
						type: _.NodeTag.T_Alias,
						aliasname: $1,
						colnames: $3
					};
				}
			| ColId
				{
					$$ = {
						type: _.NodeTag.T_Alias,
						aliasname: $1
					};
				}
		;

opt_alias_clause: alias_clause						{ $$ = $1; }
			| /*EMPTY*/								{ $$ = null; }
		;

/*
 * func_alias_clause can include both an Alias and a coldeflist, so we make it
 * return a 2-element list that gets disassembled by calling production.
 */
func_alias_clause:
			alias_clause
				{
					$$ = [$1, NIL];
				}
			| AS '(' TableFuncElementList ')'
				{
					$$ = [NULL, $3];
				}
			| AS ColId '(' TableFuncElementList ')'
				{
					var a = {
						type: _.NodeTag.T_Alias,
						aliasname: $2
					};
					$$ = [a, $4];
				}
			| ColId '(' TableFuncElementList ')'
				{
					var a = {
						type: _.NodeTag.T_Alias,
						aliasname: $1
					};
					$$ = [a, $3];
				}
			| /*EMPTY*/
				{
					$$ = [NULL, NIL];
				}
		;

join_type:	FULL join_outer							{ $$ = _.JoinType.JOIN_FULL; }
			| LEFT join_outer						{ $$ = _.JoinType.JOIN_LEFT; }
			| RIGHT join_outer						{ $$ = _.JoinType.JOIN_RIGHT; }
			| INNER_P								{ $$ = _.JoinType.JOIN_INNER; }
		;

/* OUTER is just noise... */
join_outer: OUTER_P									{ $$ = null; }
			| /*EMPTY*/								{ $$ = null; }
		;

/* JOIN qualification clauses
 * Possibilities are:
 *	USING ( column list ) allows only unqualified column names,
 *						  which must match between tables.
 *	ON expr allows more general qualifications.
 *
 * We return USING as a List node, while an ON-expr will not be a List.
 */

join_qual:	USING '(' name_list ')'					{ $$ = $3; }
			| ON a_expr								{ $$ = $2; }
		;

relation_expr:
			qualified_name
				{
					/* inheritance query, implicitly */
					$$ = $1;
					$$.inh = true;
					$$.alias = null;
				}
			| qualified_name '*'
				{
					/* inheritance query, explicitly */
					$$ = $1;
					$$.inh = true;
					$$.alias = null;
				}
			| ONLY qualified_name
				{
					/* no inheritance */
					$$ = $2;
					$$.inh = false;
					$$.alias = null;
				}
			| ONLY '(' qualified_name ')'
				{
					/* no inheritance, SQL99-style syntax */
					$$ = $3;
					$$.inh = false;
					$$.alias = null;
				}
		;


relation_expr_list:
			relation_expr							{ $$ = [$1]; }
			| relation_expr_list ',' relation_expr	{ $$ = $1; $1.push($3); }
		;

/*
 * Given "UPDATE foo set set ...", we have to decide without looking any
 * further ahead whether the first "set" is an alias or the UPDATE's SET
 * keyword.  Since "set" is allowed as a column name both interpretations
 * are feasible.  We resolve the shift/reduce conflict by giving the first
 * relation_expr_opt_alias production a higher precedence than the SET token
 * has, causing the parser to prefer to reduce, in effect assuming that the
 * SET is not an alias.
 */
relation_expr_opt_alias: relation_expr					%prec UMINUS
				{
					$$ = $1;
				}
			| relation_expr ColId
				{
					$$ = $1;
					$1.alias = {
						type: _.NodeTag.T_Alias,
						aliasname: $2
					}
				}
			| relation_expr AS ColId
				{
					$$ = $1;
					$1.alias = {
						type: _.NodeTag.T_Alias,
						aliasname: $3
					}
				}
		;

/*
 * TABLESAMPLE decoration in a FROM item
 */
tablesample_clause:
			TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
				{
					/* $$.relation will be filled in later */
					$$ = {
						type: _.NodeTag.T_RangeTableSample,
						method: $2,
						args: $4,
						repeatable: $6,
						location: @2,
					};
				}
		;

opt_repeatable_clause:
			REPEATABLE '(' a_expr ')'	{ $$ = $3; }
			| /*EMPTY*/					{ $$ = NULL; }
		;

/*
 * Window Definitions
 */
window_clause:
			WINDOW window_definition_list			{ $$ = $2; }
			| /*EMPTY*/								{ $$ = null; }
		;

window_definition_list:
			window_definition						{ $$ = [$1]; }
			| window_definition_list ',' window_definition
													{ $$ = $1; $1.push($3); }
		;

window_definition:
			ColId AS window_specification
				{
					$$ = $3;
					$$.name = $1;
				}
		;

over_clause: OVER window_specification
				{ $$ = $2; }
			| OVER ColId
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						name: $2,
						refname: null,
						partitionClause: null,
						orderClause: null,
						frameOptions: _.FRAMEOPTION_DEFAULTS,
						startOffset: null,
						endOffset: null,
						location: @2
					};
				}
			| /*EMPTY*/
				{ $$ = null; }
		;

window_specification: '(' opt_existing_window_name opt_partition_clause
						opt_sort_clause opt_frame_clause ')'
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						name: null,
						refname: $2,
						partitionClause: $3,
						orderClause: $4,
						/* copy relevant fields of opt_frame_clause */
						frameOptions: $5.frameOptions,
						startOffset: $5.startOffset,
						endOffset: $5.endOffset,
						location: @1
					};
				}
		;

/*
 * If we see PARTITION, RANGE, ROWS or GROUPS as the first token after the '('
 * of a window_specification, we want the assumption to be that there is
 * no existing_window_name; but those keywords are unreserved and so could
 * be ColIds.  We fix this by making them have the same precedence as IDENT
 * and giving the empty production here a slightly higher precedence, so
 * that the shift/reduce conflict is resolved in favor of reducing the rule.
 * These keywords are thus precluded from being an existing_window_name but
 * are not reserved for any other purpose.
 */
opt_existing_window_name: ColId						{ $$ = $1; }
			| /*EMPTY*/			/* %prec Op */		{ $$ = null; }
		;

opt_partition_clause: PARTITION BY expr_list		{ $$ = $3; }
			| /*EMPTY*/								{ $$ = null; }
		;

/*
 * For frame clauses, we return a WindowDef, but only some fields are used:
 * frameOptions, startOffset, and endOffset.
 */
opt_frame_clause:
			RANGE frame_extent opt_window_exclusion_clause
				{
					$$ = $2;
					$$.frameOptions |= _.FRAMEOPTION_NONDEFAULT | _.FRAMEOPTION_RANGE;
					$$.frameOptions |= $3;
				}
			| ROWS frame_extent opt_window_exclusion_clause
				{
					$$ = $2;
					$$.frameOptions |= _.FRAMEOPTION_NONDEFAULT | _.FRAMEOPTION_ROWS;
					$$.frameOptions |= $3;
				}
			| GROUPS frame_extent opt_window_exclusion_clause
				{
					$$ = $2;
					$$.frameOptions |= _.FRAMEOPTION_NONDEFAULT | _.FRAMEOPTION_GROUPS;
					$$.frameOptions |= $3;
				}
			| /*EMPTY*/
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						frameOptions: _.FRAMEOPTION_DEFAULTS,
						startOffset: null,
						endOffset: null
					};
				}
		;

frame_extent: frame_bound
				{
					$$ = $1;
					/* reject invalid cases */
					if ($$.frameOptions & _.FRAMEOPTION_START_UNBOUNDED_FOLLOWING)
						ereport(ERROR,
								(errcode(ERRCODE_WINDOWING_ERROR),
								 errmsg("frame start cannot be UNBOUNDED FOLLOWING"),
								 parser_errposition(@1)));
					if ($$.frameOptions & _.FRAMEOPTION_START_OFFSET_FOLLOWING)
						ereport(ERROR,
								(errcode(ERRCODE_WINDOWING_ERROR),
								 errmsg("frame starting from following row cannot end with current row"),
								 parser_errposition(@1)));
					$$.frameOptions |= _.FRAMEOPTION_END_CURRENT_ROW;
				}
			| BETWEEN frame_bound AND frame_bound
				{
					var n1 = $2;
					var n2 = $4;
					/* form merged options */
					var frameOptions = n1.frameOptions;
					/* shift converts START_ options to END_ options */
					frameOptions |= n2.frameOptions << 1;
					frameOptions |= _.FRAMEOPTION_BETWEEN;
					/* reject invalid cases */
					if (frameOptions & _.FRAMEOPTION_START_UNBOUNDED_FOLLOWING)
						ereport(ERROR,
								(errcode(ERRCODE_WINDOWING_ERROR),
								 errmsg("frame start cannot be UNBOUNDED FOLLOWING"),
								 parser_errposition(@2)));
					if (frameOptions & _.FRAMEOPTION_END_UNBOUNDED_PRECEDING)
						ereport(ERROR,
								(errcode(ERRCODE_WINDOWING_ERROR),
								 errmsg("frame end cannot be UNBOUNDED PRECEDING"),
								 parser_errposition(@4)));
					if ((frameOptions & _.FRAMEOPTION_START_CURRENT_ROW) &&
						(frameOptions & _.FRAMEOPTION_END_OFFSET_PRECEDING))
						ereport(ERROR,
								(errcode(ERRCODE_WINDOWING_ERROR),
								 errmsg("frame starting from current row cannot have preceding rows"),
								 parser_errposition(@4)));
					if ((frameOptions & _.FRAMEOPTION_START_OFFSET_FOLLOWING) &&
						(frameOptions & (_.FRAMEOPTION_END_OFFSET_PRECEDING |
										 _.FRAMEOPTION_END_CURRENT_ROW)))
						ereport(ERROR,
								(errcode(ERRCODE_WINDOWING_ERROR),
								 errmsg("frame starting from following row cannot have preceding rows"),
								 parser_errposition(@4)));
					n1.frameOptions = frameOptions;
					n1.endOffset = n2.startOffset;
					$$ = n1;
				}
		;

/*
 * This is used for both frame start and frame end, with output set up on
 * the assumption it's frame start; the frame_extent productions must reject
 * invalid cases.
 */
frame_bound:
			UNBOUNDED PRECEDING
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						frameOptions: _.FRAMEOPTION_START_UNBOUNDED_PRECEDING,
						startOffset: null,
						endOffset: null
					};
				}
			| UNBOUNDED FOLLOWING
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						frameOptions: _.FRAMEOPTION_START_UNBOUNDED_FOLLOWING,
						startOffset: null,
						endOffset: null
					};
				}
			| CURRENT_P ROW
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						frameOptions: _.FRAMEOPTION_START_CURRENT_ROW,
						startOffset: null,
						endOffset: null
					};
				}
			| a_expr PRECEDING
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						frameOptions: _.FRAMEOPTION_START_OFFSET_PRECEDING,
						startOffset: $1,
						endOffset: null
					};
				}
			| a_expr FOLLOWING
				{
					$$ = {
						type: _.NodeTag.T_WindowDef,
						frameOptions: _.FRAMEOPTION_START_OFFSET_FOLLOWING,
						startOffset: $1,
						endOffset: null
					};
				}
		;

opt_window_exclusion_clause:
			EXCLUDE CURRENT_P ROW	{ $$ = _.FRAMEOPTION_EXCLUDE_CURRENT_ROW; }
			| EXCLUDE GROUP_P		{ $$ = _.FRAMEOPTION_EXCLUDE_GROUP; }
			| EXCLUDE TIES			{ $$ = _.FRAMEOPTION_EXCLUDE_TIES; }
			| EXCLUDE NO OTHERS		{ $$ = 0; }
			| /*EMPTY*/				{ $$ = 0; }
		;

/*
 * TABLESAMPLE decoration in a FROM item
 */
tablesample_clause:
			TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
				{
					$$ = {
						type: _.NodeTag.T_RangeTableSample,
						/* n.relation will be filled in later */
						method: $2,
						args: $4,
						repeatable: $6,
						location: @2
					};
				}
		;

opt_repeatable_clause:
			REPEATABLE '(' a_expr ')'	{ $$ = $3; }
			| /*EMPTY*/					{ $$ = null; }
		;

/*
 * func_table represents a function invocation in a FROM list. It can be
 * a plain function call, like "foo(...)", or a ROWS FROM expression with
 * one or more function calls, "ROWS FROM (foo(...), bar(...))",
 * optionally with WITH ORDINALITY attached.
 * In the ROWS FROM syntax, a column definition list can be given for each
 * function, for example:
 *     ROWS FROM (foo() AS (foo_res_a text, foo_res_b text),
 *                bar() AS (bar_res_a text, bar_res_b text))
 * It's also possible to attach a column definition list to the RangeFunction
 * as a whole, but that's handled by the table_ref production.
 */
func_table: func_expr_windowless opt_ordinality
				{
					/* alias and coldeflist are set by table_ref production */
					$$ = {
						type: _.NodeTag.T_RangeFunction,
						lateral: false,
						ordinality: $2,
						is_rowsfrom: false,
						functions: [[$1, NIL]],
					};
				}
			| ROWS FROM '(' rowsfrom_list ')' opt_ordinality
				{
					/* alias and coldeflist are set by table_ref production */
					$$ = {
						type: _.NodeTag.T_RangeFunction,
						lateral: false,
						ordinality: $6,
						is_rowsfrom: true,
						functions: $4,
					}
				}
		;

rowsfrom_item: func_expr_windowless opt_col_def_list
				{ $$ = [$1, $2]; }
		;

rowsfrom_list:
			rowsfrom_item						{ $$ = [$1]; }
			| rowsfrom_list ',' rowsfrom_item	{ $$ = $1; $1.push($3); }
		;

opt_col_def_list: AS '(' TableFuncElementList ')'	{ $$ = $3; }
			| /*EMPTY*/								{ $$ = NIL; }
		;

opt_ordinality: WITH_LA ORDINALITY					{ $$ = true; }
			| /*EMPTY*/								{ $$ = false; }
		;

where_clause:
			WHERE a_expr							{ $$ = $2; }
			| /*EMPTY*/								{ $$ = null; }
		;

/* variant for UPDATE and DELETE */
where_or_current_clause:
			WHERE a_expr							{ $$ = $2; }
			| WHERE CURRENT_P OF cursor_name
				{
					/* cvarno is filled in by parse analysis */
					$$ = {
						type: _.NodeTag.T_CurrentOfExpr,
						cursor_name: $4,
						cursor_param: 0
					};
				}
			| /*EMPTY*/								{ $$ = NULL; }
		;


OptTableFuncElementList:
			TableFuncElementList				{ $$ = $1; }
			| /*EMPTY*/							{ $$ = NIL; }
		;

TableFuncElementList:
			TableFuncElement
				{
					$$ = [$1];
				}
			| TableFuncElementList ',' TableFuncElement
				{
					$$ = $1; $1.push($3);
				}
		;

TableFuncElement:	ColId Typename opt_collate_clause
				{
					$$ = {
						type: _.NodeTag.T_ColumnDef,
						colname: $1,
						typeName: $2,
						inhcount: 0,
						is_local: true,
						is_not_null: false,
						is_from_type: false,
						storage: 0,
						raw_default: NULL,
						cooked_default: NULL,
						collClause: /* (CollateClause *) */ $3,
						collOid: -1,
						constraints: NIL,
						location: @1
					}
				}
		;


/*****************************************************************************
 *
 *	Type syntax
 *		SQL introduces a large amount of type-specific syntax.
 *		Define individual clauses to handle these cases, and use
 *		 the generic case to handle regular type-extensible Postgres syntax.
 *		- thomas 1997-10-10
 *
 *****************************************************************************/

Typename:	SimpleTypename opt_array_bounds
				{
					$$ = $1;
					$$.arrayBounds = $2;
				}
			| SETOF SimpleTypename opt_array_bounds
				{
					$$ = $2;
					$$.arrayBounds = $3;
					$$.setof = true;
				}
			/* SQL standard syntax, currently only one-dimensional */
			| SimpleTypename ARRAY '[' Iconst ']'
				{
					$$ = $1;
					$$.arrayBounds = [_.makeInteger($4)];
				}
			| SETOF SimpleTypename ARRAY '[' Iconst ']'
				{
					$$ = $2;
					$$.arrayBounds = [_.makeInteger($5)];
					$$.setof = true;
				}
			| SimpleTypename ARRAY
				{
					$$ = $1;
					$$.arrayBounds = [_.makeInteger(-1)];
				}
			| SETOF SimpleTypename ARRAY
				{
					$$ = $2;
					$$.arrayBounds = [_.makeInteger(-1)];
					$$.setof = true;
				}
		;

opt_array_bounds:
			opt_array_bounds '[' ']'
					{  $$ = $1; $1.push(_.makeInteger(-1)); }
			| opt_array_bounds '[' Iconst ']'
					{  $$ = $1; $1.push(_.makeInteger($3)); }
			| /*EMPTY*/
					{  $$ = NIL; }
		;

SimpleTypename:
			GenericType								{ $$ = $1; }
			| Numeric								{ $$ = $1; }
			| Bit									{ $$ = $1; }
			| Character								{ $$ = $1; }
			| ConstDatetime							{ $$ = $1; }
			| ConstInterval opt_interval
				{
					$$ = $1;
					$$.typmods = $2;
				}
			| ConstInterval '(' Iconst ')'
				{
					$$ = $1;
					$$.typmods = [_.makeIntConst(_.INTERVAL_FULL_RANGE, -1),
											 _.makeIntConst($3, @3)];
				}
		;

/* We have a separate ConstTypename to allow defaulting fixed-length
 * types such as CHAR() and BIT() to an unspecified length.
 * SQL9x requires that these default to a length of one, but this
 * makes no sense for constructs like CHAR 'hi' and BIT '0101',
 * where there is an obvious better choice to make.
 * Note that ConstInterval is not included here since it must
 * be pushed up higher in the rules to accommodate the postfix
 * options (e.g. INTERVAL '1' YEAR). Likewise, we have to handle
 * the generic-type-name case in AexprConst to avoid premature
 * reduce/reduce conflicts against function names.
 */
ConstTypename:
			Numeric									{ $$ = $1; }
			| ConstBit								{ $$ = $1; }
			| ConstCharacter						{ $$ = $1; }
			| ConstDatetime							{ $$ = $1; }
		;

/*
 * GenericType covers all type names that don't have special syntax mandated
 * by the standard, including qualified names.  We also allow type modifiers.
 * To avoid parsing conflicts against function invocations, the modifiers
 * have to be shown as expr_list here, but parse analysis will only accept
 * constants for them.
 */
GenericType:
			type_function_name opt_type_modifiers
				{
					$$ = _.makeTypeName($1);
					$$.typmods = $2;
					$$.location = @1;
				}
			| type_function_name attrs opt_type_modifiers
				{
					$2.unshift(_.makeString($1))
					$$ = _.makeTypeNameFromNameList($2);
					$$.typmods = $3;
					$$.location = @1;
				}
		;

opt_type_modifiers: '(' expr_list ')'				{ $$ = $2; }
					| /* EMPTY */					{ $$ = NIL; }
		;

columnref:	ColId
				{
					$$ = _.makeColumnRef($1, null, @1, null /* yyscanner */);
				}
			| ColId indirection
				{
					$$ = _.makeColumnRef($1, $2, @1, null /* yyscanner */);
				}
		;

indirection_el:
			'.' attr_name
				{
					$$ = _.makeString($2);
				}
			| '.' '*'
				{
					$$ = _.makeNode('A_Star');
				}
			| '[' a_expr ']'
				{
					var ai = _.makeNode('A_Indices');
					ai.is_slice = false;
					ai.lidx = null;
					ai.uidx = $2;
					$$ = ai;
				}
			| '[' opt_slice_bound ':' opt_slice_bound ']'
				{
					var ai = _.makeNode('A_Indices');
					ai.is_slice = true;
					ai.lidx = $2;
					ai.uidx = $4;
					$$ = ai;
				}
		;

opt_slice_bound:
			a_expr									{ $$ = $1; }
			| /*EMPTY*/								{ $$ = null; }
		;

indirection:
			indirection_el							{ $$ = [$1]; }
			| indirection indirection_el			{ $1.push($2); $$ = $1; }
		;

opt_indirection:
			/*EMPTY*/								{ $$ = null; }
			| opt_indirection indirection_el		{ $$ = ($1 || []).push($2); }
		;

opt_asymmetric: ASYMMETRIC
			| /*EMPTY*/
		;


/* Column identifier --- names that can be column, table, etc names.
 */
ColId:		IDENT									{ $$ = $1; }
			| unreserved_keyword					{ $$ = $1; }
			| col_name_keyword						{ $$ = $1; }
		;

/* Type/function identifier --- names that can be type or function names.
 */
type_function_name:	IDENT							{ $$ = $1; }
			| unreserved_keyword					{ $$ = $1; }
			| type_func_name_keyword				{ $$ = $1; }
		;

/* Column label --- allowed labels in "AS" clauses.
 * This presently includes *all* Postgres keywords.
 */
ColLabel:	IDENT									{ $$ = $1; }
			| unreserved_keyword					{ $$ = $1; }
			| col_name_keyword						{ $$ = $1; }
			| type_func_name_keyword				{ $$ = $1; }
			| reserved_keyword						{ $$ = $1; }
		;


/* function arguments can have names */
func_arg_list:  func_arg_expr
				{
					$$ = [$1];
				}
			| func_arg_list ',' func_arg_expr
				{
					$1.push($3);
					$$ = $1;
				}
		;

/*
 * The production for a qualified func_name has to exactly match the
 * production for a qualified columnref, because we cannot tell which we
 * are parsing until we see what comes after it ('(' or Sconst for a func_name,
 * anything else for a columnref).  Therefore we allow 'indirection' which
 * may contain subscripts, and reject that case in the C code.  (If we
 * ever implement SQL99-like methods, such syntax may actually become legal!)
 */
func_name:	type_function_name
					{ $$ = [_.makeString($1)]; }
			| ColId indirection
					{
						$2.unshift(_.makeString($1));
						$$ = check_func_name($2, yyscanner);
					}
		;


/*
 * Constants
 */
AexprConst: Iconst
				{
					$$ = _.makeIntConst($1, @1);
				}
			| FCONST
				{
					$$ = _.makeFloatConst($1, @1);
				}
			| Sconst
				{
					$$ = _.makeStringConst($1, @1);
				}
			| BCONST
				{
					$$ = _.makeBitStringConst($1, @1);
				}
			| XCONST
				{
					/* This is a bit constant per SQL99:
					 * Without Feature F511, "BIT data type",
					 * a <general literal> shall not be a
					 * <bit string literal> or a <hex string literal>.
					 */
					$$ = _.makeBitStringConst($1, @1);
				}
			| func_name Sconst
				{
					/* generic type 'literal' syntax */
					var t = _.makeTypeNameFromNameList($1);
					t.location = @1;
					$$ = _.makeStringConstCast($2, @2, t);
				}
			| ConstTypename Sconst
				{
					$$ = _.makeStringConstCast($2, @2, $1);
				}
			| ConstInterval Sconst opt_interval
				{
					var t = $1;
					t.typmods = $3;
					$$ = _.makeStringConstCast($2, @2, t);
				}
			| ConstInterval '(' Iconst ')' Sconst
				{
					var t = $1;
					t.typmods = [_.makeIntConst(INTERVAL_FULL_RANGE, -1),
											makeIntConst($3, @3)];
					$$ = _.makeStringConstCast($5, @5, t);
				}
			| TRUE_P
				{
					$$ = _.makeBoolAConst(true, @1);
				}
			| FALSE_P
				{
					$$ = _.makeBoolAConst(false, @1);
				}
			| NULL_P
				{
					$$ = _.makeNullAConst(@1);
				}
		;

Iconst:		ICONST									{ $$ = $1; };
Sconst:		SCONST									{ $$ = $1; };

SignedIconst: Iconst								{ $$ = $1; }
			| '+' Iconst							{ $$ = + $1; }
			| '-' Iconst							{ $$ = - $1; }
		;


all_Op:		Op										{ $$ = $1; }
			| MathOp								{ $$ = $1; }
		;

MathOp:		 '+'									{ $$ = "+"; }
			| '-'									{ $$ = "-"; }
			| '*'									{ $$ = "*"; }
			| '/'									{ $$ = "/"; }
			| '%'									{ $$ = "%"; }
			| '^'									{ $$ = "^"; }
			| '<'									{ $$ = "<"; }
			| '>'									{ $$ = ">"; }
			| '='									{ $$ = "="; }
			| LESS_EQUALS							{ $$ = "<="; }
			| GREATER_EQUALS						{ $$ = ">="; }
			| NOT_EQUALS							{ $$ = "<>"; }
		;

qual_Op:	Op
					{ $$ = [_.makeString($1)]; }
			| OPERATOR '(' any_operator ')'
					{ $$ = $3; }
		;

qual_all_Op:
			all_Op
					{ $$ = [_.makeString($1)]; }
			| OPERATOR '(' any_operator ')'
					{ $$ = $3; }
		;

subquery_Op:
			all_Op
					{ $$ = [_.makeString($1)]; }
			| OPERATOR '(' any_operator ')'
					{ $$ = $3; }
			| LIKE
					{ $$ = [_.makeString("~~")]; }
			| NOT_LA LIKE
					{ $$ = [_.makeString("!~~")]; }
			| ILIKE
					{ $$ = [_.makeString("~~*")]; }
			| NOT_LA ILIKE
					{ $$ = [_.makeString("!~~*")]; }
/* cannot put SIMILAR TO here, because SIMILAR TO is a hack.
 * the regular expression is preprocessed by a function (similar_to_escape),
 * and the ~ operator for posix regular expressions is used.
 *        x SIMILAR TO y     ->    x ~ similar_to_escape(y)
 * this transformation is made on the fly by the parser upwards.
 * however the SubLink structure which handles any/some/all stuff
 * is not ready for such a thing.
 */
			;

expr_list:	a_expr
				{
					$$ = [$1];
				}
			| expr_list ',' a_expr
				{
					$1.push($3);
					$$ = $1;
				}
		;

/*
 * Productions that can be used in both a_expr and b_expr.
 *
 * Note: productions that refer recursively to a_expr or b_expr mostly
 * cannot appear here.	However, it's OK to refer to a_exprs that occur
 * inside parentheses, such as function arguments; that cannot introduce
 * ambiguity to the b_expr syntax.
 */
c_expr:		columnref								{ $$ = $1; }
			| AexprConst							{ $$ = $1; }
			| PARAM opt_indirection
				{
					var p = {
						type: _.NodeTag.T_ParamRef,
						number: $1,
						location: @1
					};

					if ($2)
					{
						$$ = {
							type: _.NodeTag.T_A_Indirection,
							arg: p,
							indirection: _.check_indirection($2, yyscanner),
						};
					}
					else
						$$ = p;
				}
			| '(' a_expr ')' opt_indirection
				{
					if ($4)
					{
						$$ = {
							type: _.NodeTag.T_A_Indirection,
							arg: $2,
							indirection: _.check_indirection($4, yyscanner),
						};
					}
					else if (operator_precedence_warning)
					{
						/*
						 * If precedence warnings are enabled, insert
						 * AEXPR_PAREN nodes wrapping all explicitly
						 * parenthesized subexpressions; this prevents bogus
						 * warnings from being issued when the ordering has
						 * been forced by parentheses.  Take care that an
						 * AEXPR_PAREN node has the same exprLocation as its
						 * child, so as not to cause surprising changes in
						 * error cursor positioning.
						 *
						 * In principle we should not be relying on a GUC to
						 * decide whether to insert AEXPR_PAREN nodes.
						 * However, since they have no effect except to
						 * suppress warnings, it's probably safe enough; and
						 * we'd just as soon not waste cycles on dummy parse
						 * nodes if we don't have to.
						 */
						$$ = _.makeA_Expr(_.A_Expr_Kind.AEXPR_PAREN, NIL, $2, NULL,
												 _.exprLocation($2));
					}
					else
						$$ = $2;
				}
			| case_expr
				{ $$ = $1; }
			| func_expr
				{ $$ = $1; }
			| select_with_parens			%prec UMINUS
				{
					$$ = {
						type: _.NodeTag.SubLink,
						subLinkType: _.SubLinkType.EXPR_SUBLINK,
						subLinkId: 0,
						testexpr: NULL,
						operName: NIL,
						subselect: $1,
						location: @1
					};
				}
			| select_with_parens indirection
				{
					/*
					 * Because the select_with_parens nonterminal is designed
					 * to "eat" as many levels of parens as possible, the
					 * '(' a_expr ')' opt_indirection production above will
					 * fail to match a sub-SELECT with indirection decoration;
					 * the sub-SELECT won't be regarded as an a_expr as long
					 * as there are parens around it.  To support applying
					 * subscripting or field selection to a sub-SELECT result,
					 * we need this redundant-looking production.
					 */
					var n = {
						type: _.NodeTag.SubLink,
						subLinkType: _.SubLinkType.EXPR_SUBLINK,
						subLinkId: 0,
						testexpr: NULL,
						operName: NIL,
						subselect: $1,
						location: @1,
					};

					$$ = {
						type: _.NodeTag.A_Indirection,
						arg: n,
						indirection: _.check_indirection($2, yyscanner),
					};
				}
			| EXISTS select_with_parens
				{
					$$ = {
						type: _.NodeTag.T_SubLink,
						subLinkType: _.SubLinkType.EXISTS_SUBLINK,
						subLinkId: 0,
						testexpr: NULL,
						operName: NIL,
						subselect: $2,
						location: @1,
					};
				}
			| ARRAY select_with_parens
				{
					$$ = {
						type: _.NodeTag.T_SubLink,
						subLinkType: ARRAY_SUBLINK,
						subLinkId: 0,
						testexpr: NULL,
						operName: NIL,
						subselect: $2,
						location: @1,
					};
				}
			| ARRAY array_expr
				{
					$$ = $2;
					/* point outermost A_ArrayExpr to the ARRAY keyword */
					$$.location = @1;
				}
			| explicit_row
				{
					$$ = {
						type: _.NodeTag.T_RowExpr,
						args: $1,
						row_typeid: -1,	/* not analyzed yet */
						colnames: NIL,	/* to be filled in during analysis */
						row_format: _.CoercionForm.COERCE_EXPLICIT_CALL, /* abuse */
						location: @1,
					};
				}
			| implicit_row
				{
					$$ = {
						type: _.NodeTag.T_RowExpr,
						args: $1,
						row_typeid: -1,	/* not analyzed yet */
						colnames: NIL,	/* to be filled in during analysis */
						row_format: _.CoercionForm.COERCE_IMPLICIT_CAST, /* abuse */
						location: @1
					};
				}
			| GROUPING '(' expr_list ')'
			  	{
					$$ = {
					  	type: _.NodeTag.T_GroupingFunc,
						args: $3,
						location: @1
					};
			  	}
		;

func_application: func_name '(' ')'
				{
					$$ = _.makeFuncCall($1, NIL, @1);
				}
			| func_name '(' func_arg_list opt_sort_clause ')'
				{
					$$ = _.makeFuncCall($1, $3, @1);
					$$.agg_order = $4;
				}
			| func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
				{
					$$ = _.makeFuncCall($1, [$4], @1);
					$$.func_variadic = true;
					$$.agg_order = $5;
				}
			| func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
				{
					$$ = _.makeFuncCall($1, [...$3, $6], @1);
					$$.func_variadic = true;
					$$.agg_order = $7;
				}
			| func_name '(' ALL func_arg_list opt_sort_clause ')'
				{
					/* Ideally we'd mark the FuncCall node to indicate
					 * "must be an aggregate", but there's no provision
					 * for that in FuncCall at the moment.
					 */
					$$ = _.makeFuncCall($1, $4, @1);
					$$.agg_order = $5;
				}
			| func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
				{
					$$ = _.makeFuncCall($1, $4, @1);
					$$.agg_order = $5;
					$$.agg_distinct = true;
				}
			| func_name '(' '*' ')'
				{
					/*
					 * We consider AGGREGATE(*) to invoke a parameterless
					 * aggregate.  This does the right thing for COUNT(*),
					 * and there are no other aggregates in SQL that accept
					 * '*' as parameter.
					 *
					 * The FuncCall node is also marked agg_star = true,
					 * so that later processing can detect what the argument
					 * really was.
					 */
					$$ = _.makeFuncCall($1, NIL, @1);
					$$.agg_star = true;
				}
		;


/*
 * func_expr and its cousin func_expr_windowless are split out from c_expr just
 * so that we have classifications for "everything that is a function call or
 * looks like one".  This isn't very important, but it saves us having to
 * document which variants are legal in places like "FROM function()" or the
 * backwards-compatible functional-index syntax for CREATE INDEX.
 * (Note that many of the special SQL functions wouldn't actually make any
 * sense as functional index entries, but we ignore that consideration here.)
 */
func_expr: func_application within_group_clause filter_clause over_clause
				{
					/** @type {FuncCall} */
					var n = $1;

					/*
					 * The order clause for WITHIN GROUP and the one for
					 * plain-aggregate ORDER BY share a field, so we have to
					 * check here that at most one is present.  We also check
					 * for DISTINCT and VARIADIC here to give a better error
					 * location.  Other consistency checks are deferred to
					 * parse analysis.
					 */
					if ($2 != NIL)
					{
						if (n.agg_order != NIL)
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("cannot use multiple ORDER BY clauses with WITHIN GROUP"),
									 parser_errposition(@2)));
						if (n.agg_distinct)
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("cannot use DISTINCT with WITHIN GROUP"),
									 parser_errposition(@2)));
						if (n.func_variadic)
							ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("cannot use VARIADIC with WITHIN GROUP"),
									 parser_errposition(@2)));
						n.agg_order = $2;
						n.agg_within_group = true;
					}
					n.agg_filter = $3;
					n.over = $4;
					$$ = n;
				}
			| func_expr_common_subexpr
				{ $$ = $1; }
		;

/*
 * As func_expr but does not accept WINDOW functions directly
 * (but they can still be contained in arguments for functions etc).
 * Use this when window expressions are not allowed, where needed to
 * disambiguate the grammar (e.g. in CREATE INDEX).
 */
func_expr_windowless:
			func_application						{ $$ = $1; }
			| func_expr_common_subexpr				{ $$ = $1; }
		;

/*
 * Special expressions that are considered to be functions.
 */
func_expr_common_subexpr:
			COLLATION FOR '(' a_expr ')'
				{
					$$ = _.makeFuncCall(_.SystemFuncName("pg_collation_for"),
											   [$4],
											   @1);
				}
			| CURRENT_DATE
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_DATE, -1, @1);
				}
			| CURRENT_TIME
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_TIME, -1, @1);
				}
			| CURRENT_TIME '(' Iconst ')'
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_TIME_N, $3, @1);
				}
			| CURRENT_TIMESTAMP
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_TIMESTAMP, -1, @1);
				}
			| CURRENT_TIMESTAMP '(' Iconst ')'
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_TIMESTAMP_N, $3, @1);
				}
			| LOCALTIME
				{
					$$ = _.makeSQLValueFunction(SVFOP_LOCALTIME, -1, @1);
				}
			| LOCALTIME '(' Iconst ')'
				{
					$$ = _.makeSQLValueFunction(SVFOP_LOCALTIME_N, $3, @1);
				}
			| LOCALTIMESTAMP
				{
					$$ = _.makeSQLValueFunction(SVFOP_LOCALTIMESTAMP, -1, @1);
				}
			| LOCALTIMESTAMP '(' Iconst ')'
				{
					$$ = _.makeSQLValueFunction(SVFOP_LOCALTIMESTAMP_N, $3, @1);
				}
			| CURRENT_ROLE
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_ROLE, -1, @1);
				}
			| CURRENT_USER
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_USER, -1, @1);
				}
			| SESSION_USER
				{
					$$ = _.makeSQLValueFunction(SVFOP_SESSION_USER, -1, @1);
				}
			| USER
				{
					$$ = _.makeSQLValueFunction(SVFOP_USER, -1, @1);
				}
			| CURRENT_CATALOG
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_CATALOG, -1, @1);
				}
			| CURRENT_SCHEMA
				{
					$$ = _.makeSQLValueFunction(SVFOP_CURRENT_SCHEMA, -1, @1);
				}
			| CAST '(' a_expr AS Typename ')'
				{ $$ = makeTypeCast($3, $5, @1); }
			| EXTRACT '(' extract_list ')'
				{
					$$ = _.makeFuncCall(_.SystemFuncName("date_part"), $3, @1);
				}
			| OVERLAY '(' overlay_list ')'
				{
					/* overlay(A PLACING B FROM C FOR D) is converted to
					 * overlay(A, B, C, D)
					 * overlay(A PLACING B FROM C) is converted to
					 * overlay(A, B, C)
					 */
					$$ = _.makeFuncCall(_.SystemFuncName("overlay"), $3, @1);
				}
			| POSITION '(' position_list ')'
				{
					/* position(A in B) is converted to position(B, A) */
					$$ = _.makeFuncCall(_.SystemFuncName("position"), $3, @1);
				}
			| SUBSTRING '(' substr_list ')'
				{
					/* substring(A from B for C) is converted to
					 * substring(A, B, C) - thomas 2000-11-28
					 */
					$$ = _.makeFuncCall(_.SystemFuncName("substring"), $3, @1);
				}
			| TREAT '(' a_expr AS Typename ')'
				{
					/* TREAT(expr AS target) converts expr of a particular type to target,
					 * which is defined to be a subtype of the original expression.
					 * In SQL99, this is intended for use with structured UDTs,
					 * but let's make this a generally useful form allowing stronger
					 * coercions than are handled by implicit casting.
					 *
					 * Convert SystemTypeName() to SystemFuncName() even though
					 * at the moment they result in the same thing.
					 */
					$$ = _.makeFuncCall(_.SystemFuncName($5.names[$5.names.length - 1].val.str),
												[$3],
												@1);
				}
			| TRIM '(' BOTH trim_list ')'
				{
					/* various trim expressions are defined in SQL
					 * - thomas 1997-07-19
					 */
					$$ = _.makeFuncCall(_.SystemFuncName("btrim"), $4, @1);
				}
			| TRIM '(' LEADING trim_list ')'
				{
					$$ = _.makeFuncCall(_.SystemFuncName("ltrim"), $4, @1);
				}
			| TRIM '(' TRAILING trim_list ')'
				{
					$$ = _.makeFuncCall(_.SystemFuncName("rtrim"), $4, @1);
				}
			| TRIM '(' trim_list ')'
				{
					$$ = _.makeFuncCall(_.SystemFuncName("btrim"), $3, @1);
				}
			| NULLIF '(' a_expr ',' a_expr ')'
				{
					$$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_NULLIF, "=", $3, $5, @1);
				}
			| COALESCE '(' expr_list ')'
				{
					$$ = {
						type: _.NodeTag.T_CoalesceExpr,
						args: $3,
						location: @1
					};
				}
			| GREATEST '(' expr_list ')'
				{
					$$ = {
						type: _.NodeTag.T_MinMaxExpr,
						args: $3,
						op: _.MinMaxOp.IS_GREATEST,
						location: @1,
					}
				}
			| LEAST '(' expr_list ')'
				{
					$$ = {
						type: _.NodeTag.MinMaxExpr,
						args: $3,
						op: _.MinMaxOp.IS_LEAST,
						location: @1
					};
				}
			// | XMLCONCAT '(' expr_list ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLCONCAT, NULL, NIL, $3, @1);
			// 	}
			// | XMLELEMENT '(' NAME_P ColLabel ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLELEMENT, $4, NIL, NIL, @1);
			// 	}
			// | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLELEMENT, $4, $6, NIL, @1);
			// 	}
			// | XMLELEMENT '(' NAME_P ColLabel ',' expr_list ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLELEMENT, $4, NIL, $6, @1);
			// 	}
			// | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ',' expr_list ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLELEMENT, $4, $6, $8, @1);
			// 	}
			// | XMLEXISTS '(' c_expr xmlexists_argument ')'
			// 	{
			// 		/* xmlexists(A PASSING [BY REF] B [BY REF]) is
			// 		 * converted to xmlexists(A, B)*/
			// 		$$ = _.makeFuncCall(_.SystemFuncName("xmlexists"), [$3, $4], @1);
			// 	}
			// | XMLFOREST '(' xml_attribute_list ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLFOREST, NULL, $3, NIL, @1);
			// 	}
			// | XMLPARSE '(' document_or_content a_expr xml_whitespace_option ')'
			// 	{
			// 		XmlExpr *x = (XmlExpr *)
			// 			makeXmlExpr(IS_XMLPARSE, NULL, NIL,
			// 						list_make2($4, makeBoolAConst($5, -1)),
			// 						@1);
			// 		x.xmloption = $3;
			// 		$$ = (Node *)x;
			// 	}
			// | XMLPI '(' NAME_P ColLabel ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLPI, $4, NULL, NIL, @1);
			// 	}
			// | XMLPI '(' NAME_P ColLabel ',' a_expr ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLPI, $4, NULL, [$6], @1);
			// 	}
			// | XMLROOT '(' a_expr ',' xml_root_version opt_xml_root_standalone ')'
			// 	{
			// 		$$ = makeXmlExpr(IS_XMLROOT, NULL, NIL,
			// 						 list_make3($3, $5, $6), @1);
			// 	}
			// | XMLSERIALIZE '(' document_or_content a_expr AS SimpleTypename ')'
			// 	{
			// 		XmlSerialize *n = makeNode(XmlSerialize);
			// 		n.xmloption = $3;
			// 		n.expr = $4;
			// 		n.typeName = $6;
			// 		n.location = @1;
			// 		$$ = (Node *)n;
			// 	}
		;

/*
 * SQL/XML support
 */
// xml_root_version: VERSION_P a_expr
// 				{ $$ = $2; }
// 			| VERSION_P NO VALUE_P
// 				{ $$ = makeNullAConst(-1); }
// 		;

// opt_xml_root_standalone: ',' STANDALONE_P YES_P
// 				{ $$ = makeIntConst(XML_STANDALONE_YES, -1); }
// 			| ',' STANDALONE_P NO
// 				{ $$ = makeIntConst(XML_STANDALONE_NO, -1); }
// 			| ',' STANDALONE_P NO VALUE_P
// 				{ $$ = makeIntConst(XML_STANDALONE_NO_VALUE, -1); }
// 			| /*EMPTY*/
// 				{ $$ = makeIntConst(XML_STANDALONE_OMITTED, -1); }
// 		;

// xml_attributes: XMLATTRIBUTES '(' xml_attribute_list ')'	{ $$ = $3; }
// 		;

// xml_attribute_list:	xml_attribute_el					{ $$ = [$1]; }
// 			| xml_attribute_list ',' xml_attribute_el	{ $$ = lappend($1, $3); }
// 		;

// xml_attribute_el: a_expr AS ColLabel
// 				{
// 					$$ = makeNode(ResTarget);
// 					$$.name = $3;
// 					$$.indirection = NIL;
// 					$$.val = (Node *) $1;
// 					$$.location = @1;
// 				}
// 			| a_expr
// 				{
// 					$$ = makeNode(ResTarget);
// 					$$.name = NULL;
// 					$$.indirection = NIL;
// 					$$.val = (Node *) $1;
// 					$$.location = @1;
// 				}
// 		;

// document_or_content: DOCUMENT_P						{ $$ = XMLOPTION_DOCUMENT; }
// 			| CONTENT_P								{ $$ = XMLOPTION_CONTENT; }
// 		;

// xml_whitespace_option: PRESERVE WHITESPACE_P		{ $$ = true; }
// 			| STRIP_P WHITESPACE_P					{ $$ = false; }
// 			| /*EMPTY*/								{ $$ = false; }
// 		;

// /* We allow several variants for SQL and other compatibility. */
// xmlexists_argument:
// 			PASSING c_expr
// 				{
// 					$$ = $2;
// 				}
// 			| PASSING c_expr xml_passing_mech
// 				{
// 					$$ = $2;
// 				}
// 			| PASSING xml_passing_mech c_expr
// 				{
// 					$$ = $3;
// 				}
// 			| PASSING xml_passing_mech c_expr xml_passing_mech
// 				{
// 					$$ = $3;
// 				}
// 		;

// xml_passing_mech:
// 			BY REF
// 			| BY VALUE_P
// 		;


/*
 * SQL numeric data types
 */
Numeric:	INT_P
				{
					$$ = _.SystemTypeName("int4");
					$$.location = @1;
				}
			| INTEGER
				{
					$$ = _.SystemTypeName("int4");
					$$.location = @1;
				}
			| SMALLINT
				{
					$$ = _.SystemTypeName("int2");
					$$.location = @1;
				}
			| BIGINT
				{
					$$ = _.SystemTypeName("int8");
					$$.location = @1;
				}
			| REAL
				{
					$$ = _.SystemTypeName("float4");
					$$.location = @1;
				}
			| FLOAT_P opt_float
				{
					$$ = $2;
					$$.location = @1;
				}
			| DOUBLE_P PRECISION
				{
					$$ = _.SystemTypeName("float8");
					$$.location = @1;
				}
			| DECIMAL_P opt_type_modifiers
				{
					$$ = _.SystemTypeName("numeric");
					$$.typmods = $2;
					$$.location = @1;
				}
			| DEC opt_type_modifiers
				{
					$$ = _.SystemTypeName("numeric");
					$$.typmods = $2;
					$$.location = @1;
				}
			| NUMERIC opt_type_modifiers
				{
					$$ = _.SystemTypeName("numeric");
					$$.typmods = $2;
					$$.location = @1;
				}
			| BOOLEAN_P
				{
					$$ = _.SystemTypeName("bool");
					$$.location = @1;
				}
		;

opt_float:	'(' Iconst ')'
				{
					/*
					 * Check FLOAT() precision limits assuming IEEE floating
					 * types - thomas 1997-09-18
					 */
					if ($2 < 1)
						ereport(ERROR,
								(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
								 errmsg("precision for type float must be at least 1 bit"),
								 parser_errposition(@2)));
					else if ($2 <= 24)
						$$ = _.SystemTypeName("float4");
					else if ($2 <= 53)
						$$ = _.SystemTypeName("float8");
					else
						ereport(ERROR,
								(errcode(ERRCODE_INVALID_PARAMETER_VALUE),
								 errmsg("precision for type float must be less than 54 bits"),
								 parser_errposition(@2)));
				}
			| /*EMPTY*/
				{
					$$ = _.SystemTypeName("float8");
				}
		;

/*
 * SQL bit-field data types
 * The following implements BIT() and BIT VARYING().
 */
Bit:		BitWithLength
				{
					$$ = $1;
				}
			| BitWithoutLength
				{
					$$ = $1;
				}
		;

/* ConstBit is like Bit except "BIT" defaults to unspecified length */
/* See notes for ConstCharacter, which addresses same issue for "CHAR" */
ConstBit:	BitWithLength
				{
					$$ = $1;
				}
			| BitWithoutLength
				{
					$$ = $1;
					$$.typmods = null;
				}
		;

BitWithLength:
			BIT opt_varying '(' expr_list ')'
				{
					var typname = $2 ? "varbit" : "bit";

					$$ = _.SystemTypeName(typname);
					$$.typmods = $4;
					$$.location = @1;
				}
		;

BitWithoutLength:
			BIT opt_varying
				{
					/* bit defaults to bit(1), varbit to no limit */
					if ($2)
					{
						$$ = _.SystemTypeName("varbit");
					}
					else
					{
						$$ = _.SystemTypeName("bit");
						$$.typmods = [_.makeIntConst(1, -1)];
					}
					$$.location = @1;
				}
		;


/*
 * SQL character data types
 * The following implements CHAR() and VARCHAR().
 */
Character:  CharacterWithLength
				{
					$$ = $1;
				}
			| CharacterWithoutLength
				{
					$$ = $1;
				}
		;

ConstCharacter:  CharacterWithLength
				{
					$$ = $1;
				}
			| CharacterWithoutLength
				{
					/* Length was not specified so allow to be unrestricted.
					 * This handles problems with fixed-length (bpchar) strings
					 * which in column definitions must default to a length
					 * of one, but should not be constrained if the length
					 * was not specified.
					 */
					$$ = $1;
					$$.typmods = null;
				}
		;

CharacterWithLength:  character '(' Iconst ')'
				{
					$$ = _.SystemTypeName($1);
					$$.typmods = [_.makeIntConst($3, @3)];
					$$.location = @1;
				}
		;

CharacterWithoutLength:	 character
				{
					$$ = _.SystemTypeName($1);
					/* char defaults to char(1), varchar to no limit */
					if (strcmp($1, "bpchar") == 0)
						$$.typmods = [_.makeIntConst(1, -1)];
					$$.location = @1;
				}
		;

character:	CHARACTER opt_varying
										{ $$ = $2 ? "varchar": "bpchar"; }
			| CHAR_P opt_varying
										{ $$ = $2 ? "varchar": "bpchar"; }
			| VARCHAR
										{ $$ = "varchar"; }
			| NATIONAL CHARACTER opt_varying
										{ $$ = $3 ? "varchar": "bpchar"; }
			| NATIONAL CHAR_P opt_varying
										{ $$ = $3 ? "varchar": "bpchar"; }
			| NCHAR opt_varying
										{ $$ = $2 ? "varchar": "bpchar"; }
		;

opt_varying:
			VARYING									{ $$ = true; }
			| /*EMPTY*/								{ $$ = false; }
		;

/*
 * SQL date/time types
 */
ConstDatetime:
			TIMESTAMP '(' Iconst ')' opt_timezone
				{
					if ($5)
						$$ = _.SystemTypeName("timestamptz");
					else
						$$ = _.SystemTypeName("timestamp");
					$$.typmods = [_.makeIntConst($3, @3)];
					$$.location = @1;
				}
			| TIMESTAMP opt_timezone
				{
					if ($2)
						$$ = _.SystemTypeName("timestamptz");
					else
						$$ = _.SystemTypeName("timestamp");
					$$.location = @1;
				}
			| TIME '(' Iconst ')' opt_timezone
				{
					if ($5)
						$$ = _.SystemTypeName("timetz");
					else
						$$ = _.SystemTypeName("time");
					$$.typmods = [_.makeIntConst($3, @3)];
					$$.location = @1;
				}
			| TIME opt_timezone
				{
					if ($2)
						$$ = _.SystemTypeName("timetz");
					else
						$$ = _.SystemTypeName("time");
					$$.location = @1;
				}
		;

ConstInterval:
			INTERVAL
				{
					$$ = _.SystemTypeName("interval");
					$$.location = @1;
				}
		;

opt_timezone:
			WITH_LA TIME ZONE						{ $$ = true; }
			| WITHOUT TIME ZONE						{ $$ = false; }
			| /*EMPTY*/								{ $$ = false; }
		;

opt_interval:
			YEAR_P
				{ $$ = [_.makeIntConst(_.INTERVAL_MASK(_.YEAR), @1)]; }
			| MONTH_P
				{ $$ = [_.makeIntConst(_.INTERVAL_MASK(_.MONTH), @1)]; }
			| DAY_P
				{ $$ = [_.makeIntConst(_.INTERVAL_MASK(_.DAY), @1)]; }
			| HOUR_P
				{ $$ = [_.makeIntConst(_.INTERVAL_MASK(_.HOUR), @1)]; }
			| MINUTE_P
				{ $$ = [_.makeIntConst(_.INTERVAL_MASK(_.MINUTE), @1)]; }
			| interval_second
				{ $$ = $1; }
			| YEAR_P TO MONTH_P
				{
					$$ = [_.makeIntConst(_.INTERVAL_MASK(_.YEAR) |
												 _.INTERVAL_MASK(_.MONTH), @1)];
				}
			| DAY_P TO HOUR_P
				{
					$$ = [_.makeIntConst(_.INTERVAL_MASK(_.DAY) |
												 _.INTERVAL_MASK(_.HOUR), @1)];
				}
			| DAY_P TO MINUTE_P
				{
					$$ = [_.makeIntConst(_.INTERVAL_MASK(_.DAY) |
												 _.INTERVAL_MASK(_.HOUR) |
												 _.INTERVAL_MASK(_.MINUTE), @1)];
				}
			| DAY_P TO interval_second
				{
					$$ = $3;
					linitial($$) = _.makeIntConst(_.INTERVAL_MASK(_.DAY) |
												_.INTERVAL_MASK(_.HOUR) |
												_.INTERVAL_MASK(_.MINUTE) |
												_.INTERVAL_MASK(_.SECOND), @1);
				}
			| HOUR_P TO MINUTE_P
				{
					$$ = [_.makeIntConst(_.INTERVAL_MASK(_.HOUR) |
												 _.INTERVAL_MASK(_.MINUTE), @1)];
				}
			| HOUR_P TO interval_second
				{
					$$ = $3;
					linitial($$) = _.makeIntConst(_.INTERVAL_MASK(_.HOUR) |
												_.INTERVAL_MASK(_.MINUTE) |
												_.INTERVAL_MASK(_.SECOND), @1);
				}
			| MINUTE_P TO interval_second
				{
					$$ = $3;
					linitial($$) = _.makeIntConst(_.INTERVAL_MASK(_.MINUTE) |
												_.INTERVAL_MASK(_.SECOND), @1);
				}
			| /*EMPTY*/
				{ $$ = null; }
		;

interval_second:
			SECOND_P
				{
					$$ = [_.makeIntConst(_.INTERVAL_MASK(_.SECOND), @1)];
				}
			| SECOND_P '(' Iconst ')'
				{
					$$ = [_.makeIntConst(_.INTERVAL_MASK(_.SECOND), @1),
									_.makeIntConst($3, @3)];
				}
		;

/*
 * General expressions
 * This is the heart of the expression syntax.
 *
 * We have two expression types: a_expr is the unrestricted kind, and
 * b_expr is a subset that must be used in some places to avoid shift/reduce
 * conflicts.  For example, we can't do BETWEEN as "BETWEEN a_expr AND a_expr"
 * because that use of AND conflicts with AND as a boolean operator.  So,
 * b_expr is used in BETWEEN and we remove boolean keywords from b_expr.
 *
 * Note that '(' a_expr ')' is a b_expr, so an unrestricted expression can
 * always be used by surrounding it with parens.
 *
 * c_expr is all the productions that are common to a_expr and b_expr;
 * it's factored out just to eliminate redundant coding.
 *
 * Be careful of productions involving more than one terminal token.
 * By default, bison will assign such productions the precedence of their
 * last terminal, but in nearly all cases you want it to be the precedence
 * of the first terminal instead; otherwise you will not get the behavior
 * you expect!  So we use %prec annotations freely to set precedences.
 */
a_expr:		c_expr									{ $$ = $1; }
			| a_expr TYPECAST Typename
					{ $$ = _.makeTypeCast($1, $3, @2); }
			| a_expr COLLATE any_name
				{
					$$ = {
						type: _.NodeTag.T_CollateClause,
						arg: $1,
						collname: $3,
						location: @2
					};
				}
			| a_expr AT TIME ZONE a_expr			%prec AT
				{
					$$ = _.makeFuncCall(_.SystemFuncName("timezone"),
											   [$5, $1],
											   @2);
				}
			| '+' a_expr					%prec UMINUS
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "+", null, $2, @1); }
			| '-' a_expr					%prec UMINUS
				{ $$ = _.doNegate($2, @1); }
			| a_expr '+' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "+", $1, $3, @2); }
			| a_expr '-' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "-", $1, $3, @2); }
			| a_expr '*' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "*", $1, $3, @2); }
			| a_expr '/' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "/", $1, $3, @2); }
			| a_expr '%' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "%", $1, $3, @2); }
			| a_expr '^' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "^", $1, $3, @2); }
			| a_expr '<' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "<", $1, $3, @2); }
			| a_expr '>' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, ">", $1, $3, @2); }
			| a_expr '=' a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "=", $1, $3, @2); }
			| a_expr LESS_EQUALS a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "<=", $1, $3, @2); }
			| a_expr GREATER_EQUALS a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, ">=", $1, $3, @2); }
			| a_expr NOT_EQUALS a_expr
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "<>", $1, $3, @2); }

			| a_expr qual_Op a_expr				%prec Op
				{ $$ = _.makeA_Expr(_.A_Expr_Kind.AEXPR_OP, $2, $1, $3, @2); }
			| qual_Op a_expr					%prec Op
				{ $$ = _.makeA_Expr(_.A_Expr_Kind.AEXPR_OP, $1, null, $2, @1); }
			| a_expr qual_Op					%prec POSTFIXOP
				{ $$ = _.makeA_Expr(_.A_Expr_Kind.AEXPR_OP, $2, $1, null, @2); }

			| a_expr AND a_expr
				{ $$ = _.makeAndExpr($1, $3, @2); }
			| a_expr OR a_expr
				{ $$ = _.makeOrExpr($1, $3, @2); }
			| NOT a_expr
				{ $$ = _.makeNotExpr($2, @1); }
			| NOT_LA a_expr						%prec NOT
				{ $$ = _.makeNotExpr($2, @1); }
		;


any_name_list:
			any_name								{ $$ = [$1]; }
			| any_name_list ',' any_name			{ $1.push($3); $$ = $1; }
		;

any_name:	ColId						{ $$ = [_.makeString($1)]; }
			| ColId attrs				{ $2.unshift(_.makeString($1)); $$ = $2; }
		;

attrs:		'.' attr_name
					{ $$ = [makeString($2)]; }
			| attrs '.' attr_name
					{ $1.push(_.makeString($3)); $$ = $1; }
		;

type_name_list:
			Typename								{ $$ = [$1]; }
			| type_name_list ',' Typename			{ $1.push($3); $$ = $1; }
		;

/*****************************************************************************
 *
 *	target list for SELECT
 *
 *****************************************************************************/

opt_target_list: target_list						{ $$ = $1; }
			| /* EMPTY */							{ $$ = null; }
		;

target_list:
			target_el								{ $$ = [$1]; }
			| target_list ',' target_el				{ $$ = $1; $1.push($3); }
		;

target_el:	a_expr AS ColLabel
				{
					$$ = {
						type: _.NodeTag.T_ResTarget,
						name: $3,
						indirection: null,
						val: $1,
						location: @1
					}
				}
			/*
			 * We support omitting AS only for column labels that aren't
			 * any known keyword.  There is an ambiguity against postfix
			 * operators: is "a ! b" an infix expression, or a postfix
			 * expression and a column label?  We prefer to resolve this
			 * as an infix expression, which we accomplish by assigning
			 * IDENT a precedence higher than POSTFIXOP.
			 */
			| a_expr IDENT
				{
					$$ = {
						type: _.NodeTag.T_ResTarget,
						name: $2,
						indirection: null,
						val: $1,
						location: @1
					}
				}
			| a_expr
				{
					$$ = {
						type: _.NodeTag.T_ResTarget,
						name: null,
						indirection: null,
						val: $1,
						location: @1
					}
				}
			| '*'
				{
					$$ = {
						type: _.NodeTag.T_ResTarget,
						name: null,
						indirection: null,
						val: {
							type: _.NodeTag.T_ColumnRef,
							fields: [{ type: _.NodeTag.T_A_Star }],
							location: @1
						},
						location: @1
					}
				}
		;


/*****************************************************************************
 *
 *	Names and constants
 *
 *****************************************************************************/

qualified_name_list:
			qualified_name							{ $$ = [$1]; }
			| qualified_name_list ',' qualified_name { $$ = $1; $1.push($3); }
		;

/*
 * The production for a qualified relation name has to exactly match the
 * production for a qualified func_name, because in a FROM clause we cannot
 * tell which we are parsing until we see what comes after it ('(' for a
 * func_name, something else for a relation). Therefore we allow 'indirection'
 * which may contain subscripts, and reject that case in the C code.
 */
qualified_name:
			ColId
				{
					$$ = _.makeRangeVar(null, $1, @1);
				}
			| ColId indirection
				{
					// check_qualified_name($2, yyscanner);
					$$ = _.makeRangeVar(null, null, @1);
					switch ($2.length)
					{
						case 1:
							$$.catalogname = null;
							$$.schemaname = $1;
							$$.relname = $2[0].str;
							break;
						case 2:
							$$.catalogname = $1;
							$$.schemaname = $2[0].str;
							$$.relname = $2[1].str
							break;
						default:
							$2.unshift(_.makeString($1));
							/* ereport(ERROR,
									(errcode(ERRCODE_SYNTAX_ERROR),
									 errmsg("improper qualified name (too many dotted names): %s",
											NameListToString($2))),
									 parser_errposition(@1))); */
							throw new Error(`improper qualified name (too many dotted names): ${$1}, ${$2}`)
							break;
					}
				}
		;

opt_name_list:
			'(' name_list ')'						{ $$ = $2; }
			| /*EMPTY*/								{ $$ = null; }
		;

name_list:	name
					{ $$ = [_.makeString($1)]; }
			| name_list ',' name
					{ $1.push(_.makeString($3)); $$ = $1; }
		;


name:		ColId									{ $$ = $1; };

database_name:
			ColId									{ $$ = $1; };

access_method:
			ColId									{ $$ = $1; };

attr_name:	ColLabel								{ $$ = $1; };

index_name: ColId									{ $$ = $1; };

file_name:	Sconst									{ $$ = $1; };


opt_table:	TABLE									{}
			| /*EMPTY*/								{}
		;

all_or_distinct:
			ALL										{ $$ = true; }
			| DISTINCT								{ $$ = false; }
			| /*EMPTY*/								{ $$ = false; }
		;

/* We use (NIL) as a placeholder to indicate that all target expressions
 * should be placed in the DISTINCT list during parsetree analysis.
 */
distinct_clause:
			DISTINCT								{ $$ = []; }
			| DISTINCT ON '(' expr_list ')'			{ $$ = $4; }
		;

opt_all_clause:
			ALL										{ $$ = null;}
			| /*EMPTY*/								{ $$ = null; }
		;

opt_sort_clause:
			sort_clause								{ $$ = $1;}
			| /*EMPTY*/								{ $$ = null; }
		;

sort_clause:
			ORDER BY sortby_list					{ $$ = $3; }
		;

sortby_list:
			sortby									{ $$ = [$1]; }
			| sortby_list ',' sortby				{ $1.push($3); $$ = $1; }
		;


select_limit:
			limit_clause offset_clause			{ $$ = [$2, $1]; }
			| offset_clause limit_clause		{ $$ = [$1, $2]; }
			| limit_clause						{ $$ = [NULL, $1]; }
			| offset_clause						{ $$ = [$1, NULL]; }
		;

opt_select_limit:
			select_limit						{ $$ = $1; }
			| /* EMPTY */						{ $$ = [NULL,NULL]; }
		;

limit_clause:
			LIMIT select_limit_value
				{ $$ = $2; }
			| LIMIT select_limit_value ',' select_offset_value
				{
					/* Disabled because it was too confusing, bjm 2002-02-18 */
					ereport(ERROR,
							(errcode(ERRCODE_SYNTAX_ERROR),
							 errmsg("LIMIT #,# syntax is not supported"),
							 errhint("Use separate LIMIT and OFFSET clauses."),
							 parser_errposition(@1)));
				}
			/* SQL:2008 syntax */
			/* to avoid shift/reduce conflicts, handle the optional value with
			 * a separate production rather than an opt_ expression.  The fact
			 * that ONLY is fully reserved means that this way, we defer any
			 * decision about what rule reduces ROW or ROWS to the point where
			 * we can see the ONLY token in the lookahead slot.
			 */
			| FETCH first_or_next select_fetch_first_value row_or_rows ONLY
				{ $$ = $3; }
			| FETCH first_or_next row_or_rows ONLY
				{ $$ = _.makeIntConst(1, -1); }
		;

offset_clause:
			OFFSET select_offset_value
				{ $$ = $2; }
			/* SQL:2008 syntax */
			| OFFSET select_fetch_first_value row_or_rows
				{ $$ = $2; }
		;

select_limit_value:
			a_expr									{ $$ = $1; }
			| ALL
				{
					/* LIMIT ALL is represented as a NULL constant */
					$$ = _.makeNullAConst(@1);
				}
		;

select_offset_value:
			a_expr									{ $$ = $1; }
		;
