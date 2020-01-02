%code imports {
	const _ = require('./src');
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

stmt:       a_expr;


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
					$$ = {
						type: _.NodeTag.T_RangeTableSample,
						/* n->relation will be filled in later */
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
					const ai = _.makeNode('A_Indices');
					ai.is_slice = false;
					ai.lidx = null;
					ai.uidx = $2;
					$$ = ai;
				}
			| '[' opt_slice_bound ':' opt_slice_bound ']'
				{
					ai = _.makeNode('A_Indices');
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
		;


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
					char *typname;

					typname = $2 ? "varbit" : "bit";
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
					$$ = _.makeFuncCall(SystemFuncName("timezone"),
											   [$5, $1],
											   @2);
				}
			| '+' a_expr					%prec UMINUS
				{ $$ = _.makeSimpleA_Expr(_.A_Expr_Kind.AEXPR_OP, "+", NULL, $2, @1); }
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
