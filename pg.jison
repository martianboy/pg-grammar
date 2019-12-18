%pure-parser
%expect 0
%locations

%token <keyword> ABORT_P 
%token <str>	IDENT FCONST SCONST BCONST XCONST Op
%token <ival>	ICONST PARAM
%token			TYPECAST DOT_DOT COLON_EQUALS EQUALS_GREATER
%token			LESS_EQUALS GREATER_EQUALS NOT_EQUALS

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

// unreserved_keyword:
// 			  ABORT_P
// 			| ABSOLUTE_P
// 			| ACCESS
// 			| ACTION
// 			| ADD_P
// 			| ADMIN
// 			| AFTER
// 			| AGGREGATE
// 			| ALSO
// 			| ALTER
// 			| ALWAYS
// 			| ASSERTION
// 			| ASSIGNMENT
// 			| AT
// 			| ATTACH
// 			| ATTRIBUTE
// 			| BACKWARD
// 			| BEFORE
// 			| BEGIN_P
// 			| BY
// 			| CACHE
// 			| CALL
// 			| CALLED
// 			| CASCADE
// 			| CASCADED
// 			| CATALOG_P
// 			| CHAIN
// 			| CHARACTERISTICS
// 			| CHECKPOINT
// 			| CLASS
// 			| CLOSE
// 			| CLUSTER
// 			| COLUMNS
// 			| COMMENT
// 			| COMMENTS
// 			| COMMIT
// 			| COMMITTED
// 			| CONFIGURATION
// 			| CONFLICT
// 			| CONNECTION
// 			| CONSTRAINTS
// 			| CONTENT_P
// 			| CONTINUE_P
// 			| CONVERSION_P
// 			| COPY
// 			| COST
// 			| CSV
// 			| CUBE
// 			| CURRENT_P
// 			| CURSOR
// 			| CYCLE
// 			| DATA_P
// 			| DATABASE
// 			| DAY_P
// 			| DEALLOCATE
// 			| DECLARE
// 			| DEFAULTS
// 			| DEFERRED
// 			| DEFINER
// 			| DELETE_P
// 			| DELIMITER
// 			| DELIMITERS
// 			| DEPENDS
// 			| DETACH
// 			| DICTIONARY
// 			| DISABLE_P
// 			| DISCARD
// 			| DOCUMENT_P
// 			| DOMAIN_P
// 			| DOUBLE_P
// 			| DROP
// 			| EACH
// 			| ENABLE_P
// 			| ENCODING
// 			| ENCRYPTED
// 			| ENUM_P
// 			| ESCAPE
// 			| EVENT
// 			| EXCLUDE
// 			| EXCLUDING
// 			| EXCLUSIVE
// 			| EXECUTE
// 			| EXPLAIN
// 			| EXTENSION
// 			| EXTERNAL
// 			| FAMILY
// 			| FILTER
// 			| FIRST_P
// 			| FOLLOWING
// 			| FORCE
// 			| FORWARD
// 			| FUNCTION
// 			| FUNCTIONS
// 			| GENERATED
// 			| GLOBAL
// 			| GRANTED
// 			| GROUPS
// 			| HANDLER
// 			| HEADER_P
// 			| HOLD
// 			| HOUR_P
// 			| IDENTITY_P
// 			| IF_P
// 			| IMMEDIATE
// 			| IMMUTABLE
// 			| IMPLICIT_P
// 			| IMPORT_P
// 			| INCLUDE
// 			| INCLUDING
// 			| INCREMENT
// 			| INDEX
// 			| INDEXES
// 			| INHERIT
// 			| INHERITS
// 			| INLINE_P
// 			| INPUT_P
// 			| INSENSITIVE
// 			| INSERT
// 			| INSTEAD
// 			| INVOKER
// 			| ISOLATION
// 			| KEY
// 			| LABEL
// 			| LANGUAGE
// 			| LARGE_P
// 			| LAST_P
// 			| LEAKPROOF
// 			| LEVEL
// 			| LISTEN
// 			| LOAD
// 			| LOCAL
// 			| LOCATION
// 			| LOCK_P
// 			| LOCKED
// 			| LOGGED
// 			| MAPPING
// 			| MATCH
// 			| MATERIALIZED
// 			| MAXVALUE
// 			| METHOD
// 			| MINUTE_P
// 			| MINVALUE
// 			| MODE
// 			| MONTH_P
// 			| MOVE
// 			| NAME_P
// 			| NAMES
// 			| NEW
// 			| NEXT
// 			| NO
// 			| NOTHING
// 			| NOTIFY
// 			| NOWAIT
// 			| NULLS_P
// 			| OBJECT_P
// 			| OF
// 			| OFF
// 			| OIDS
// 			| OLD
// 			| OPERATOR
// 			| OPTION
// 			| OPTIONS
// 			| ORDINALITY
// 			| OTHERS
// 			| OVER
// 			| OVERRIDING
// 			| OWNED
// 			| OWNER
// 			| PARALLEL
// 			| PARSER
// 			| PARTIAL
// 			| PARTITION
// 			| PASSING
// 			| PASSWORD
// 			| PLANS
// 			| POLICY
// 			| PRECEDING
// 			| PREPARE
// 			| PREPARED
// 			| PRESERVE
// 			| PRIOR
// 			| PRIVILEGES
// 			| PROCEDURAL
// 			| PROCEDURE
// 			| PROCEDURES
// 			| PROGRAM
// 			| PUBLICATION
// 			| QUOTE
// 			| RANGE
// 			| READ
// 			| REASSIGN
// 			| RECHECK
// 			| RECURSIVE
// 			| REF
// 			| REFERENCING
// 			| REFRESH
// 			| REINDEX
// 			| RELATIVE_P
// 			| RELEASE
// 			| RENAME
// 			| REPEATABLE
// 			| REPLACE
// 			| REPLICA
// 			| RESET
// 			| RESTART
// 			| RESTRICT
// 			| RETURNS
// 			| REVOKE
// 			| ROLE
// 			| ROLLBACK
// 			| ROLLUP
// 			| ROUTINE
// 			| ROUTINES
// 			| ROWS
// 			| RULE
// 			| SAVEPOINT
// 			| SCHEMA
// 			| SCHEMAS
// 			| SCROLL
// 			| SEARCH
// 			| SECOND_P
// 			| SECURITY
// 			| SEQUENCE
// 			| SEQUENCES
// 			| SERIALIZABLE
// 			| SERVER
// 			| SESSION
// 			| SET
// 			| SETS
// 			| SHARE
// 			| SHOW
// 			| SIMPLE
// 			| SKIP
// 			| SNAPSHOT
// 			| SQL_P
// 			| STABLE
// 			| STANDALONE_P
// 			| START
// 			| STATEMENT
// 			| STATISTICS
// 			| STDIN
// 			| STDOUT
// 			| STORAGE
// 			| STORED
// 			| STRICT_P
// 			| STRIP_P
// 			| SUBSCRIPTION
// 			| SUPPORT
// 			| SYSID
// 			| SYSTEM_P
// 			| TABLES
// 			| TABLESPACE
// 			| TEMP
// 			| TEMPLATE
// 			| TEMPORARY
// 			| TEXT_P
// 			| TIES
// 			| TRANSACTION
// 			| TRANSFORM
// 			| TRIGGER
// 			| TRUNCATE
// 			| TRUSTED
// 			| TYPE_P
// 			| TYPES_P
// 			| UNBOUNDED
// 			| UNCOMMITTED
// 			| UNENCRYPTED
// 			| UNKNOWN
// 			| UNLISTEN
// 			| UNLOGGED
// 			| UNTIL
// 			| UPDATE
// 			| VACUUM
// 			| VALID
// 			| VALIDATE
// 			| VALIDATOR
// 			| VALUE_P
// 			| VARYING
// 			| VERSION_P
// 			| VIEW
// 			| VIEWS
// 			| VOLATILE
// 			| WHITESPACE_P
// 			| WITHIN
// 			| WITHOUT
// 			| WORK
// 			| WRAPPER
// 			| WRITE
// 			| XML_P
// 			| YEAR_P
// 			| YES_P
// 			| ZONE
// 		;

// /* Column identifier --- keywords that can be column, table, etc names.
//  *
//  * Many of these keywords will in fact be recognized as type or function
//  * names too; but they have special productions for the purpose, and so
//  * can't be treated as "generic" type or function names.
//  *
//  * The type names appearing here are not usable as function names
//  * because they can be followed by '(' in typename productions, which
//  * looks too much like a function call for an LR(1) parser.
//  */
// col_name_keyword:
// 			  BETWEEN
// 			| BIGINT
// 			| BIT
// 			| BOOLEAN_P
// 			| CHAR_P
// 			| CHARACTER
// 			| COALESCE
// 			| DEC
// 			| DECIMAL_P
// 			| EXISTS
// 			| EXTRACT
// 			| FLOAT_P
// 			| GREATEST
// 			| GROUPING
// 			| INOUT
// 			| INT_P
// 			| INTEGER
// 			| INTERVAL
// 			| LEAST
// 			| NATIONAL
// 			| NCHAR
// 			| NONE
// 			| NULLIF
// 			| NUMERIC
// 			| OUT_P
// 			| OVERLAY
// 			| POSITION
// 			| PRECISION
// 			| REAL
// 			| ROW
// 			| SETOF
// 			| SMALLINT
// 			| SUBSTRING
// 			| TIME
// 			| TIMESTAMP
// 			| TREAT
// 			| TRIM
// 			| VALUES
// 			| VARCHAR
// 			| XMLATTRIBUTES
// 			| XMLCONCAT
// 			| XMLELEMENT
// 			| XMLEXISTS
// 			| XMLFOREST
// 			| XMLNAMESPACES
// 			| XMLPARSE
// 			| XMLPI
// 			| XMLROOT
// 			| XMLSERIALIZE
// 			| XMLTABLE
// 		;

// name_list:	name
// 					{ $$ = list_make1(makeString($1)); }
// 			| name_list ',' name
// 					{ $$ = lappend($1, makeString($3)); }
// 		;

stmtblock:	stmtmulti       { console.log('stmtblock#1'); return $1; }
            | stmtmulti EOF { console.log($1); return $1; }
        ;

stmtmulti:	stmt
				{
					if ($1 != null)
						$$ = [$1];
					else
						$$ = null;
				}
		;

stmt:       IDENT         { return $1; };
