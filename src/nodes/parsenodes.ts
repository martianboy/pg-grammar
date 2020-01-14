import { Value } from "./value";
import { NodeTag } from './tags';
import { Node, OnConflictAction } from './node';
import { IntoClause, Expr, RangeVar, Alias, Oid } from "./primnodes";

/* Sort ordering options for ORDER BY and CREATE INDEX */
export enum SortByDir
{
	SORTBY_DEFAULT,
	SORTBY_ASC,
	SORTBY_DESC,
	SORTBY_USING				/* not allowed in CREATE INDEX ... */
}

export enum SortByNulls
{
	SORTBY_NULLS_DEFAULT,
	SORTBY_NULLS_FIRST,
	SORTBY_NULLS_LAST
}

export interface ColumnRef extends Node<NodeTag.T_ColumnRef> {
  fields: any[];
  location: number;
}

export interface ParamRef extends Node<NodeTag.T_ParamRef> {
	number: number;			/* the number of the parameter */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * A_Expr - infix, prefix, and postfix expressions
 */
export enum A_Expr_Kind
{
	AEXPR_OP,					/* normal operator */
	AEXPR_OP_ANY,				/* scalar op ANY (array) */
	AEXPR_OP_ALL,				/* scalar op ALL (array) */
	AEXPR_DISTINCT,				/* IS DISTINCT FROM - name must be "=" */
	AEXPR_NOT_DISTINCT,			/* IS NOT DISTINCT FROM - name must be "=" */
	AEXPR_NULLIF,				/* NULLIF - name must be "=" */
	AEXPR_OF,					/* IS [NOT] OF - name must be "=" or "<>" */
	AEXPR_IN,					/* [NOT] IN - name must be "=" or "<>" */
	AEXPR_LIKE,					/* [NOT] LIKE - name must be "~~" or "!~~" */
	AEXPR_ILIKE,				/* [NOT] ILIKE - name must be "~~*" or "!~~*" */
	AEXPR_SIMILAR,				/* [NOT] SIMILAR - name must be "~" or "!~" */
	AEXPR_BETWEEN,				/* name must be "BETWEEN" */
	AEXPR_NOT_BETWEEN,			/* name must be "NOT BETWEEN" */
	AEXPR_BETWEEN_SYM,			/* name must be "BETWEEN SYMMETRIC" */
	AEXPR_NOT_BETWEEN_SYM,		/* name must be "NOT BETWEEN SYMMETRIC" */
	AEXPR_PAREN					/* nameless dummy node for parentheses */
}

export interface A_Expr extends Node<NodeTag.T_A_Expr> {
	kind: A_Expr_Kind;		/* see above */
	name: Node<NodeTag.T_String>[];    			/* possibly-qualified name of operator */
	lexpr: Expr | null;		    	/* left argument, or NULL if none */
	rexpr: Expr | null;			    /* right argument, or NULL if none */
	location: number;		  /* token location, or -1 if unknown */
}

/*
 * A_Const - a literal constant
 */
export interface A_Const extends Node<NodeTag.T_A_Const> {
	val?: Value<any>;			/* value (includes type info, see value.h) */
	location: number;		/* token location, or -1 if unknown */
}
/*
 * TypeName - specifies a type in definitions
 *
 * For TypeName structures generated internally, it is often easier to
 * specify the type by OID than by name.  If "names" is NIL then the
 * actual type OID is given by typeOid, otherwise typeOid is unused.
 * Similarly, if "typmods" is NIL then the actual typmod is expected to
 * be prespecified in typemod, otherwise typemod is unused.
 *
 * If pct_type is true, then names is actually a field name and we look up
 * the type of that field.  Otherwise (the normal case), names is a type
 * name possibly qualified with schema and database name.
 */
export interface TypeName extends Node<NodeTag.T_TypeName> {
	names: Node<NodeTag.T_String>[];			/* qualified name (list of Value strings) */
	// Oid			typeOid;		/* type identified by OID */
	setof?: boolean;			/* is a set? */
	pct_type?: boolean;		/* %TYPE specified? */
	typmods: unknown[];		/* type modifier expression(s) */
	typemod: number;		/* prespecified type modifier */
	arrayBounds?: unknown[];	/* array bounds */
	location: number;		/* token location, or -1 if unknown */
};

/*
 * TypeCast - a CAST expression
 */
export interface TypeCast<T extends NodeTag = any> extends Node<NodeTag.T_TypeCast> {
	arg: Node<T>;			/* the expression being casted */
	typeName: TypeName;		/* the target type */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * CollateClause - a COLLATE expression
 */
export interface CollateClause extends Node<NodeTag.T_CollateClause>
{
	arg: unknown;			/* input expression */
	collname: Value<NodeTag.T_String>;		/* possibly-qualified collation name */
	location: number;		/* token location, or -1 if unknown */
};

/*
 * FuncCall - a function or aggregate invocation
 *
 * agg_order (if not NIL) indicates we saw 'foo(... ORDER BY ...)', or if
 * agg_within_group is true, it was 'foo(...) WITHIN GROUP (ORDER BY ...)'.
 * agg_star indicates we saw a 'foo(*)' construct, while agg_distinct
 * indicates we saw 'foo(DISTINCT ...)'.  In any of these cases, the
 * construct *must* be an aggregate call.  Otherwise, it might be either an
 * aggregate or some other kind of function.  However, if FILTER or OVER is
 * present it had better be an aggregate or window function.
 *
 * Normally, you'd initialize this via makeFuncCall() and then only change the
 * parts of the struct its defaults don't match afterwards, as needed.
 */
export interface FuncCall extends Node<NodeTag.T_FuncCall>
{
	funcname: Value<NodeTag.T_String>[];		/* qualified name of function */
	args: Expr[];				/* the arguments (list of exprs) */
	agg_order: SortBy[] | null;	/* ORDER BY (list of SortBy) */
	agg_filter: unknown | null;	/* FILTER clause, if any */
	agg_within_group: boolean;	/* ORDER BY appeared in WITHIN GROUP */
	agg_star: boolean;			/* argument was really '*' */
	agg_distinct: boolean;		/* arguments were labeled DISTINCT */
	func_variadic: boolean;		/* last argument was labeled VARIADIC */
	over: WindowDef | null;		/* OVER clause, if any */
	location: number;			/* token location, or -1 if unknown */
}

/*
 * A_Star - '*' representing all columns of a table or compound field
 *
 * This can appear within ColumnRef.fields, A_Indirection.indirection, and
 * ResTarget.indirection lists.
 */
export interface A_Star extends Node<NodeTag.T_A_Star>
{
}

/*
 * A_Indices - array subscript or slice bounds ([idx] or [lidx:uidx])
 *
 * In slice case, either or both of lidx and uidx can be NULL (omitted).
 * In non-slice case, uidx holds the single subscript and lidx is always NULL.
 */
export interface A_Indices extends Node<NodeTag.T_A_Indices>
{
	is_slice: boolean;		/* true if slice (i.e., colon present) */
	lidx: unknown;			/* slice lower bound, if any */
	uidx: unknown;			/* subscript, or slice upper bound if any */
}

/*
 * A_Indirection - select a field and/or array element from an expression
 *
 * The indirection list can contain A_Indices nodes (representing
 * subscripting), string Value nodes (representing field selection --- the
 * string value is the name of the field to select), and A_Star nodes
 * (representing selection of all fields of a composite type).
 * For example, a complex selection operation like
 *				(foo).field1[42][7].field2
 * would be represented with a single A_Indirection node having a 4-element
 * indirection list.
 *
 * Currently, A_Star must appear only as the last list element --- the grammar
 * is responsible for enforcing this!
 */
export interface A_Indirection extends Node<NodeTag.T_A_Indirection>
{
	arg: unknown;		      /* the thing being selected from */
	indirection: unknown[];	  /* subscripts and/or field names and/or * */
}

export type Indirection =
	| Value<NodeTag.T_String>
	| A_Star
	| A_Indices;

/*
 * A_ArrayExpr - an ARRAY[] construct
 */
export interface A_ArrayExpr extends Node<NodeTag.T_A_ArrayExpr>
{
	elements: unknown[];		/* array element expressions */
	location: number;		      /* token location, or -1 if unknown */
}

/*
 * ResTarget -
 *	  result target (used in target list of pre-transformed parse trees)
 *
 * In a SELECT target list, 'name' is the column label from an
 * 'AS ColumnLabel' clause, or NULL if there was none, and 'val' is the
 * value expression itself.  The 'indirection' field is not used.
 *
 * INSERT uses ResTarget in its target-column-names list.  Here, 'name' is
 * the name of the destination column, 'indirection' stores any subscripts
 * attached to the destination, and 'val' is not used.
 *
 * In an UPDATE target list, 'name' is the name of the destination column,
 * 'indirection' stores any subscripts attached to the destination, and
 * 'val' is the expression to assign.
 *
 * See A_Indirection for more info about what can appear in 'indirection'.
 */
export interface ResTarget extends Node<NodeTag.T_ResTarget>
{
	name: string;			/* column name or NULL */
	indirection: A_Indirection[] | null;	/* subscripts, field names, and '*', or NIL */
	val: Expr;			/* the value expression to compute or assign */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * SortBy - for ORDER BY clause
 */
export interface SortBy extends Node<NodeTag.T_SortBy>
{
	node: A_Expr;			/* expression to sort on */
	sortby_dir: SortByDir;		/* ASC/DESC/USING/default */
	sortby_nulls: SortByNulls;	/* NULLS FIRST/LAST */
	useOp: unknown[];			/* name of op to use, if SORTBY_USING */
	location: number;		/* operator location, or -1 if none/unknown */
}

/*
 * WindowDef - raw representation of WINDOW and OVER clauses
 *
 * For entries in a WINDOW list, "name" is the window name being defined.
 * For OVER clauses, we use "name" for the "OVER window" syntax, or "refname"
 * for the "OVER (window)" syntax, which is subtly different --- the latter
 * implies overriding the window frame clause.
 */
export interface WindowDef extends Node<NodeTag.T_WindowDef>
{
	name: string;			/* window's own name */
	refname: string;		/* referenced window name, if any */
	partitionClause: unknown[];	/* PARTITION BY expression list */
	orderClause: unknown[];	/* ORDER BY (list of SortBy) */
	frameOptions: number;	/* frame_clause options, see below */
	startOffset: unknown;	/* expression for starting bound, if any */
	endOffset: unknown;		/* expression for ending bound, if any */
	location: number;		/* parse location, or -1 if none/unknown */
}

/*
 * frameOptions is an OR of these bits.  The NONDEFAULT and BETWEEN bits are
 * used so that ruleutils.c can tell which properties were specified and
 * which were defaulted; the correct behavioral bits must be set either way.
 * The START_foo and END_foo options must come in pairs of adjacent bits for
 * the convenience of gram.y, even though some of them are useless/invalid.
 */
export const FRAMEOPTION_NONDEFAULT = 					0x00001 /* any specified? */
export const FRAMEOPTION_RANGE = 						0x00002 /* RANGE behavior */
export const FRAMEOPTION_ROWS = 						0x00004 /* ROWS behavior */
export const FRAMEOPTION_GROUPS = 						0x00008 /* GROUPS behavior */
export const FRAMEOPTION_BETWEEN = 						0x00010 /* BETWEEN given? */
export const FRAMEOPTION_START_UNBOUNDED_PRECEDING = 	0x00020 /* start is U. P. */
export const FRAMEOPTION_END_UNBOUNDED_PRECEDING = 		0x00040 /* (disallowed) */
export const FRAMEOPTION_START_UNBOUNDED_FOLLOWING = 	0x00080 /* (disallowed) */
export const FRAMEOPTION_END_UNBOUNDED_FOLLOWING = 		0x00100 /* end is U. F. */
export const FRAMEOPTION_START_CURRENT_ROW = 			0x00200 /* start is C. R. */
export const FRAMEOPTION_END_CURRENT_ROW = 				0x00400 /* end is C. R. */
export const FRAMEOPTION_START_OFFSET_PRECEDING = 		0x00800 /* start is O. P. */
export const FRAMEOPTION_END_OFFSET_PRECEDING = 		0x01000 /* end is O. P. */
export const FRAMEOPTION_START_OFFSET_FOLLOWING = 		0x02000 /* start is O. F. */
export const FRAMEOPTION_END_OFFSET_FOLLOWING = 		0x04000 /* end is O. F. */
export const FRAMEOPTION_EXCLUDE_CURRENT_ROW = 			0x08000 /* omit C.R. */
export const FRAMEOPTION_EXCLUDE_GROUP = 				0x10000 /* omit C.R. & peers */
export const FRAMEOPTION_EXCLUDE_TIES = 				0x20000 /* omit C.R.'s peers */

export const FRAMEOPTION_START_OFFSET = 
	(FRAMEOPTION_START_OFFSET_PRECEDING | FRAMEOPTION_START_OFFSET_FOLLOWING)
export const FRAMEOPTION_END_OFFSET = 
	(FRAMEOPTION_END_OFFSET_PRECEDING | FRAMEOPTION_END_OFFSET_FOLLOWING)
export const FRAMEOPTION_EXCLUSION = 
	(FRAMEOPTION_EXCLUDE_CURRENT_ROW | FRAMEOPTION_EXCLUDE_GROUP |
	 FRAMEOPTION_EXCLUDE_TIES)

export const FRAMEOPTION_DEFAULTS = 
	(FRAMEOPTION_RANGE | FRAMEOPTION_START_UNBOUNDED_PRECEDING |
	 FRAMEOPTION_END_CURRENT_ROW)


/*
 * RangeSubselect - subquery appearing in a FROM clause
 */
export interface RangeSubselect extends Node<NodeTag.T_RangeSubselect>
{
	lateral: boolean;		/* does it have LATERAL prefix? */
	subquery: unknown;		/* the untransformed sub-select clause */
	alias: Alias;			/* table alias & optional column aliases */
}

/*
 * RangeFunction - function call appearing in a FROM clause
 *
 * functions is a List because we use this to represent the construct
 * ROWS FROM(func1(...), func2(...), ...).  Each element of this list is a
 * two-element sublist, the first element being the untransformed function
 * call tree, and the second element being a possibly-empty list of ColumnDef
 * nodes representing any columndef list attached to that function within the
 * ROWS FROM() syntax.
 *
 * alias and coldeflist represent any alias and/or columndef list attached
 * at the top level.  (We disallow coldeflist appearing both here and
 * per-function, but that's checked in parse analysis, not by the grammar.)
 */
export interface RangeFunction extends Node<NodeTag.T_RangeFunction>
{
	lateral: boolean;		/* does it have LATERAL prefix? */
	ordinality: boolean;		/* does it have WITH ORDINALITY suffix? */
	is_rowsfrom: boolean;	/* is result of ROWS FROM() syntax? */
	functions: unknown[];		/* per-function information, see above */
	alias: Alias;			/* table alias & optional column aliases */
	coldeflist: unknown[];		/* list of ColumnDef nodes to describe result
								 * of function returning RECORD */
}

/*
 * RangeTableFunc - raw form of "table functions" such as XMLTABLE
 */
export interface RangeTableFunc extends Node<NodeTag.T_RangeTableFunc>
{
	lateral: boolean;		/* does it have LATERAL prefix? */
	docexpr: unknown; 		/* document expression */
	rowexpr: unknown; 		/* row generator expression */
	namespaces: unknown[];	/* list of namespaces as ResTarget */
	columns: unknown[];		/* list of RangeTableFuncCol */
	alias: Alias; 			/* table alias & optional column aliases */
	location: number; 		/* token location, or -1 if unknown */
}

/*
 * RangeTableFuncCol - one column in a RangeTableFunc->columns
 *
 * If for_ordinality is true (FOR ORDINALITY), then the column is an int4
 * column and the rest of the fields are ignored.
 */
export interface RangeTableFuncCol extends Node<NodeTag.T_RangeTableFuncCol>
{
	colname: string;			/* name of generated column */
	typeName: TypeName;			/* type of generated column */
	for_ordinality: boolean;	/* does it have FOR ORDINALITY? */
	is_not_null: boolean;		/* does it have NOT NULL? */
	colexpr: unknown;			/* column filter expression */
	coldefexpr: unknown;		/* column default value expression */
	location: number;			/* token location, or -1 if unknown */
}

/*
 * RangeTableSample - TABLESAMPLE appearing in a raw FROM clause
 *
 * This node, appearing only in raw parse trees, represents
 *		<relation> TABLESAMPLE <method> (<params>) REPEATABLE (<num>)
 * Currently, the <relation> can only be a RangeVar, but we might in future
 * allow RangeSubselect and other options.  Note that the RangeTableSample
 * is wrapped around the node representing the <relation>, rather than being
 * a subfield of it.
 */
export interface RangeTableSample extends Node<NodeTag.T_RangeTableSample>
{
	relation: unknown;		/* relation to be sampled */
	method: unknown[];			/* sampling method name (possibly qualified) */
	args: unknown[];			/* argument(s) for sampling method */
	repeatable: unknown;		/* REPEATABLE expression, or NULL if none */
	location: number;		/* method name location, or -1 if unknown */
}

/*
 * ColumnDef - column definition (used in various creates)
 *
 * If the column has a default value, we may have the value expression
 * in either "raw" form (an untransformed parse tree) or "cooked" form
 * (a post-parse-analysis, executable expression tree), depending on
 * how this ColumnDef node was created (by parsing, or by inheritance
 * from an existing relation).  We should never have both in the same node!
 *
 * Similarly, we may have a COLLATE specification in either raw form
 * (represented as a CollateClause with arg==NULL) or cooked form
 * (the collation's OID).
 *
 * The constraints list may contain a CONSTR_DEFAULT item in a raw
 * parsetree produced by gram.y, but transformCreateStmt will remove
 * the item and set raw_default instead.  CONSTR_DEFAULT items
 * should not appear in any subsequent processing.
 */
export interface ColumnDef extends Node<NodeTag.T_ColumnDef>
{
	colname: string	  ;		/* name of column */
	typeName: TypeName  ;		/* type of column */
	inhcount: number;		/* number of times column is inherited */
	is_local: boolean;		/* column has local (non-inherited) def'n */
	is_not_null: boolean;	/* NOT NULL constraint specified? */
	is_from_type: boolean;	/* column definition came from table type */
	storage: string;		/* attstorage setting, or 0 for default */
	raw_default: unknown	  ;	/* default value (untransformed parse tree) */
	cooked_default: unknown	  ; /* default value (transformed expr tree) */
	identity: string;		/* attidentity setting */
	identitySequence: RangeVar  ;	/* to store identity sequence name for
									 * ALTER TABLE ... ADD COLUMN */
	generated: string;		/* attgenerated setting */
	collClause: CollateClause;	/* untransformed COLLATE spec, if any */
	collOid: Oid;		/* collation OID (InvalidOid if not set) */
	constraints: unknown[]	  ;	/* other constraints on column */
	fdwoptions: unknown[]	  ;		/* per-column FDW options */
	location: number;		/* parse location, or -1 if none/unknown */
}

/*
 * TableLikeClause - CREATE TABLE ( ... LIKE ... ) clause
 */
export interface TableLikeClause extends Node<NodeTag.T_TableLikeClause>
{
	relation: RangeVar;
	options: number;		/* OR of TableLikeOption flags */
}

export enum TableLikeOption
{
	CREATE_TABLE_LIKE_COMMENTS = 1 << 0,
	CREATE_TABLE_LIKE_CONSTRAINTS = 1 << 1,
	CREATE_TABLE_LIKE_DEFAULTS = 1 << 2,
	CREATE_TABLE_LIKE_GENERATED = 1 << 3,
	CREATE_TABLE_LIKE_IDENTITY = 1 << 4,
	CREATE_TABLE_LIKE_INDEXES = 1 << 5,
	CREATE_TABLE_LIKE_STATISTICS = 1 << 6,
	CREATE_TABLE_LIKE_STORAGE = 1 << 7,
	CREATE_TABLE_LIKE_ALL = Number.MAX_SAFE_INTEGER
}

/*
 * IndexElem - index parameters (used in CREATE INDEX, and in ON CONFLICT)
 *
 * For a plain index attribute, 'name' is the name of the table column to
 * index, and 'expr' is NULL.  For an index expression, 'name' is NULL and
 * 'expr' is the expression tree.
 */
export interface IndexElem extends Node<NodeTag.T_IndexElem>
{
	name: string	   ;			/* name of attribute to index, or NULL */
	expr: unknown	   ;			/* expression to index, or NULL */
	indexcolname: string	   ;	/* name for index column; NULL = default */
	collation: unknown[]	   ;		/* name of collation; NIL = default */
	opclass: unknown[]	   ;		/* name of desired opclass; NIL = default */
	ordering: SortByDir;		/* ASC/DESC/default */
	nulls_ordering: SortByNulls; /* FIRST/LAST/default */
}

/*
 * WithClause -
 *	   representation of WITH clause
 *
 * Note: WithClause does not propagate into the Query representation;
 * but CommonTableExpr does.
 */
export interface WithClause extends Node<NodeTag.T_WithClause>
{
	ctes: unknown[];			/* list of CommonTableExprs */
	recursive: boolean;		/* true = WITH RECURSIVE */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * InferClause -
 *		ON CONFLICT unique index inference clause
 *
 * Note: InferClause does not propagate into the Query representation.
 */
export interface InferClause extends Node<NodeTag.T_InferClause>
{
	indexElems: unknown[];		/* IndexElems to infer unique index */
	whereClause: Expr;	/* qualification (partial-index predicate) */
	conname: string;		/* Constraint name, or NULL if unnamed */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * OnConflictClause -
 *		representation of ON CONFLICT clause
 *
 * Note: OnConflictClause does not propagate into the Query representation.
 */
export interface OnConflictClause extends Node<NodeTag.T_OnConflictClause>
{
	action: OnConflictAction;	/* DO NOTHING or UPDATE? */
	infer: InferClause;			/* Optional index inference clause */
	targetList: ResTarget[];		/* the target list (of ResTarget) */
	whereClause: unknown;	/* qualifications */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * CommonTableExpr -
 *	   representation of WITH list element
 *
 * We don't currently support the SEARCH or CYCLE clause.
 */
export enum CTEMaterialize
{
	CTEMaterializeDefault,		/* no option specified */
	CTEMaterializeAlways,		/* MATERIALIZED */
	CTEMaterializeNever			/* NOT MATERIALIZED */
}

export interface CommonTableExpr extends Node<NodeTag.T_CommonTableExpr>
{
	ctename: string;		/* query name (never qualified) */
	aliascolnames: unknown[];	/* optional list of column names */
	ctematerialized: CTEMaterialize; /* is this an optimization fence? */
	/* SelectStmt/InsertStmt/etc before parse analysis, Query afterwards: */
	ctequery: unknown;		/* the CTE's subquery */
	location: number;		/* token location, or -1 if unknown */
	/* These fields are set during parse analysis: */
	cterecursive: boolean;	/* is this CTE actually recursive? */
	cterefcount: number;	/* number of RTEs referencing this CTE
								 * (excluding internal self-references) */
	ctecolnames: unknown[];	/* list of output column names */
	ctecoltypes: unknown[];	/* OID list of output column type OIDs */
	ctecoltypmods: unknown[];	/* integer list of output column typmods */
	ctecolcollations: unknown[];	/* OID list of column collation OIDs */
}

export enum SetOperation
{
	SETOP_NONE = 0,
	SETOP_UNION,
	SETOP_INTERSECT,
	SETOP_EXCEPT
}

/*
 * GroupingSet -
 *		representation of CUBE, ROLLUP and GROUPING SETS clauses
 *
 * In a Query with grouping sets, the groupClause contains a flat list of
 * SortGroupClause nodes for each distinct expression used.  The actual
 * structure of the GROUP BY clause is given by the groupingSets tree.
 *
 * In the raw parser output, GroupingSet nodes (of all types except SIMPLE
 * which is not used) are potentially mixed in with the expressions in the
 * groupClause of the SelectStmt.  (An expression can't contain a GroupingSet,
 * but a list may mix GroupingSet and expression nodes.)  At this stage, the
 * content of each node is a list of expressions, some of which may be RowExprs
 * which represent sublists rather than actual row constructors, and nested
 * GroupingSet nodes where legal in the grammar.  The structure directly
 * reflects the query syntax.
 *
 * In parse analysis, the transformed expressions are used to build the tlist
 * and groupClause list (of SortGroupClause nodes), and the groupingSets tree
 * is eventually reduced to a fixed format:
 *
 * EMPTY nodes represent (), and obviously have no content
 *
 * SIMPLE nodes represent a list of one or more expressions to be treated as an
 * atom by the enclosing structure; the content is an integer list of
 * ressortgroupref values (see SortGroupClause)
 *
 * CUBE and ROLLUP nodes contain a list of one or more SIMPLE nodes.
 *
 * SETS nodes contain a list of EMPTY, SIMPLE, CUBE or ROLLUP nodes, but after
 * parse analysis they cannot contain more SETS nodes; enough of the syntactic
 * transforms of the spec have been applied that we no longer have arbitrarily
 * deep nesting (though we still preserve the use of cube/rollup).
 *
 * Note that if the groupingSets tree contains no SIMPLE nodes (only EMPTY
 * nodes at the leaves), then the groupClause will be empty, but this is still
 * an aggregation query (similar to using aggs or HAVING without GROUP BY).
 *
 * As an example, the following clause:
 *
 * GROUP BY GROUPING SETS ((a,b), CUBE(c,(d,e)))
 *
 * looks like this after raw parsing:
 *
 * SETS( RowExpr(a,b) , CUBE( c, RowExpr(d,e) ) )
 *
 * and parse analysis converts it to:
 *
 * SETS( SIMPLE(1,2), CUBE( SIMPLE(3), SIMPLE(4,5) ) )
 */
export enum GroupingSetKind
{
	GROUPING_SET_EMPTY,
	GROUPING_SET_SIMPLE,
	GROUPING_SET_ROLLUP,
	GROUPING_SET_CUBE,
	GROUPING_SET_SETS
}

export interface GroupingSet extends Node<NodeTag.T_GroupingSet>
{
	kind: GroupingSetKind;
	content: Expr[];
	location: number;
}


/* ----------------------
 *		Insert Statement
 *
 * The source expression is represented by SelectStmt for both the
 * SELECT and VALUES cases.  If selectStmt is NULL, then the query
 * is INSERT ... DEFAULT VALUES.
 * ----------------------
 */
export interface InsertStmt extends Node<NodeTag.T_InsertStmt>
{
	relation: RangeVar;		/* relation to insert into */
	cols: unknown[];			/* optional: names of the target columns */
	selectStmt: SelectStmt | null;		/* the source SELECT/VALUES, or NULL */
	onConflictClause?: OnConflictClause | null; /* ON CONFLICT clause */
	returningList?: unknown[] | null;	/* list of expressions to return */
	withClause?: WithClause | null;		/* WITH clause */
	override: OverridingKind;	/* OVERRIDING clause */
}

/* ----------------------
 *		Delete Statement
 * ----------------------
 */
export interface DeleteStmt extends Node<NodeTag.T_DeleteStmt>
{
	relation: RangeVar;		/* relation to delete from */
	usingClause?: unknown[] | null;	/* optional using clause for more tables */
	whereClause?: unknown | null;	/* qualifications */
	returningList?: unknown[] | null;	/* list of expressions to return */
	withClause?: WithClause | null;		/* WITH clause */
}

/* ----------------------
 *		Update Statement
 * ----------------------
 */
export interface UpdateStmt extends Node<NodeTag.T_UpdateStmt>
{
	relation: RangeVar;		/* relation to update */
	targetList: unknown[];		/* the target list (of ResTarget) */
	whereClause: unknown;	/* qualifications */
	fromClause: unknown[];		/* optional from clause for more tables */
	returningList: unknown[];	/* list of expressions to return */
	withClause: WithClause;		/* WITH clause */
}

export interface BaseSelectStmt extends Node<NodeTag.T_SelectStmt> {
	/*
	 * These fields are used in both "leaf" SelectStmts and upper-level
	 * SelectStmts.
	 */
	sortClause?: unknown[];		/* sort clause (a list of SortBy's) */
	limitOffset?: unknown;	/* # of result tuples to skip */
	limitCount?: unknown;		/* # of result tuples to return */
	lockingClause?: unknown[];	/* FOR UPDATE (list of LockingClause's) */
	withClause?: WithClause;		/* WITH clause */
}

interface LeafSelectStmt extends BaseSelectStmt {
	/*
	 * These fields are used only in "leaf" SelectStmts.
	 */
	distinctClause?: unknown[];  /* NULL, list of DISTINCT ON exprs, or
								 * lcons(NIL,NIL) for all (SELECT DISTINCT) */
	intoClause?: IntoClause;		/* target for SELECT INTO */
	targetList: ResTarget[];	/* the target list (of ResTarget) */
	fromClause: Array<RangeVar | SelectStmt>;		/* the FROM clause */
	whereClause?: Expr;	/* WHERE qualification */
	groupClause?: unknown[];	/* GROUP BY clauses */
	havingClause?: unknown;	/* HAVING conditional-expression */
	windowClause?: unknown[];	/* WINDOW window_name AS (...), ... */

	/*
	 * In a "leaf" node representing a VALUES list, the above fields are all
	 * null, and instead this field is set.  Note that the elements of the
	 * sublists are just expressions, without ResTarget decoration. Also note
	 * that a list element can be DEFAULT (represented as a SetToDefault
	 * node), regardless of the context of the VALUES list. It's up to parse
	 * analysis to reject that where not valid.
	 */
	valuesLists: unknown[];	/* untransformed list of expression lists */
}

interface UpperLevelSelectStmt extends BaseSelectStmt {
	/*
	 * These fields are used only in upper-level SelectStmts.
	 */
	op: SetOperation;			/* type of set op */
	all: boolean;			/* ALL specified? */
	larg?: SelectStmt;	/* left child */
	rarg?: SelectStmt;	/* right child */
	/* Eventually add fields for CORRESPONDING spec here */
}

export type SelectStmt = LeafSelectStmt | UpperLevelSelectStmt;

/* ----------------------
 *		Declare Cursor Statement
 *
 * The "query" field is initially a raw parse tree, and is converted to a
 * Query node during parse analysis.  Note that rewriting and planning
 * of the query are always postponed until execution.
 * ----------------------
 */
export const CURSOR_OPT_BINARY =		0x0001	/* BINARY */
export const CURSOR_OPT_SCROLL =		0x0002	/* SCROLL explicitly given */
export const CURSOR_OPT_NO_SCROLL =		0x0004	/* NO SCROLL explicitly given */
export const CURSOR_OPT_INSENSITIVE =	0x0008	/* INSENSITIVE */
export const CURSOR_OPT_HOLD =			0x0010	/* WITH HOLD */
/* these planner-control flags do not correspond to any SQL grammar: */
export const CURSOR_OPT_FAST_PLAN =		0x0020	/* prefer fast-start plan */
export const CURSOR_OPT_GENERIC_PLAN =	0x0040	/* force use of generic plan */
export const CURSOR_OPT_CUSTOM_PLAN =	0x0080	/* force use of custom plan */
export const CURSOR_OPT_PARALLEL_OK =	0x0100	/* parallel mode OK */

export interface DeclareCursorStmt extends Node<NodeTag.T_DeclareCursorStmt>
{
	portalname: string;		/* name of the portal (cursor) */
	options: number;		/* bitmask of options (see above) */
	query: SelectStmt;			/* the query (see comments above) */
}

export enum OverridingKind
{
	OVERRIDING_NOT_SET,
	OVERRIDING_USER_VALUE,
	OVERRIDING_SYSTEM_VALUE
}

/* ----------------------
 *		{Begin|Commit|Rollback} Transaction Statement
 * ----------------------
 */
export enum TransactionStmtKind
{
	TRANS_STMT_BEGIN,
	TRANS_STMT_START,			/* semantically identical to BEGIN */
	TRANS_STMT_COMMIT,
	TRANS_STMT_ROLLBACK,
	TRANS_STMT_SAVEPOINT,
	TRANS_STMT_RELEASE,
	TRANS_STMT_ROLLBACK_TO,
	TRANS_STMT_PREPARE,
	TRANS_STMT_COMMIT_PREPARED,
	TRANS_STMT_ROLLBACK_PREPARED
}

export interface TransactionStmt extends Node<NodeTag.T_TransactionStmt>
{
	kind: TransactionStmtKind;	/* see above */
	options: unknown;		/* for BEGIN/START commands */
	savepoint_name: string; /* for savepoint commands */
	gid: string;			/* for two-phase-commit related commands */
	chain: boolean;			/* AND CHAIN option */
}

/*
 * DefElem - a generic "name = value" option definition
 *
 * In some contexts the name can be qualified.  Also, certain SQL commands
 * allow a SET/ADD/DROP action to be attached to option settings, so it's
 * convenient to carry a field for that too.  (Note: currently, it is our
 * practice that the grammar allows namespace and action only in statements
 * where they are relevant; C code can just ignore those fields in other
 * statements.)
 */
export enum DefElemAction
{
	DEFELEM_UNSPEC,				/* no action given */
	DEFELEM_SET,
	DEFELEM_ADD,
	DEFELEM_DROP
} DefElemAction;

export interface DefElem extends Node<NodeTag.T_DefElem>
{
	defnamespace: string | null;	/* NULL if unqualified name */
	defname: string;
	arg: Value<any> | TypeName;			/* a (Value *) or a (TypeName *) */
	defaction: DefElemAction;	/* unspecified action, or SET/ADD/DROP */
	location: number;		/* token location, or -1 if unknown */
}
