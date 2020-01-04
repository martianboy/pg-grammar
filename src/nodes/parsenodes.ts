import { makeString, Value } from "./value";
import { NodeTag } from './tags';
import { Node, OnConflictAction } from './node';
import { IntoClause, Expr, RangeVar } from "./primnodes";

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
	typmods: Node<any>[];		/* type modifier expression(s) */
	typemod: number;		/* prespecified type modifier */
	arrayBounds?: Node<any>[];	/* array bounds */
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
	arg: Node<any>;			/* input expression */
	collname: Value<NodeTag.T_String>;		/* possibly-qualified collation name */
	location: number;		/* token location, or -1 if unknown */
};

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
	lidx: Node<any>;			/* slice lower bound, if any */
	uidx: Node<any>;			/* subscript, or slice upper bound if any */
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
	arg: Node<any>;			        /* the thing being selected from */
	indirection: unknown[];	/* subscripts and/or field names and/or * */
}

/*
 * A_ArrayExpr - an ARRAY[] construct
 */
export interface A_ArrayExpr extends Node<NodeTag.T_ArrayExpr>
{
	elements: Node<any>[];		/* array element expressions */
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
	whereClause: A_Expr;	/* qualification (partial-index predicate) */
	conname: string;		/* Constraint name, or NULL if unnamed */
	location: number;		/* token location, or -1 if unknown */
};

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
	content: A_Expr[];
	location: number;
}


interface BaseSelectStmt extends Node<NodeTag.T_SelectStmt> {
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
	whereClause?: A_Expr;	/* WHERE qualification */
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

export function makeColumnRef(colname: string, indirection: any[], location: number, yyscanner: unknown) {
  /*
   * Generate a ColumnRef node, with an A_Indirection node added if there
   * is any subscripting in the specified indirection list.  However,
   * any field selection at the start of the indirection list must be
   * transposed into the "fields" part of the ColumnRef node.
   */
  const c: ColumnRef = {
    type: NodeTag.T_ColumnRef,
    location,
    fields: Array.isArray(indirection) ? indirection : []
  };
  let nfields = 0;

  if (Array.isArray(indirection)) {
    // for(const l of indirection) {
    //     if (IsA(lfirst(l), A_Indices))
    //     {
    //         A_Indirection *i = makeNode(A_Indirection);
    //         if (nfields == 0)
    //         {
    //             /* easy case - all indirection goes to A_Indirection */
    //             c.fields = list_make1(makeString(colname));
    //             i.indirection = check_indirection(indirection, yyscanner);
    //         }
    //         else
    //         {
    //             /* got to split the list in two */
    //             i.indirection = check_indirection(list_copy_tail(indirection,
    //                                                             nfields),
    //                                             yyscanner);
    //             indirection = list_truncate(indirection, nfields);
    //             c.fields = lcons(makeString(colname), indirection);
    //         }
    //         i.arg = (Node *) c;
    //         return (Node *) i;
    //     }
    //     else if (IsA(lfirst(l), A_Star))
    //     {
    //         /* We only allow '*' at the end of a ColumnRef */
    //         if (lnext(indirection, l) != NULL)
    //             parser_yyerror("improper use of \"*\"");
    //     }
    //     nfields++;
    // }
  }

  /* No subscripting, so all indirection gets added to field list */
  c.fields.unshift(makeString(colname));

  return c;
}
