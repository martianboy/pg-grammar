import { Node, JoinType, OnConflictAction } from "./node";
import { NodeTag } from "./tags";
import { Value } from "./value";
import { A_Expr, A_Const } from "./parsenodes";


/* ----------------------------------------------------------------
 *					node types for executable expressions
 * ----------------------------------------------------------------
 */

/*
 * Expr - generic superclass for executable-expression nodes
 *
 * All node types that are used in executable expression trees should derive
 * from Expr (that is, have Expr as their first field).  Since Expr only
 * contains NodeTag, this is a formality, but it is an easy form of
 * documentation.  See also the ExprState node types in execnodes.h.
 */
export type Expr =
	| A_Const
	| A_Expr
	| Var
	| Const
	| RangeVar
	| TableFunc
	| Param
	| BoolExpr
	| InferenceElem
	| CaseExpr
	| CaseTestExpr
	| ArrayExpr
	| RowExpr
	| RowCompareExpr
	| CoalesceExpr
	| MinMaxExpr
	| SQLValueFunction
	| SubscriptingRef
	| FuncExpr
	| NamedArgExpr
	| OpExpr
	| ScalarArrayOpExpr
	| XmlExpr
	| SubLink
	| NullTest
	| BooleanTest
	;

/*
 * Var - expression node representing a variable (ie, a table column)
 *
 * Note: during parsing/planning, varnoold/varoattno are always just copies
 * of varno/varattno.  At the tail end of planning, Var nodes appearing in
 * upper-level plan nodes are reassigned to point to the outputs of their
 * subplans; for example, in a join node varno becomes INNER_VAR or OUTER_VAR
 * and varattno becomes the index of the proper element of that subplan's
 * target list.  Similarly, INDEX_VAR is used to identify Vars that reference
 * an index column rather than a heap column.  (In ForeignScan and CustomScan
 * plan nodes, INDEX_VAR is abused to signify references to columns of a
 * custom scan tuple type.)  In all these cases, varnoold/varoattno hold the
 * original values.  The code doesn't really need varnoold/varoattno, but they
 * are very useful for debugging and interpreting completed plans, so we keep
 * them around.
 */
export const INNER_VAR = 65000;	/* reference to inner subplan */
export const OUTER_VAR = 65001;	/* reference to outer subplan */
export const INDEX_VAR = 65002;	/* reference to index column */

export const IS_SPECIAL_VARNO = (varno: number) => varno >= INNER_VAR;

/* Symbols for the indexes of the special RTE entries in rules */
export const PRS2_OLD_VARNO = 1;
export const PRS2_NEW_VARNO = 2;

export type Oid = number;
export type Index = number;
export type AttrNumber = number;

export interface Var extends Node<NodeTag.T_Var>
{
	varno: Index;			/* index of this var's relation in the range
							 * table, or INNER_VAR/OUTER_VAR/INDEX_VAR */
	varattno: AttrNumber;	/* attribute number of this var, or zero for
							 * all attrs ("whole-row Var") */
	vartype: number;		/* pg_type OID for the type of this var */
	vartypmod: number;		/* pg_attribute typmod value */
	varcollid: number;		/* OID of collation, or Invalidif: Oid none */
	varlevelsup: Index		/* for subquery variables referencing outer
							 * relations; 0 in a normal var, >0 means N
							 * levels up */
	varnoold: Index			/* original value of varno, for debugging */
	varoattno: AttrNumber	/* original value of varattno */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * Const
 *
 * Note: for varlena data types, we make a rule that a Const node's value
 * must be in non-extended form (4-byte header, no compression or external
 * references).  This ensures that the Const node is self-contained and makes
 * it more likely that equal() will see logically identical values as equal.
 */
export interface Const extends Node<NodeTag.T_Const>
{
	consttype: number;		/* pg_type OID of the constant's datatype */
	consttypmod: number;	/* typmod value, if any */
	constcollid: number;	/* OID of collation, or Invalidif: Oid none */
	constlen: number;		/* typlen of the constant's datatype */
	constvalue: unknown;	/* the constant's value */
	constisnull: boolean; 	/* whether the constant is null (if true,
							 * constvalue is undefined) */
	constbyval: boolean;	/* whether this datatype is passed by value.
							 * If true, then all the information is stored
							 * in the Datum. If false, then the Datum
							 * contains a pointer to the information. */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * Alias -
 *	  specifies an alias for a range variable; the alias might also
 *	  specify renaming of columns within the table.
 *
 * Note: colnames is a list of Value nodes (always strings).  In Alias structs
 * associated with RTEs, there may be entries corresponding to dropped
 * columns; these are normally empty strings ("").  See parsenodes.h for info.
 */
export interface Alias extends Node<NodeTag.T_Alias> {
	aliasname: string;		/* aliased rel name (never qualified) */
	colnames: Value<NodeTag.T_String>[];		/* optional list of column aliases */
}

enum OnCommitAction
{
	ONCOMMIT_NOOP,				/* No ON COMMIT clause (do nothing) */
	ONCOMMIT_PRESERVE_ROWS,		/* ON COMMIT PRESERVE ROWS (do nothing) */
	ONCOMMIT_DELETE_ROWS,		/* ON COMMIT DELETE ROWS */
	ONCOMMIT_DROP				/* ON COMMIT DROP */
}

/*
 * RangeVar - range variable, used in FROM clauses
 *
 * Also used to represent table names in utility statements; there, the alias
 * field is not used, and inh tells whether to apply the operation
 * recursively to child tables.  In some contexts it is also useful to carry
 * a TEMP table indication here.
 */
export interface RangeVar extends Node<NodeTag.T_RangeVar> {
	catalogname: string | null;	/* the catalog (database) name, or NULL */
	schemaname: string;		/* the schema name, or NULL */
	relname: string;		/* the relation/sequence name */
	inh: boolean;			/* expand rel by inheritance? recursively act
								 * on children? */
	relpersistence: string; /* see RELPERSISTENCE_* in pg_class.h */
	alias: Alias | null;			/* table alias & optional column aliases */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * TableFunc - node for a table function, such as XMLTABLE.
 *
 * Entries in the ns_names list are either string Value nodes containing
 * literal namespace names, or NULL pointers to represent DEFAULT.
 */
export interface TableFunc extends Node<NodeTag.T_TableFunc>
{
	ns_uris: unknown[];			/* list of namespace URI expressions */
	ns_names: Node<NodeTag.T_String>[];			/* list of namespace names or NULL */
	docexpr: Node<NodeTag.T_String>;			/* input document expression */
	rowexpr: Node<any>;			/* row filter expression */
	colnames: Node<NodeTag.T_String>[];			/* column names (list of String) */
	coltypes: Node<any>[];			/* OID list of column type OIDs */
	coltypmods: Node<any>[];		/* integer list of column typmods */
	colcollations: Node<any>[];	/* OID list of column collation OIDs */
	colexprs: Node<any>[];			/* list of column filter expressions */
	coldefexprs: Node<any>[];		/* list of column default expressions */
	// notnulls: Bitmapset;		/* nullability flag for each output column */
	ordinalitycol: number;		/* counts from 0; -1 if none specified */
	location: number;			/* token location, or -1 if unknown */
}

/*
 * IntoClause - target information for SELECT INTO, CREATE TABLE AS, and
 * CREATE MATERIALIZED VIEW
 *
 * For CREATE MATERIALIZED VIEW, viewQuery is the parsed-but-not-rewritten
 * SELECT Query for the view; otherwise it's NULL.  (Although it's actually
 * Query*, we declare it as Node* to avoid a forward reference.)
 */
export interface IntoClause extends Node<NodeTag.T_IntoClause>
{
	rel: RangeVar;			/* target relation name */
	colNames: Value<NodeTag.T_String>[];		/* column names to assign, or NIL */
	accessMethod: string;	/* table access method */
	options: unknown[];		/* options from WITH clause */
	onCommit: OnCommitAction;	/* what do we do at COMMIT? */
	tableSpaceName: string; /* table space to use, or NULL */
	viewQuery: Node<any>;		/* materialized view's SELECT query */
	skipData: boolean;		/* true for WITH NO DATA */
}


/*
 * Param
 *
 *		paramkind specifies the kind of parameter. The possible values
 *		for this field are:
 *
 *		PARAM_EXTERN:  The parameter value is supplied from outside the plan.
 *				Such parameters are numbered from 1 to n.
 *
 *		PARAM_EXEC:  The parameter is an internal executor parameter, used
 *				for passing values into and out of sub-queries or from
 *				nestloop joins to their inner scans.
 *				For historical reasons, such parameters are numbered from 0.
 *				These numbers are independent of PARAM_EXTERN numbers.
 *
 *		PARAM_SUBLINK:	The parameter represents an output column of a SubLink
 *				node's sub-select.  The column number is contained in the
 *				`paramid' field.  (This type of Param is converted to
 *				PARAM_EXEC during planning.)
 *
 *		PARAM_MULTIEXPR:  Like PARAM_SUBLINK, the parameter represents an
 *				output column of a SubLink node's sub-select, but here, the
 *				SubLink is always a MULTIEXPR SubLink.  The high-order 16 bits
 *				of the `paramid' field contain the SubLink's subLinkId, and
 *				the low-order 16 bits contain the column number.  (This type
 *				of Param is also converted to PARAM_EXEC during planning.)
 */
export enum ParamKind
{
	PARAM_EXTERN,
	PARAM_EXEC,
	PARAM_SUBLINK,
	PARAM_MULTIEXPR
}

export interface Param extends Node<NodeTag.T_Param>
{
	paramkind: ParamKind;	/* kind of parameter. See above */
	paramid: number;		/* numeric ID for parameter */
	paramtype: Oid;			/* pg_type OID of parameter's datatype */
	paramtypmod: number;	/* typmod value, if known */
	paramcollid: Oid;		/* OID of collation, or Invalidif: Oid none */
	location: number;		/* token location, or -1 if unknown */
}


/* ----------------
 *	SubscriptingRef: describes a subscripting operation over a container
 *			(array, etc).
 *
 * A SubscriptingRef can describe fetching a single element from a container,
 * fetching a part of container (e.g. array slice), storing a single element into
 * a container, or storing a slice.  The "store" cases work with an
 * initial container value and a source value that is inserted into the
 * appropriate part of the container; the result of the operation is an
 * entire new modified container value.
 *
 * If reflowerindexpr = NIL, then we are fetching or storing a single container
 * element at the subscripts given by refupperindexpr. Otherwise we are
 * fetching or storing a container slice, that is a rectangular subcontainer
 * with lower and upper bounds given by the index expressions.
 * reflowerindexpr must be the same length as refupperindexpr when it
 * is not NIL.
 *
 * In the slice case, individual expressions in the subscript lists can be
 * NULL, meaning "substitute the array's current lower or upper bound".
 *
 * Note: the result datatype is the element type when fetching a single
 * element; but it is the array type when doing subarray fetch or either
 * type of store.
 *
 * Note: for the cases where a container is returned, if refexpr yields a R/W
 * expanded container, then the implementation is allowed to modify that object
 * in-place and return the same object.)
 * ----------------
 */
export interface SubscriptingRef extends Node<NodeTag.T_SubscriptingRef>
{
	refcontainertype: Oid;	/* type of the container proper */
	refelemtype: Oid;	/* type of the container elements */
	reftypmod: number;		/* typmod of the container (and elements too) */
	refcollid: Oid;		/* OID of collation, or Invalidif: Oid none */
	refupperindexpr: unknown[];	/* expressions that evaluate to upper
									 * container indexes */
	reflowerindexpr: unknown[];	/* expressions that evaluate to lower
									 * container indexes, or NIL for single
									 * container element */
	refexpr: Expr;		/* the expression that evaluates to a
								 * container value */

	refassgnexpr: Expr;	/* expression for the source value, or NULL if
								 * fetch */
}

/*
 * CoercionContext - distinguishes the allowed set of type casts
 *
 * NB: ordering of the alternatives is significant; later (larger) values
 * allow more casts than earlier ones.
 */
export enum CoercionContext
{
	COERCION_IMPLICIT,			/* coercion in context of expression */
	COERCION_ASSIGNMENT,		/* coercion in context of assignment */
	COERCION_EXPLICIT			/* explicit cast operation */
}

/*
 * CoercionForm - how to display a node that could have come from a cast
 *
 * NB: equal() ignores CoercionForm fields, therefore this *must* not carry
 * any semantically significant information.  We need that behavior so that
 * the planner will consider equivalent implicit and explicit casts to be
 * equivalent.  In cases where those actually behave differently, the coercion
 * function's arguments will be different.
 */
export enum CoercionForm
{
	COERCE_EXPLICIT_CALL,		/* display as a function call */
	COERCE_EXPLICIT_CAST,		/* display as an explicit cast */
	COERCE_IMPLICIT_CAST		/* implicit cast, so hide it */
}

/*
 * FuncExpr - expression node for a function call
 */
export interface FuncExpr extends Node<NodeTag.T_FuncExpr>
{
	funcid: Oid;			/* PG_PROC OID of the function */
	funcresulttype: Oid; /* PG_TYPE OID of result value */
	funcretset: boolean;		/* true if function returns set */
	funcvariadic: boolean;	/* true if variadic arguments have been
								 * combined into an array last argument */
	funcformat: CoercionForm;	/* how to display this function call */
	funccollid: Oid;		/* OID of collation of result */
	inputcollid: Oid;	/* OID of collation that function should use */
	args: unknown[];			/* arguments to the function */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * NamedArgExpr - a named argument of a function
 *
 * This node type can only appear in the args list of a FuncCall or FuncExpr
 * node.  We support pure positional call notation (no named arguments),
 * named notation (all arguments are named), and mixed notation (unnamed
 * arguments followed by named ones).
 *
 * Parse analysis sets argnumber to the positional index of the argument,
 * but doesn't rearrange the argument list.
 *
 * The planner will convert argument lists to pure positional notation
 * during expression preprocessing, so execution never sees a NamedArgExpr.
 */
export interface NamedArgExpr extends Node<NodeTag.T_NamedArgExpr>
{
	arg: Expr;				/* the argument expression */
	name: string;			/* the name */
	argnumber: number;		/* argument's number in positional notation */
	location: number;		/* argument name location, or -1 if unknown */
}

/*
 * OpExpr - expression node for an operator invocation
 *
 * Semantically, this is essentially the same as a function call.
 *
 * Note that opfuncid is not necessarily filled in immediately on creation
 * of the node.  The planner makes sure it is valid before passing the node
 * tree to the executor, but during parsing/planning opfuncid can be 0.
 */
export interface OpExpr extends Node<NodeTag.T_OpExpr>
{
	opno: Oid;			/* PG_OPERATOR OID of the operator */
	opfuncid: Oid;		/* PG_PROC OID of underlying function */
	opresulttype: Oid;	/* PG_TYPE OID of result value */
	opretset: boolean;		/* true if operator returns set */
	opcollid: Oid;		/* OID of collation of result */
	inputcollid: Oid;	/* OID of collation that operator should use */
	args: unknown[];			/* arguments to the operator (1 or 2) */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * DistinctExpr - expression node for "x IS DISTINCT FROM y"
 *
 * Except for the nodetag, this is represented identically to an OpExpr
 * referencing the "=" operator for x and y.
 * We use "=", not the more obvious "<>", because more datatypes have "="
 * than "<>".  This means the executor must invert the operator result.
 * Note that the operator function won't be called at all if either input
 * is NULL, since then the result can be determined directly.
 */
export type DistinctExpr = OpExpr;

/*
 * NullIfExpr - a NULLIF expression
 *
 * Like DistinctExpr, this is represented the same as an OpExpr referencing
 * the "=" operator for x and y.
 */
export type NullIfExpr = OpExpr;

/*
 * ScalarArrayOpExpr - expression node for "scalar op ANY/ALL (array)"
 *
 * The operator must yield boolean.  It is applied to the left operand
 * and each element of the righthand array, and the results are combined
 * with OR or AND (for ANY or ALL respectively).  The node representation
 * is almost the same as for the underlying operator, but we need a useOr
 * flag to remember whether it's ANY or ALL, and we don't have to store
 * the result type (or the collation) because it must be boolean.
 */
export interface ScalarArrayOpExpr extends Node<NodeTag.T_ScalarArrayOpExpr>
{
	opno: Oid;			/* PG_OPERATOR OID of the operator */
	opfuncid: Oid;		/* PG_PROC OID of underlying function */
	useOr: boolean;			/* true for ANY, false for ALL */
	inputcollid: Oid;	/* OID of collation that operator should use */
	args: unknown[];			/* the scalar and array operands */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * BoolExpr - expression node for the basic Boolean operators AND, OR, NOT
 *
 * Notice the arguments are given as a List.  For NOT, of course the list
 * must always have exactly one element.  For AND and OR, there can be two
 * or more arguments.
 */
export enum BoolExprType
{
	AND_EXPR, OR_EXPR, NOT_EXPR
}

export interface BoolExpr extends Node<NodeTag.T_BoolExpr>
{
	boolop: BoolExprType;
	args: any[];			/* arguments to this expression */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * SubLink
 *
 * A SubLink represents a subselect appearing in an expression, and in some
 * cases also the combining operator(s) just above it.  The subLinkType
 * indicates the form of the expression represented:
 *	EXISTS_SUBLINK		EXISTS(SELECT ...)
 *	ALL_SUBLINK			(lefthand) op ALL (SELECT ...)
 *	ANY_SUBLINK			(lefthand) op ANY (SELECT ...)
 *	ROWCOMPARE_SUBLINK	(lefthand) op (SELECT ...)
 *	EXPR_SUBLINK		(SELECT with single targetlist item ...)
 *	MULTIEXPR_SUBLINK	(SELECT with multiple targetlist items ...)
 *	ARRAY_SUBLINK		ARRAY(SELECT with single targetlist item ...)
 *	CTE_SUBLINK			WITH query (never actually part of an expression)
 * For ALL, ANY, and ROWCOMPARE, the lefthand is a list of expressions of the
 * same length as the subselect's targetlist.  ROWCOMPARE will *always* have
 * a list with more than one entry; if the subselect has just one target
 * then the parser will create an EXPR_SUBLINK instead (and any operator
 * above the subselect will be represented separately).
 * ROWCOMPARE, EXPR, and MULTIEXPR require the subselect to deliver at most
 * one row (if it returns no rows, the result is NULL).
 * ALL, ANY, and ROWCOMPARE require the combining operators to deliver boolean
 * results.  ALL and ANY combine the per-row results using AND and OR
 * semantics respectively.
 * ARRAY requires just one target column, and creates an array of the target
 * column's type using any number of rows resulting from the subselect.
 *
 * SubLink is classed as an Expr node, but it is not actually executable;
 * it must be replaced in the expression tree by a SubPlan node during
 * planning.
 *
 * NOTE: in the raw output of gram.y, testexpr contains just the raw form
 * of the lefthand expression (if any), and operName is the String name of
 * the combining operator.  Also, subselect is a raw parsetree.  During parse
 * analysis, the parser transforms testexpr into a complete boolean expression
 * that compares the lefthand value(s) to PARAM_SUBLINK nodes representing the
 * output columns of the subselect.  And subselect is transformed to a Query.
 * This is the representation seen in saved rules and in the rewriter.
 *
 * In EXISTS, EXPR, MULTIEXPR, and ARRAY SubLinks, testexpr and operName
 * are unused and are always null.
 *
 * subLinkId is currently used only for MULTIEXPR SubLinks, and is zero in
 * other SubLinks.  This number identifies different multiple-assignment
 * subqueries within an UPDATE statement's SET list.  It is unique only
 * within a particular targetlist.  The output column(s) of the MULTIEXPR
 * are referenced by PARAM_MULTIEXPR Params appearing elsewhere in the tlist.
 *
 * The CTE_SUBLINK case never occurs in actual SubLink nodes, but it is used
 * in SubPlans generated for WITH subqueries.
 */
export enum SubLinkType
{
	EXISTS_SUBLINK,
	ALL_SUBLINK,
	ANY_SUBLINK,
	ROWCOMPARE_SUBLINK,
	EXPR_SUBLINK,
	MULTIEXPR_SUBLINK,
	ARRAY_SUBLINK,
	CTE_SUBLINK					/* for SubPlans only */
}

export interface SubLink extends Node<NodeTag.T_SubLink>
{
	subLinkType: SubLinkType;	/* see above */
	subLinkId: number;		/* ID (1..n); 0 if not MULTIEXPR */
	testexpr: unknown;		/* outer-query test for ALL/ANY/ROWCOMPARE */
	operName: unknown[];		/* originally specified operator name */
	subselect: unknown;		/* subselect as Query* or raw parsetree */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * InferenceElem - an element of a unique index inference specification
 *
 * This mostly matches the structure of IndexElems, but having a dedicated
 * primnode allows for a clean separation between the use of index parameters
 * by utility commands, and this node.
 */
export interface InferenceElem extends Node<NodeTag.T_InferenceElem>
{
	expr: Expr;			/* expression to infer from, or NULL */
	infercollid: number;	/* OID of collation, or InvalidOid */
	inferopclass: number;	/* OID of att opclass, or InvalidOid */
}

/*--------------------
 * TargetEntry -
 *	   a target entry (used in query target lists)
 *
 * Strictly speaking, a TargetEntry isn't an expression node (since it can't
 * be evaluated by ExecEvalExpr).  But we treat it as one anyway, since in
 * very many places it's convenient to process a whole query targetlist as a
 * single expression tree.
 *
 * In a SELECT's targetlist, resno should always be equal to the item's
 * ordinal position (counting from 1).  However, in an INSERT or UPDATE
 * targetlist, resno represents the attribute number of the destination
 * column for the item; so there may be missing or out-of-order resnos.
 * It is even legal to have duplicated resnos; consider
 *		UPDATE table SET arraycol[1] = ..., arraycol[2] = ..., ...
 * The two meanings come together in the executor, because the planner
 * transforms INSERT/UPDATE tlists into a normalized form with exactly
 * one entry for each column of the destination table.  Before that's
 * happened, however, it is risky to assume that resno == position.
 * Generally get_tle_by_resno() should be used rather than list_nth()
 * to fetch tlist entries by resno, and only in SELECT should you assume
 * that resno is a unique identifier.
 *
 * resname is required to represent the correct column name in non-resjunk
 * entries of top-level SELECT targetlists, since it will be used as the
 * column title sent to the frontend.  In most other contexts it is only
 * a debugging aid, and may be wrong or even NULL.  (In particular, it may
 * be wrong in a tlist from a stored rule, if the referenced column has been
 * renamed by ALTER TABLE since the rule was made.  Also, the planner tends
 * to store NULL rather than look up a valid name for tlist entries in
 * non-toplevel plan nodes.)  In resjunk entries, resname should be either
 * a specific system-generated name (such as "ctid") or NULL; anything else
 * risks confusing ExecGetJunkAttribute!
 *
 * ressortgroupref is used in the representation of ORDER BY, GROUP BY, and
 * DISTINCT items.  Targetlist entries with ressortgroupref=0 are not
 * sort/group items.  If ressortgroupref>0, then this item is an ORDER BY,
 * GROUP BY, and/or DISTINCT target value.  No two entries in a targetlist
 * may have the same nonzero ressortgroupref --- but there is no particular
 * meaning to the nonzero values, except as tags.  (For example, one must
 * not assume that lower ressortgroupref means a more significant sort key.)
 * The order of the associated SortGroupClause lists determine the semantics.
 *
 * resorigtbl/resorigcol identify the source of the column, if it is a
 * simple reference to a column of a base table (or view).  If it is not
 * a simple reference, these fields are zeroes.
 *
 * If resjunk is true then the column is a working column (such as a sort key)
 * that should be removed from the final output of the query.  Resjunk columns
 * must have resnos that cannot duplicate any regular column's resno.  Also
 * note that there are places that assume resjunk columns come after non-junk
 * columns.
 *--------------------
 */
export interface TargetEntry extends Node<NodeTag.T_TargetEntry>
{
	expr: Expr;			/* expression to evaluate */
	resno: AttrNumber;			/* attribute number (see notes above) */
	resname: string;		/* name of the column (could be NULL) */
	ressortgroupref: Index;	/* nonzero if referenced by a sort/group
									 * clause */
	resorigtbl: number;		/* OID of column's source table */
	resorigcol: AttrNumber;		/* column's number in source table */
	resjunk: boolean;		/* set to true to eliminate the attribute from
								 * final target list */
}


/* ----------------------------------------------------------------
 *					node types for join trees
 *
 * The leaves of a join tree structure are RangeTblRef nodes.  Above
 * these, JoinExpr nodes can appear to denote a specific kind of join
 * or qualified join.  Also, FromExpr nodes can appear to denote an
 * ordinary cross-product join ("FROM foo, bar, baz WHERE ...").
 * FromExpr is like a JoinExpr of jointype JOIN_INNER, except that it
 * may have any number of child nodes, not just two.
 *
 * NOTE: the top level of a Query's jointree is always a FromExpr.
 * Even if the jointree contains no rels, there will be a FromExpr.
 *
 * NOTE: the qualification expressions present in JoinExpr nodes are
 * *in addition to* the query's main WHERE clause, which appears as the
 * qual of the top-level FromExpr.  The reason for associating quals with
 * specific nodes in the jointree is that the position of a qual is critical
 * when outer joins are present.  (If we enforce a qual too soon or too late,
 * that may cause the outer join to produce the wrong set of NULL-extended
 * rows.)  If all joins are inner joins then all the qual positions are
 * semantically interchangeable.
 *
 * NOTE: in the raw output of gram.y, a join tree contains RangeVar,
 * RangeSubselect, and RangeFunction nodes, which are all replaced by
 * RangeTblRef nodes during the parse analysis phase.  Also, the top-level
 * FromExpr is added during parse analysis; the grammar regards FROM and
 * WHERE as separate.
 * ----------------------------------------------------------------
 */

/*
 * RangeTblRef - reference to an entry in the query's rangetable
 *
 * We could use direct pointers to the RT entries and skip having these
 * nodes, but multiple pointers to the same node in a querytree cause
 * lots of headaches, so it seems better to store an index into the RT.
 */
export interface RangeTblRef extends Node<NodeTag.T_RangeTblRef>
{
	rtindex: number;
}

/*----------
 * JoinExpr - for SQL JOIN expressions
 *
 * isNatural, usingClause, and quals are interdependent.  The user can write
 * only one of NATURAL, USING(), or ON() (this is enforced by the grammar).
 * If he writes NATURAL then parse analysis generates the equivalent USING()
 * list, and from that fills in "quals" with the right equality comparisons.
 * If he writes USING() then "quals" is filled with equality comparisons.
 * If he writes ON() then only "quals" is set.  Note that NATURAL/USING
 * are not equivalent to ON() since they also affect the output column list.
 *
 * alias is an Alias node representing the AS alias-clause attached to the
 * join expression, or NULL if no clause.  NB: presence or absence of the
 * alias has a critical impact on semantics, because a join with an alias
 * restricts visibility of the tables/columns inside it.
 *
 * During parse analysis, an RTE is created for the Join, and its index
 * is filled into rtindex.  This RTE is present mainly so that Vars can
 * be created that refer to the outputs of the join.  The planner sometimes
 * generates JoinExprs internally; these can have rtindex = 0 if there are
 * no join alias variables referencing such joins.
 *----------
 */
export interface JoinExpr extends Node<NodeTag.T_JoinExpr>
{
	jointype: JoinType;		/* type of join */
	isNatural: boolean;		/* Natural join? Will need to shape table */
	larg: Node<any> | null;			/* left subtree */
	rarg: Node<any> | null;			/* right subtree */
	usingClause?: Value<NodeTag.T_String>[];	/* USING clause, if any (list of String) */
	quals?: Node<any>[];			/* qualifiers on join, if any */
	alias?: Alias;			/* user-written alias clause, if any */
	rtindex: number;		/* RT index assigned for join, or 0 */
}

/*----------
 * FromExpr - represents a FROM ... WHERE ... construct
 *
 * This is both more flexible than a JoinExpr (it can have any number of
 * children, including zero) and less so --- we don't need to deal with
 * aliases and so on.  The output column set is implicitly just the union
 * of the outputs of the children.
 *----------
 */
export interface FromExpr extends Node<NodeTag.T_FromExpr>
{
	fromlist: unknown[];		/* List of join subtrees */
	quals: Node<any>[];			/* qualifiers on join, if any */
}

/*----------
 * OnConflictExpr - represents an ON CONFLICT DO ... expression
 *
 * The optimizer requires a list of inference elements, and optionally a WHERE
 * clause to infer a unique index.  The unique index (or, occasionally,
 * indexes) inferred are used to arbitrate whether or not the alternative ON
 * CONFLICT path is taken.
 *----------
 */
export interface OnConflictExpr extends Node<NodeTag.T_OnConflictExpr>
{
	action: OnConflictAction;	/* DO NOTHING or UPDATE? */

	/* Arbiter */
	arbiterElems: InferenceElem[];	/* unique index arbiter list (of
								 * InferenceElem's) */
	arbiterWhere: unknown;	/* unique index arbiter WHERE clause */
	constraint: number;		/* pg_constraint OID for arbiter */

	/* ON CONFLICT UPDATE */
	onConflictSet: unknown[];	/* List of ON CONFLICT SET TargetEntrys */
	onConflictWhere: unknown[];	/* qualifiers to restrict UPDATE to */
	exclRelIndex: number;	/* RT index of 'excluded' relation */
	exclRelTlist: unknown[];	/* tlist of the EXCLUDED pseudo relation */
}


/*----------
 * CaseExpr - a CASE expression
 *
 * We support two distinct forms of CASE expression:
 *		CASE WHEN boolexpr THEN expr [ WHEN boolexpr THEN expr ... ]
 *		CASE testexpr WHEN compexpr THEN expr [ WHEN compexpr THEN expr ... ]
 * These are distinguishable by the "arg" field being NULL in the first case
 * and the testexpr in the second case.
 *
 * In the raw grammar output for the second form, the condition expressions
 * of the WHEN clauses are just the comparison values.  Parse analysis
 * converts these to valid boolean expressions of the form
 *		CaseTestExpr '=' compexpr
 * where the CaseTestExpr node is a placeholder that emits the correct
 * value at runtime.  This structure is used so that the testexpr need be
 * evaluated only once.  Note that after parse analysis, the condition
 * expressions always yield boolean.
 *
 * Note: we can test whether a CaseExpr has been through parse analysis
 * yet by checking whether casetype is Invalidor: Oid not.
 *----------
 */
export interface CaseExpr extends Node<NodeTag.T_CaseExpr>
{
	casetype: Oid;			/* type of expression result */
	casecollid: Oid;		/* OID of collation, or Invalidif: Oid none */
	arg: Expr;				/* implicit equality comparison argument */
	args: unknown[];		/* the arguments (list of WHEN clauses) */
	defresult: Expr;		/* the default result (ELSE clause) */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * CaseWhen - one arm of a CASE expression
 */
export interface CaseWhen extends Node<NodeTag.T_CaseWhen>
{
	expr: Expr;			/* condition expression */
	result: Expr;			/* substitution result */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * Placeholder node for the test value to be processed by a CASE expression.
 * This is effectively like a Param, but can be implemented more simply
 * since we need only one replacement value at a time.
 *
 * We also abuse this node type for some other purposes, including:
 *	* Placeholder for the current array element value in ArrayCoerceExpr;
 *	  see build_coercion_expression().
 *	* Nested FieldStore/SubscriptingRef assignment expressions in INSERT/UPDATE;
 *	  see transformAssignmentIndirection().
 *
 * The uses in CaseExpr and ArrayCoerceExpr are safe only to the extent that
 * there is not any other CaseExpr or ArrayCoerceExpr between the value source
 * node and its child CaseTestExpr(s).  This is true in the parse analysis
 * output, but the planner's function-inlining logic has to be careful not to
 * break it.
 *
 * The nested-assignment-expression case is safe because the only node types
 * that can be above such CaseTestExprs are FieldStore and SubscriptingRef.
 */
export interface CaseTestExpr extends Node<NodeTag.T_CaseTestExpr>
{
	typeId: Oid;			/* type for substituted value */
	typeMod: number;		/* typemod for substituted value */
	collation: Oid;		/* collation for the substituted value */
}

/*
 * ArrayExpr - an ARRAY[] expression
 *
 * Note: if multidims is false, the constituent expressions all yield the
 * scalar type identified by element_typeid.  If multidims is true, the
 * constituent expressions all yield arrays of element_typeid (ie, the same
 * type as array_typeid); at runtime we must check for compatible subscripts.
 */
export interface ArrayExpr extends Node<NodeTag.T_ArrayExpr>
{
	array_typeid: Oid;		/* type of expression result */
	array_collid: Oid;		/* OID of collation, or Invalidif: Oid none */
	element_typeid: Oid; 	/* common type of array elements */
	elements: unknown[];	/* the array elements or sub-arrays */
	multidims: boolean;		/* true if elements are sub-arrays */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * RowExpr - a ROW() expression
 *
 * Note: the list of fields must have a one-for-one correspondence with
 * physical fields of the associated rowtype, although it is okay for it
 * to be shorter than the rowtype.  That is, the N'th list element must
 * match up with the N'th physical field.  When the N'th physical field
 * is a dropped column (attisdropped) then the N'th list element can just
 * be a NULL constant.  (This case can only occur for named composite types,
 * not RECORD types, since those are built from the RowExpr itself rather
 * than vice versa.)  It is important not to assume that length(args) is
 * the same as the number of columns logically present in the rowtype.
 *
 * colnames provides field names in cases where the names can't easily be
 * obtained otherwise.  Names *must* be provided if row_typeid is RECORDOID.
 * If row_typeid identifies a known composite type, colnames can be NIL to
 * indicate the type's cataloged field names apply.  Note that colnames can
 * be non-NIL even for a composite type, and typically is when the RowExpr
 * was created by expanding a whole-row Var.  This is so that we can retain
 * the column alias names of the RTE that the Var referenced (which would
 * otherwise be very difficult to extract from the parsetree).  Like the
 * args list, colnames is one-for-one with physical fields of the rowtype.
 */
export interface RowExpr extends Node<NodeTag.T_RowExpr>
{
	args: unknown;			/* the fields */
	row_typeid: Oid;		/* RECORDOID or a composite type's ID */

	/*
	 * row_typeid cannot be a domain over composite, only plain composite.  To
	 * create a composite domain value, apply CoerceToDomain to the RowExpr.
	 *
	 * Note: we deliberately do NOT store a typmod.  Although a typmod will be
	 * associated with specific RECORD types at runtime, it will differ for
	 * different backends, and so cannot safely be stored in stored
	 * parsetrees.  We must assume typmod -1 for a RowExpr node.
	 *
	 * We don't need to store a collation either.  The result type is
	 * necessarily composite, and composite types never have a collation.
	 */
	row_format: CoercionForm;						/* how to display this node */
	colnames: Value<NodeTag.T_String>[] | null;		/* list of String, or NIL */
	location: number;								/* token location, or -1 if unknown */
}

/*
 * RowCompareExpr - row-wise comparison, such as (a, b) <= (1, 2)
 *
 * We support row comparison for any operator that can be determined to
 * act like =, <>, <, <=, >, or >= (we determine this by looking for the
 * operator in btree opfamilies).  Note that the same operator name might
 * map to a different operator for each pair of row elements, since the
 * element datatypes can vary.
 *
 * A RowCompareExpr node is only generated for the < <= > >= cases;
 * the = and <> cases are translated to simple AND or OR combinations
 * of the pairwise comparisons.  However, we include = and <> in the
 * RowCompareType enum for the convenience of parser logic.
 */
export enum RowCompareType
{
	/* Values of this enum are chosen to match btree strategy numbers */
	ROWCOMPARE_LT = 1,			/* BTLessStrategyNumber */
	ROWCOMPARE_LE = 2,			/* BTLessEqualStrategyNumber */
	ROWCOMPARE_EQ = 3,			/* BTEqualStrategyNumber */
	ROWCOMPARE_GE = 4,			/* BTGreaterEqualStrategyNumber */
	ROWCOMPARE_GT = 5,			/* BTGreaterStrategyNumber */
	ROWCOMPARE_NE = 6			/* no such btree strategy */
}

export interface RowCompareExpr extends Node<NodeTag.T_RowCompareExpr>
{
	rctype: RowCompareType;		/* LT LE GE or GT, never EQ or NE */
	opnos: unknown[];			/* OID list of pairwise comparison ops */
	opfamilies: unknown[];		/* OID list of containing operator families */
	inputcollids: unknown[];	/* OID list of collations for comparisons */
	largs: unknown[];			/* the left-hand input arguments */
	rargs: unknown[];			/* the right-hand input arguments */
}

/*
 * CoalesceExpr - a COALESCE expression
 */
export interface CoalesceExpr extends Node<NodeTag.T_CoalesceExpr>
{
	coalescetype: Oid;		/* type of expression result */
	coalescecollid: Oid; 	/* OID of collation, or Invalidif: Oid none */
	args: unknown;			/* the arguments */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * MinMaxExpr - a GREATEST or LEAST function
 */
export enum MinMaxOp
{
	IS_GREATEST,
	IS_LEAST
}

export interface MinMaxExpr extends Node<NodeTag.T_MinMaxExpr>
{
	minmaxtype: Oid;			/* common type of arguments and result */
	minmaxcollid: Oid;			/* OID of collation of result */
	inputcollid: Oid;			/* OID of collation that function should use */
	op: MinMaxOp;				/* function to execute */
	args: unknown[];			/* the arguments */
	location: number;			/* token location, or -1 if unknown */
}

/*
 * SQLValueFunction - parameterless functions with special grammar productions
 *
 * The SQL standard categorizes some of these as <datetime value function>
 * and others as <general value specification>.  We call 'em SQLValueFunctions
 * for lack of a better term.  We store type and typmod of the result so that
 * some code doesn't need to know each function individually, and because
 * we would need to store typmod anyway for some of the datetime functions.
 * Note that currently, all variants return non-collating datatypes, so we do
 * not need a collation field; also, all these functions are stable.
 */
export enum SQLValueFunctionOp
{
	SVFOP_CURRENT_DATE,
	SVFOP_CURRENT_TIME,
	SVFOP_CURRENT_TIME_N,
	SVFOP_CURRENT_TIMESTAMP,
	SVFOP_CURRENT_TIMESTAMP_N,
	SVFOP_LOCALTIME,
	SVFOP_LOCALTIME_N,
	SVFOP_LOCALTIMESTAMP,
	SVFOP_LOCALTIMESTAMP_N,
	SVFOP_CURRENT_ROLE,
	SVFOP_CURRENT_USER,
	SVFOP_USER,
	SVFOP_SESSION_USER,
	SVFOP_CURRENT_CATALOG,
	SVFOP_CURRENT_SCHEMA
} SQLValueFunctionOp;

interface SQLValueFunction extends Node<NodeTag.T_SQLValueFunction>
{
	op: SQLValueFunctionOp;	/* which function this is */
	typmod: number;
	location: number;		/* token location, or -1 if unknown */
}

/*
 * XmlExpr - various SQL/XML functions requiring special grammar productions
 *
 * 'name' carries the "NAME foo" argument (already XML-escaped).
 * 'named_args' and 'arg_names' represent an xml_attribute list.
 * 'args' carries all other arguments.
 *
 * Note: result type/typmod/collation are not stored, but can be deduced
 * from the XmlExprOp.  The type/typmod fields are just used for display
 * purposes, and are NOT necessarily the true result type of the node.
 */
export enum XmlExprOp
{
	IS_XMLCONCAT,				/* XMLCONCAT(args) */
	IS_XMLELEMENT,				/* XMLELEMENT(name, xml_attributes, args) */
	IS_XMLFOREST,				/* XMLFOREST(xml_attributes) */
	IS_XMLPARSE,				/* XMLPARSE(text, is_doc, preserve_ws) */
	IS_XMLPI,					/* XMLPI(name [, args]) */
	IS_XMLROOT,					/* XMLROOT(xml, version, standalone) */
	IS_XMLSERIALIZE,			/* XMLSERIALIZE(is_document, xmlval) */
	IS_DOCUMENT					/* xmlval IS DOCUMENT */
}

export enum XmlOptionType
{
	XMLOPTION_DOCUMENT,
	XMLOPTION_CONTENT
}

export interface XmlExpr extends Node<NodeTag.T_XmlExpr>
{
	op: XmlExprOp;				/* xml function ID */
	name: string;			/* name in xml(NAME foo ...) syntaxes */
	named_args: unknown[];		/* non-XML expressions for xml_attributes */
	arg_names: Value<NodeTag.T_String>[] | null;		/* parallel list of Value strings */
	args: unknown[];			/* list of expressions */
	xmloption?: XmlOptionType;	/* DOCUMENT or CONTENT */
	location: number;		/* token location, or -1 if unknown */
}

/* ----------------
 * NullTest
 *
 * NullTest represents the operation of testing a value for NULLness.
 * The appropriate test is performed and returned as a boolean Datum.
 *
 * When argisrow is false, this simply represents a test for the null value.
 *
 * When argisrow is true, the input expression must yield a rowtype, and
 * the node implements "row IS [NOT] NULL" per the SQL standard.  This
 * includes checking individual fields for NULLness when the row datum
 * itself isn't NULL.
 *
 * NOTE: the combination of a rowtype input and argisrow==false does NOT
 * correspond to the SQL notation "row IS [NOT] NULL"; instead, this case
 * represents the SQL notation "row IS [NOT] DISTINCT FROM NULL".
 * ----------------
 */

export enum NullTestType
{
	IS_NULL,
	IS_NOT_NULL
}

export interface NullTest extends Node<NodeTag.T_NullTest>
{
	arg: Expr;			/* input expression */
	nulltesttype: NullTestType;	/* IS NULL, IS NOT NULL */
	argisrow: boolean;		/* T to perform field-by-field null checks */
	location: number;		/* token location, or -1 if unknown */
}

/*
 * BooleanTest
 *
 * BooleanTest represents the operation of determining whether a boolean
 * is TRUE, FALSE, or UNKNOWN (ie, NULL).  All six meaningful combinations
 * are supported.  Note that a NULL input does *not* cause a NULL result.
 * The appropriate test is performed and returned as a boolean Datum.
 */

export enum BoolTestType
{
	IS_TRUE,
	IS_NOT_TRUE,
	IS_FALSE,
	IS_NOT_FALSE,
	IS_UNKNOWN,
	IS_NOT_UNKNOWN
}

export interface BooleanTest extends Node<NodeTag.T_BooleanTest>
{
	arg: Expr;			/* input expression */
	booltesttype: BoolTestType;	/* test type */
	location: number;		/* token location, or -1 if unknown */
}
