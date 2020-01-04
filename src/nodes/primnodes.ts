import { Node, JoinType, OnConflictAction } from "./node";
import { NodeTag } from "./tags";
import { Value } from "./value";


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
	| Var
	| Const
	| RangeVar
	| TableFunc
	| Param
	| BoolExpr
	| InferenceElem
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
// export type Oid = number;

export interface Var extends Node<NodeTag.T_Var>
{
	varno: Index;			/* index of this var's relation in the range
							 * table, or INNER_VAR/OUTER_VAR/INDEX_VAR */
	varattno: AttrNumber;	/* attribute number of this var, or zero for
							 * all attrs ("whole-row Var") */
	vartype: number;		/* pg_type OID for the type of this var */
	vartypmod: number;		/* pg_attribute typmod value */
	varcollid: number;		/* OID of collation, or InvalidOid if none */
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
	constcollid: number;	/* OID of collation, or InvalidOid if none */
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
	paramcollid: Oid;		/* OID of collation, or InvalidOid if none */
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
};

export interface BoolExpr extends Node<NodeTag.T_BoolExpr>
{
	boolop: BoolExprType;
	args: any[];			/* arguments to this expression */
	location: number;		/* token location, or -1 if unknown */
};


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
};

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
