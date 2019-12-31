import { Node } from "./node";
import { NodeTag } from "./tags";


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
	colnames: Node<any>[];		/* optional list of column aliases */
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
