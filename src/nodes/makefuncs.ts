import { Node } from './node';
import { RangeVar, Var, Index, AttrNumber, Oid } from "./primnodes";
import { NodeTag } from "./tags";
import { TypeName, A_Expr, A_Expr_Kind } from "./parsenodes";
import { makeString } from "./value";

/*
 * makeA_Expr -
 *		makes an A_Expr node
 */
export function makeA_Expr(kind: A_Expr_Kind, name: Node<NodeTag.T_String>[],
		   lexpr: Node<any>, rexpr: Node<any>, location: number): A_Expr
{
    return {
        type: NodeTag.T_A_Expr,
        kind,
        name,
        lexpr,
        rexpr,
        location
    }
}

/*
 * makeSimpleA_Expr -
 *		As above, given a simple (unqualified) operator name
 */
export function makeSimpleA_Expr(kind: A_Expr_Kind, name: string,
				 lexpr: Node<any>, rexpr: Node<any>, location: number): A_Expr
{
    return {
        type: NodeTag.T_A_Expr,
        kind,
        name: [makeString(name)],
        lexpr,
        rexpr,
        location
    }
}

/*
 * makeVar -
 *	  creates a Var node
 */
export function makeVar(varno: Index,
		varattno: AttrNumber,
		vartype: Oid,
		vartypmod: number,
		varcollid: Oid,
		varlevelsup: Index): Var
{
    return {
        type: NodeTag.T_Var,
        varno,
        varattno,
        varcollid,
        vartype,
        vartypmod,
        varlevelsup,
        /*
         * Since few if any routines ever create Var nodes with varnoold/varoattno
         * different from varno/varattno, we don't provide separate arguments for
         * them, but just initialize them to the given varno/varattno. This
         * reduces code clutter and chance of error for most callers.
         */
        varnoold: varno,
        varoattno: varattno,
        /* Likewise, we just set location to "unknown" here */
        location: -1
    };
}

/*
 * makeRangeVar -
 *	  creates a RangeVar node (rather oversimplified case)
 */
export function makeRangeVar(schemaname: string, relname: string, location: number): RangeVar
{
	const r: RangeVar = {
        type: NodeTag.T_RangeVar,
        catalogname: null,
        schemaname,
        relname,
        inh: true,
        relpersistence: '',
        alias: null,
        location
    }

	return r;
}

/*
 * makeTypeName -
 *	build a TypeName node for an unqualified name.
 *
 * typmod is defaulted, but can be changed later by caller.
 */
export function makeTypeName(typeName: string): TypeName
{
	return makeTypeNameFromNameList([makeString(typeName)]);
}

/*
 * makeTypeNameFromNameList -
 *	build a TypeName node for a String list representing a qualified name.
 *
 * typmod is defaulted, but can be changed later by caller.
 */
export function makeTypeNameFromNameList(names: Node<NodeTag.T_String>[]): TypeName
{
    return {
        type: NodeTag.T_TypeName,
        names,
        typmods: [],
        typemod: -1,
        location: -1
    };
}

/* SystemTypeName()
 * Build a properly-qualified reference to a built-in type.
 *
 * typmod is defaulted, but may be changed afterwards by caller.
 * Likewise for the location.
 */
export function SystemTypeName(name: string): TypeName
{
	return makeTypeNameFromNameList([makeString("pg_catalog"), makeString(name)]);
}
