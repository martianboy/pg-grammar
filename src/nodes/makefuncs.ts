import { Node, IsA } from './node';
import { RangeVar, Var, Index, AttrNumber, Oid, BoolExpr, BoolExprType, FromExpr, Const, Alias, Expr } from "./primnodes";
import { NodeTag } from "./tags";
import { TypeName, A_Expr, A_Expr_Kind } from "./parsenodes";
import { makeString, Value } from "./value";
import { BoolGetDatum } from './datum';
import { BOOLOID } from '../common/pg_type_d';
import { RELPERSISTENCE_PERMANENT } from '../common/pg_class_d';

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
				 lexpr: Node<any> | null, rexpr: Node<any> | null, location: number): A_Expr
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
    return {
        type: NodeTag.T_RangeVar,
	    catalogname: null,
	    schemaname,
	    relname,
	    inh: true,
	    relpersistence: RELPERSISTENCE_PERMANENT,
	    alias: null,
	    location: location,
    }
}


/*
 * makeFromExpr -
 *	  creates a FromExpr node
 */
export function makeFromExpr(fromlist: unknown[], quals: Node<any>[]): FromExpr
{
	return {
        type: NodeTag.T_FromExpr,
        fromlist,
        quals
    }
}

/*
 * makeConst -
 *	  creates a Const node
 */
export function makeConst(consttype: Oid,
		  consttypmod: number,
		  constcollid: Oid,
		  constlen: number,
		  constvalue: any,
		  constisnull: boolean,
		  constbyval: boolean): Const
{
	/*
	 * If it's a varlena value, force it to be in non-expanded (non-toasted)
	 * format; this avoids any possible dependency on external values and
	 * improves consistency of representation, which is important for equal().
	 */
	if (!constisnull && constlen == -1)
		constvalue = null;

    return {
        type: NodeTag.T_Const,
        consttype,
        consttypmod,
        constcollid,
        constlen,
        constvalue,
        constisnull,
        constbyval,
        location: -1
    }
}

/*
 * makeNullConst -
 *	  creates a Const node representing a NULL of the specified type/typmod
 *
 * This is a convenience routine that just saves a lookup of the type's
 * storage properties.
 */
export function makeNullConst(consttype: Oid, consttypmod: number, constcollid: Oid): Const
{
	return makeConst(consttype,
					 consttypmod,
					 constcollid,
					 0,
					 0,
					 true,
					 false);
}

/*
 * makeBoolConst -
 *	  creates a Const node representing a boolean value (can be NULL too)
 */
export function makeBoolConst(value: boolean, isnull: boolean)
{
	/* note that pg_type.h hardwires size of bool as 1 ... duplicate it */
	return makeConst(BOOLOID, -1, 0, 1,
							  BoolGetDatum(value), isnull, true);
}

/*
 * makeBoolExpr -
 *	  creates a BoolExpr node
 */
export function makeBoolExpr(boolop: BoolExprType, args: unknown[], location: number): BoolExpr
{
	return {
        type: NodeTag.T_BoolExpr,
        boolop,
        args,
        location
    };
}

/*
 * makeAlias -
 *	  creates an Alias node
 *
 * NOTE: the given name is copied, but the colnames list (if any) isn't.
 */
export function makeAlias(aliasname: string, colnames: Value<NodeTag.T_String>[]): Alias
{
	return {
        type: NodeTag.T_Alias,
        aliasname,
        colnames
    };
}

// /*
//  * makeRelabelType -
//  *	  creates a RelabelType node
//  */
// export function makeRelabelType(arg: Expr, rtype: Oid, rtypmod: number, rcollid: Oid,
// 				rformat: CoercionForm): RelabelType
// {
//     return {
//         type: NodeTag.T_RelabelType,
// 	    arg,
// 	    resulttype: rtype,
// 	    resulttypmod: rtypmod,
// 	    resultcollid: rcollid,
// 	    relabelformat: rformat,
//         location: -1
//     };
// }

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


export function makeAndExpr(lexpr: Node<any> | null, rexpr: Node<any>, location: number)
{
	let lexp = lexpr;

	/* Look through AEXPR_PAREN nodes so they don't affect flattening */
	while (IsA(lexp, 'A_Expr') &&
		   ((lexp as A_Expr).kind == A_Expr_Kind.AEXPR_PAREN))
		lexp = (lexp as A_Expr).lexpr;
	/* Flatten "a AND b AND c ..." to a single BoolExpr on sight */
	if (IsA(lexp, 'BoolExpr'))
	{
		let blexpr: BoolExpr = lexp as BoolExpr;

		if (blexpr.boolop == BoolExprType.AND_EXPR)
		{
			blexpr.args.push(rexpr);
			return blexpr;
		}
	}
	return makeBoolExpr(BoolExprType.AND_EXPR, [lexpr, rexpr], location);
}

export function makeOrExpr(lexpr: Node<any> | null, rexpr: Node<any> | null, location: number)
{
	let lexp = lexpr;

	/* Look through AEXPR_PAREN nodes so they don't affect flattening */
	while (IsA(lexp, 'A_Expr') &&
		   ((lexp as A_Expr).kind == A_Expr_Kind.AEXPR_PAREN))
		lexp = (lexp as A_Expr).lexpr;
	/* Flatten "a AND b AND c ..." to a single BoolExpr on sight */
	if (IsA(lexp, 'BoolExpr'))
	{
		let blexpr: BoolExpr = lexp as BoolExpr;

		if (blexpr.boolop == BoolExprType.OR_EXPR)
		{
			blexpr.args.push(rexpr);
			return blexpr;
		}
	}
	return makeBoolExpr(BoolExprType.OR_EXPR, [lexpr, rexpr], location);
}

export function makeNotExpr(expr: Expr, location: number)
{
	return makeBoolExpr(BoolExprType.NOT_EXPR, [expr], location);
}
