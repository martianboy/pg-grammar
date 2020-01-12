import { Node } from './node';
import { RangeVar, Var, Index, AttrNumber, Oid, BoolExpr, BoolExprType, FromExpr, Const, Alias, Expr, SQLValueFunctionOp, XmlExpr, XmlExprOp } from "./primnodes";
import { NodeTag } from "./tags";
import { TypeName, A_Expr, A_Expr_Kind, GroupingSetKind, GroupingSet, SelectStmt, SetOperation, FuncCall, A_ArrayExpr } from "./parsenodes";
import { makeString, Value } from "./value";
import { BoolGetDatum } from './datum';
import { BOOLOID } from '../common/pg_type_d';
import { RELPERSISTENCE_PERMANENT } from '../common/pg_class_d';

/*
 * makeA_Expr -
 *		makes an A_Expr node
 */
export function makeA_Expr(kind: A_Expr_Kind, name: Node<NodeTag.T_String>[],
		   lexpr: Expr, rexpr: Expr, location: number): A_Expr
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
				 lexpr: Expr | null, rexpr: Expr | null, location: number): A_Expr
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

/* SystemFuncName()
 * Build a properly-qualified reference to a built-in function.
 */
export function SystemFuncName(name: string)
{
	return [makeString("pg_catalog"), makeString(name)];
}

export function makeAndExpr(lexpr: Expr | null, rexpr: Expr, location: number)
{
	let lexp = lexpr;

	/* Look through AEXPR_PAREN nodes so they don't affect flattening */
	while (lexp?.type === NodeTag.T_A_Expr &&
		   (lexp.kind == A_Expr_Kind.AEXPR_PAREN))
		lexp = lexp.lexpr;
	/* Flatten "a AND b AND c ..." to a single BoolExpr on sight */
	if (lexp?.type === NodeTag.T_BoolExpr)
	{
		let blexpr = lexp;

		if (blexpr.boolop == BoolExprType.AND_EXPR)
		{
			blexpr.args.push(rexpr);
			return blexpr;
		}
	}
	return makeBoolExpr(BoolExprType.AND_EXPR, [lexpr, rexpr], location);
}

export function makeOrExpr(lexpr: Expr | null, rexpr: Expr | null, location: number)
{
	let lexp = lexpr;

	/* Look through AEXPR_PAREN nodes so they don't affect flattening */
	while (lexp?.type === NodeTag.T_A_Expr &&
			(lexp.kind == A_Expr_Kind.AEXPR_PAREN))
		lexp = lexp.lexpr;
	/* Flatten "a AND b AND c ..." to a single BoolExpr on sight */
	if (lexp?.type === NodeTag.T_BoolExpr)
	{
		let blexpr = lexp;

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

/*
 * makeGroupingSet
 *
 */
export function makeGroupingSet(kind: GroupingSetKind, content: A_Expr[], location: number): GroupingSet
{
	return {
		type: NodeTag.T_GroupingSet,
		kind,
		content,
		location
	};
}

export function makeSetOp(op: SetOperation, all: boolean, larg: SelectStmt, rarg: SelectStmt): SelectStmt
{
	return {
		type: NodeTag.T_SelectStmt,
		op,
		all,
		larg,
		rarg
	};
}


/*
 * makeFuncCall -
 *
 * Initialize a FuncCall struct with the information every caller must
 * supply.  Any non-default parameters have to be inserted by the caller.
 */
export function makeFuncCall(name: Value<NodeTag.T_String>[], args: Expr[], location: number): FuncCall
{
	return {
		type: NodeTag.T_FuncCall,
		funcname: name,
		args,
		agg_order: null,
		agg_filter: null,
		agg_within_group: false,
		agg_star: false,
		agg_distinct: false,
		func_variadic: false,
		over: null,
		location: location,
	}
}


export function makeAArrayExpr(elements: unknown[], location: number): A_ArrayExpr
{
	return {
		type: NodeTag.T_A_ArrayExpr,
		elements,
		location,
	};
}

export function makeSQLValueFunction(op: SQLValueFunctionOp, typmod: number, location: number)
{
	return {
		type: NodeTag.T_SQLValueFunction,
		op,
		/* svf->type will be filled during parse analysis */
		typmod,
		location
	};
}

export function makeXmlExpr(op: XmlExprOp, name: string, named_args: unknown[], args: unknown[],
			location: number): XmlExpr
{
	return {
		type: NodeTag.T_XmlExpr,
		op,
		name,
		/*
		 * named_args is a list of ResTarget; it'll be split apart into separate
		 * expression and name lists in transformXmlExpr().
		 */
		named_args,
		/* xmloption, if relevant, must be filled in by caller */
		arg_names: null,
		args,
		location
	};
}

/*
 * Merge the input and output parameters of a table function.
 */
// static List *
// mergeTableFuncParameters(List *func_args, List *columns)
// {
// 	ListCell   *lc;

// 	/* Explicit OUT and INOUT parameters shouldn't be used in this syntax */
// 	foreach(lc, func_args)
// 	{
// 		FunctionParameter *p = (FunctionParameter *) lfirst(lc);

// 		if (p->mode != FUNC_PARAM_IN && p->mode != FUNC_PARAM_VARIADIC)
// 			ereport(ERROR,
// 					(errcode(ERRCODE_SYNTAX_ERROR),
// 					 errmsg("OUT and INOUT arguments aren't allowed in TABLE functions")));
// 	}

// 	return list_concat(func_args, columns);
// }
