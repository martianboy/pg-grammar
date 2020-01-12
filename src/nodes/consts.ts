import { Node } from "./node";
import { TypeName, TypeCast, A_Const, A_Expr_Kind } from "./parsenodes";
import { NodeTag } from "./tags";
import { Value } from "./value";
import { SystemTypeName, makeSimpleA_Expr } from "./makefuncs";
import { Expr } from "./primnodes";

export function makeTypeCast<T extends NodeTag>(arg: Node<T>, typename: TypeName, location: number): TypeCast<T> {
	return {
        type: NodeTag.T_TypeCast,
        arg,
        typeName: typename,
        location
    }
}

export function makeStringConst(str: string, location: number): A_Const {
    return {
        type: NodeTag.T_A_Const,
        location,
        val: {
            type: NodeTag.T_String,
            val: {
                str,
                ival: 0
            }
        }
    }
}

export function makeStringConstCast(str: string, location: number, typename: TypeName): TypeCast<NodeTag.T_A_Const> {
	const s = makeStringConst(str, location);

	return makeTypeCast(s, typename, -1);
}

export function makeIntConst(val: number, location: number): A_Const {
	return {
        type: NodeTag.T_A_Const,
        location,
        val: {
            type: NodeTag.T_Integer,
            val: {
                str: '',
                ival: val
            }
        }
    }

}

export function makeFloatConst(str: string, location: number): A_Const {
	return {
        type: NodeTag.T_A_Const,
        location,
        val: {
            type: NodeTag.T_Float,
            val: {
                str,
                ival: 0
            }
        }
    }
}

export function makeBitStringConst(str: string, location: number): A_Const {
	return {
        type: NodeTag.T_A_Const,
        location,
        val: {
            type: NodeTag.T_BitString,
            val: {
                str,
                ival: 0
            }
        }
    }
}

export function makeNullAConst(location: number): A_Const {
	return {
        type: NodeTag.T_A_Const,
        location
    }
}

export function makeAConst(v: Value<any>, location: number): A_Const {
	switch (v.type)
	{
		case NodeTag.T_Float:
			return makeFloatConst(v.val.str, location);

		case NodeTag.T_Integer:
			return makeIntConst(v.val.ival, location);

		case NodeTag.T_String:
		default:
			return makeStringConst(v.val.str, location);
	}
}

/* makeBoolAConst()
 * Create an A_Const string node and put it inside a boolean cast.
 */
export function makeBoolAConst(state: boolean, location: number): TypeCast<NodeTag.T_A_Const> {
	const n: A_Const = {
        type: NodeTag.T_A_Const,
        location,
        val: {
            type: NodeTag.T_String,
            val: {
                ival: 0,
                str: state ? 't' : 'f'
            }
        }
    }

	return makeTypeCast(n, SystemTypeName("bool"), -1);
}

/* doNegate()
 * Handle negation of a numeric constant.
 *
 * Formerly, we did this here because the optimizer couldn't cope with
 * indexquals that looked like "var = -4" --- it wants "var = const"
 * and a unary minus operator applied to a constant didn't qualify.
 * As of Postgres 7.0, that problem doesn't exist anymore because there
 * is a constant-subexpression simplifier in the optimizer.  However,
 * there's still a good reason for doing this here, which is that we can
 * postpone committing to a particular internal representation for simple
 * negative constants.	It's better to leave "-123.456" in string form
 * until we know what the desired type is.
 */
export function doNegate(n: Expr, location: number)
{
	if (n.type === NodeTag.T_A_Const)
	{
		const con = n;

		/* report the constant's location as that of the '-' sign */
		con.location = location;

		if (con.val && con.val.type == NodeTag.T_Integer)
		{
			con.val.val.ival = -con.val.val.ival;
			return n;
		}
		if (con.val && con.val.type == NodeTag.T_Float)
		{
			doNegateFloat(con.val);
			return n;
		}
	}

	return makeSimpleA_Expr(A_Expr_Kind.AEXPR_OP, '-', null, n, location);
}

export function doNegateFloat(v: Value<NodeTag.T_Float>)
{
	let oldval: string = v.val.str;

	// Assert(IsA(v, Float));
	if (oldval.startsWith('+'))
        oldval = oldval.slice(1);

	if (oldval.startsWith('-'))
		v.val.str = oldval.slice(1);	/* just strip the '-' */
	else
		v.val.str = `-${oldval}`;
}
