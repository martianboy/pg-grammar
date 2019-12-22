import { Node } from "./node";
import { TypeName, TypeCast, A_Const } from "./parsenodes";
import { NodeTag } from "./tags";
import { Value } from "./value";
import { SystemTypeName } from "./makefuncs";

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
