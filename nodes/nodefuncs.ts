import { Node } from "./node";
import { RangeVar, Var, Const, TableFunc, Param, Expr } from "./primnodes";
import { NodeTag } from "./tags";

/*
 *	exprLocation -
 *	  returns the parse location of an expression tree, for error reports
 *
 * -1 is returned if the location can't be determined.
 *
 * For expressions larger than a single token, the intent here is to
 * return the location of the expression's leftmost token, not necessarily
 * the topmost Node's location field.  For example, an OpExpr's location
 * field will point at the operator name, but if it is not a prefix operator
 * then we should return the location of the left-hand operand instead.
 * The reason is that we want to reference the entire expression not just
 * that operator, and pointing to its start seems to be the most natural way.
 *
 * The location is not perfect --- for example, since the grammar doesn't
 * explicitly represent parentheses in the parsetree, given something that
 * had been written "(a + b) * c" we are going to point at "a" not "(".
 * But it should be plenty good enough for error reporting purposes.
 *
 * You might think that this code is overly general, for instance why check
 * the operands of a FuncExpr node, when the function name can be expected
 * to be to the left of them?  There are a couple of reasons.  The grammar
 * sometimes builds expressions that aren't quite what the user wrote;
 * for instance x IS NOT BETWEEN ... becomes a NOT-expression whose keyword
 * pointer is to the right of its leftmost argument.  Also, nodes that were
 * inserted implicitly by parse analysis (such as FuncExprs for implicit
 * coercions) will have location -1, and so we can have odd combinations of
 * known and unknown locations in a tree.
 */
export function exprLocation(expr: Expr): number
{
	let loc: number;

	if (expr == null)
        return -1;

	switch (expr.type)
	{
		case NodeTag.T_RangeVar:
		case NodeTag.T_TableFunc:
		case NodeTag.T_Var:
		case NodeTag.T_Const:
		case NodeTag.T_Param:
			loc = expr.location;
            break;
        default:
            loc = -1;
            break;
    }

    return loc;
}