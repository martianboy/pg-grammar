import { Node, IsA } from '../nodes/node';
import { SelectStmt, WithClause, Indirection } from "../nodes/parsenodes";
import { ereport, errcode, errmsg, parser_errposition, ERROR, ERRCODE_SYNTAX_ERROR } from '../common/errors';
import { exprLocation } from '../nodes/nodefuncs';
import { NodeTag } from '../nodes/index';

/* insertSelectOptions()
 * Insert ORDER BY, etc into an already-constructed SelectStmt.
 *
 * This routine is just to avoid duplicating code in SelectStmt productions.
 */
export function insertSelectOptions(stmt: SelectStmt,
					sortClause: unknown[], lockingClause: unknown[],
					limitOffset: Node<any>, limitCount: Node<any>,
					withClause: WithClause,
					yyscanner: unknown)
{
	// Assert(IsA(stmt, 'SelectStmt'));

	/*
	 * Tests here are to reject constructs like
	 *	(SELECT foo ORDER BY bar) ORDER BY baz
	 */
	if (sortClause)
	{
		if (stmt.sortClause)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("multiple ORDER BY clauses not allowed"),
					 parser_errposition(exprLocation(sortClause))));
		stmt.sortClause = sortClause;
	}
	/* We can handle multiple locking clauses, though */
	stmt.lockingClause = (stmt.lockingClause || []).concat(lockingClause);
	if (limitOffset)
	{
		if (stmt.limitOffset)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("multiple OFFSET clauses not allowed"),
					 parser_errposition(exprLocation(limitOffset))));
		stmt.limitOffset = limitOffset;
	}
	if (limitCount)
	{
		if (stmt.limitCount)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("multiple LIMIT clauses not allowed"),
					 parser_errposition(exprLocation(limitCount))));
		stmt.limitCount = limitCount;
	}
	if (withClause)
	{
		if (stmt.withClause)
			ereport(ERROR,
					(errcode(ERRCODE_SYNTAX_ERROR),
					 errmsg("multiple WITH clauses not allowed"),
					 parser_errposition(exprLocation(withClause))));
		stmt.withClause = withClause;
	}
}

/* check_indirection --- check the result of indirection production
 *
 * We only allow '*' at the end of the list, but it's hard to enforce that
 * in the grammar, so do it here.
 */
export function
check_indirection(indirection: Indirection[], yyscanner: unknown): unknown[]
{
	const idx = indirection.findIndex(l => l.type === NodeTag.T_A_Star)

	if (idx > -1 && idx < indirection.length - 1) {
		throw new Error("improper use of \"*\"");
	}

	return indirection;
}
