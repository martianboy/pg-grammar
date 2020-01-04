import { Node } from '../nodes/node';
import { SelectStmt, WithClause } from "../nodes/parsenodes";
import { ereport, errcode, errmsg, parser_errposition, ERROR, ERRCODE_SYNTAX_ERROR } from '../common/errors';
import { exprLocation } from '../nodes/nodefuncs';

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
