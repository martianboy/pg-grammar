/*
 * This enum represents the different strengths of FOR UPDATE/SHARE clauses.
 * The ordering here is important, because the highest numerical value takes
 * precedence when a RTE is specified multiple ways.  See applyLockingClause.
 */
export enum LockClauseStrength
{
	LCS_NONE,					/* no such clause - only used in PlanRowMark */
	LCS_FORKEYSHARE,			/* FOR KEY SHARE */
	LCS_FORSHARE,				/* FOR SHARE */
	LCS_FORNOKEYUPDATE,			/* FOR NO KEY UPDATE */
	LCS_FORUPDATE				/* FOR UPDATE */
}

/*
 * This enum controls how to deal with rows being locked by FOR UPDATE/SHARE
 * clauses (i.e., it represents the NOWAIT and SKIP LOCKED options).
 * The ordering here is important, because the highest numerical value takes
 * precedence when a RTE is specified multiple ways.  See applyLockingClause.
 */
export enum LockWaitPolicy
{
	/* Wait for the lock to become available (default behavior) */
	LockWaitBlock,
	/* Skip rows that can't be locked (SKIP LOCKED) */
	LockWaitSkip,
	/* Raise an error if a row cannot be locked (NOWAIT) */
	LockWaitError
}

/*
 * Possible lock modes for a tuple.
 */
export enum LockTupleMode
{
	/* SELECT FOR KEY SHARE */
	LockTupleKeyShare,
	/* SELECT FOR SHARE */
	LockTupleShare,
	/* SELECT FOR NO KEY UPDATE, and UPDATEs that don't modify key columns */
	LockTupleNoKeyExclusive,
	/* SELECT FOR UPDATE, UPDATEs that modify key columns, and DELETE */
	LockTupleExclusive
}
