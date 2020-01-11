
/*
 * These are the valid values of type LOCKMODE for all the standard lock
 * methods (both DEFAULT and USER).
 */

/* NoLock is not a lock mode, but a flag value meaning "don't get a lock" */
export const NoLock = 					0

export const AccessShareLock = 			1	/* SELECT */
export const RowShareLock = 			2	/* SELECT FOR UPDATE/FOR SHARE */
export const RowExclusiveLock = 		3	/* INSERT, UPDATE, DELETE */
export const ShareUpdateExclusiveLock = 4	/* VACUUM (non-FULL),ANALYZE, CREATE INDEX
									         * CONCURRENTLY */
export const ShareLock = 				5	/* CREATE INDEX (WITHOUT CONCURRENTLY) */
export const ShareRowExclusiveLock = 	6	/* like EXCLUSIVE MODE, but allows ROW
									         * SHARE */
export const ExclusiveLock = 			7	/* blocks ROW SHARE/SELECT...FOR UPDATE */
export const AccessExclusiveLock = 		8	/* ALTER TABLE, DROP TABLE, VACUUM FULL,
									         * and unqualified LOCK TABLE */

export const MaxLockMode = 				8
