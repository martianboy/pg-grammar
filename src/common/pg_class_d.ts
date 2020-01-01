export const RELKIND_RELATION = "r"; /* ordinary table */
export const RELKIND_INDEX = "i"; /* secondary index */
export const RELKIND_SEQUENCE = "S"; /* sequence object */
export const RELKIND_TOASTVALUE = "t"; /* for out-of-line values */
export const RELKIND_VIEW = "v"; /* view */
export const RELKIND_MATVIEW = "m"; /* materialized view */
export const RELKIND_COMPOSITE_TYPE = "c"; /* composite type */
export const RELKIND_FOREIGN_TABLE = "f"; /* foreign table */
export const RELKIND_PARTITIONED_TABLE = "p"; /* partitioned table */
export const RELKIND_PARTITIONED_INDEX = "I"; /* partitioned index */

export const RELPERSISTENCE_PERMANENT = "p"; /* regular table */
export const RELPERSISTENCE_UNLOGGED = "u"; /* unlogged permanent table */
export const RELPERSISTENCE_TEMP = "t"; /* temporary table */

/* default selection for replica identity (primary key or nothing) */
export const REPLICA_IDENTITY_DEFAULT = "d";
/* no replica identity is logged for this relation */
export const REPLICA_IDENTITY_NOTHING = "n";
/* all columns are logged as replica identity */
export const REPLICA_IDENTITY_FULL = "f";
/*
 * an explicitly chosen candidate key's columns are used as replica identity.
 * Note this will still be set if the index has been dropped; in that case it
 * has the same meaning as 'd'.
 */
export const REPLICA_IDENTITY_INDEX = "i";
