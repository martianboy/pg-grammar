export enum ScanKeywordCategory {
    UNRESERVED_KEYWORD,
    RESERVED_KEYWORD,
    COL_NAME_KEYWORD,
    TYPE_FUNC_NAME_KEYWORD
}

function PG_KEYWORD(kwname: string, value: string, category: ScanKeywordCategory) {
    return { kwname, value, category };
}

export const keywordlist = [
    PG_KEYWORD("abort", 'ABORT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("absolute", 'ABSOLUTE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("access", 'ACCESS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("action", 'ACTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("add", 'ADD_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("admin", 'ADMIN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("after", 'AFTER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("aggregate", 'AGGREGATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("all", 'ALL', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("also", 'ALSO', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("alter", 'ALTER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("always", 'ALWAYS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("analyse", 'ANALYSE', ScanKeywordCategory.RESERVED_KEYWORD)		/* British spelling */,
    PG_KEYWORD("analyze", 'ANALYZE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("and", 'AND', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("any", 'ANY', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("array", 'ARRAY', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("as", 'AS', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("asc", 'ASC', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("assertion", 'ASSERTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("assignment", 'ASSIGNMENT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("asymmetric", 'ASYMMETRIC', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("at", 'AT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("attach", 'ATTACH', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("attribute", 'ATTRIBUTE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("authorization", 'AUTHORIZATION', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("backward", 'BACKWARD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("before", 'BEFORE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("begin", 'BEGIN_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("between", 'BETWEEN', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("bigint", 'BIGINT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("binary", 'BINARY', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("bit", 'BIT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("boolean", 'BOOLEAN_P', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("both", 'BOTH', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("by", 'BY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cache", 'CACHE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("call", 'CALL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("called", 'CALLED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cascade", 'CASCADE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cascaded", 'CASCADED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("case", 'CASE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("cast", 'CAST', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("catalog", 'CATALOG_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("chain", 'CHAIN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("char", 'CHAR_P', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("character", 'CHARACTER', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("characteristics", 'CHARACTERISTICS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("check", 'CHECK', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("checkpoint", 'CHECKPOINT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("class", 'CLASS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("close", 'CLOSE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cluster", 'CLUSTER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("coalesce", 'COALESCE', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("collate", 'COLLATE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("collation", 'COLLATION', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("column", 'COLUMN', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("columns", 'COLUMNS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("comment", 'COMMENT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("comments", 'COMMENTS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("commit", 'COMMIT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("committed", 'COMMITTED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("concurrently", 'CONCURRENTLY', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("configuration", 'CONFIGURATION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("conflict", 'CONFLICT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("connection", 'CONNECTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("constraint", 'CONSTRAINT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("constraints", 'CONSTRAINTS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("content", 'CONTENT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("continue", 'CONTINUE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("conversion", 'CONVERSION_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("copy", 'COPY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cost", 'COST', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("create", 'CREATE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("cross", 'CROSS', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("csv", 'CSV', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cube", 'CUBE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("current", 'CURRENT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("current_catalog", 'CURRENT_CATALOG', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("current_date", 'CURRENT_DATE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("current_role", 'CURRENT_ROLE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("current_schema", 'CURRENT_SCHEMA', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("current_time", 'CURRENT_TIME', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("current_timestamp", 'CURRENT_TIMESTAMP', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("current_user", 'CURRENT_USER', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("cursor", 'CURSOR', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("cycle", 'CYCLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("data", 'DATA_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("database", 'DATABASE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("day", 'DAY_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("deallocate", 'DEALLOCATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("dec", 'DEC', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("decimal", 'DECIMAL_P', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("declare", 'DECLARE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("default", 'DEFAULT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("defaults", 'DEFAULTS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("deferrable", 'DEFERRABLE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("deferred", 'DEFERRED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("definer", 'DEFINER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("delete", 'DELETE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("delimiter", 'DELIMITER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("delimiters", 'DELIMITERS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("depends", 'DEPENDS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("desc", 'DESC', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("detach", 'DETACH', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("dictionary", 'DICTIONARY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("disable", 'DISABLE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("discard", 'DISCARD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("distinct", 'DISTINCT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("do", 'DO', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("document", 'DOCUMENT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("domain", 'DOMAIN_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("double", 'DOUBLE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("drop", 'DROP', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("each", 'EACH', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("else", 'ELSE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("enable", 'ENABLE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("encoding", 'ENCODING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("encrypted", 'ENCRYPTED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("end", 'END_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("enum", 'ENUM_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("escape", 'ESCAPE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("event", 'EVENT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("except", 'EXCEPT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("exclude", 'EXCLUDE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("excluding", 'EXCLUDING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("exclusive", 'EXCLUSIVE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("execute", 'EXECUTE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("exists", 'EXISTS', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("explain", 'EXPLAIN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("expression", 'EXPRESSION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("extension", 'EXTENSION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("external", 'EXTERNAL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("extract", 'EXTRACT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("false", 'FALSE_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("family", 'FAMILY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("fetch", 'FETCH', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("filter", 'FILTER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("first", 'FIRST_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("float", 'FLOAT_P', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("following", 'FOLLOWING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("for", 'FOR', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("force", 'FORCE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("foreign", 'FOREIGN', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("forward", 'FORWARD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("freeze", 'FREEZE', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("from", 'FROM', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("full", 'FULL', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("function", 'FUNCTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("functions", 'FUNCTIONS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("generated", 'GENERATED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("global", 'GLOBAL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("grant", 'GRANT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("granted", 'GRANTED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("greatest", 'GREATEST', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("group", 'GROUP_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("grouping", 'GROUPING', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("groups", 'GROUPS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("handler", 'HANDLER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("having", 'HAVING', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("header", 'HEADER_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("hold", 'HOLD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("hour", 'HOUR_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("identity", 'IDENTITY_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("if", 'IF_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("ilike", 'ILIKE', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("immediate", 'IMMEDIATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("immutable", 'IMMUTABLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("implicit", 'IMPLICIT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("import", 'IMPORT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("in", 'IN_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("include", 'INCLUDE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("including", 'INCLUDING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("increment", 'INCREMENT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("index", 'INDEX', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("indexes", 'INDEXES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("inherit", 'INHERIT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("inherits", 'INHERITS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("initially", 'INITIALLY', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("inline", 'INLINE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("inner", 'INNER_P', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("inout", 'INOUT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("input", 'INPUT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("insensitive", 'INSENSITIVE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("insert", 'INSERT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("instead", 'INSTEAD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("int", 'INT_P', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("integer", 'INTEGER', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("intersect", 'INTERSECT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("interval", 'INTERVAL', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("into", 'INTO', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("invoker", 'INVOKER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("is", 'IS', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("isnull", 'ISNULL', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("isolation", 'ISOLATION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("join", 'JOIN', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("key", 'KEY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("label", 'LABEL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("language", 'LANGUAGE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("large", 'LARGE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("last", 'LAST_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("lateral", 'LATERAL_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("leading", 'LEADING', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("leakproof", 'LEAKPROOF', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("least", 'LEAST', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("left", 'LEFT', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("level", 'LEVEL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("like", 'LIKE', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("limit", 'LIMIT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("listen", 'LISTEN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("load", 'LOAD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("local", 'LOCAL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("localtime", 'LOCALTIME', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("localtimestamp", 'LOCALTIMESTAMP', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("location", 'LOCATION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("lock", 'LOCK_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("locked", 'LOCKED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("logged", 'LOGGED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("mapping", 'MAPPING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("match", 'MATCH', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("materialized", 'MATERIALIZED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("maxvalue", 'MAXVALUE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("method", 'METHOD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("minute", 'MINUTE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("minvalue", 'MINVALUE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("mode", 'MODE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("month", 'MONTH_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("move", 'MOVE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("name", 'NAME_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("names", 'NAMES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("national", 'NATIONAL', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("natural", 'NATURAL', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("nchar", 'NCHAR', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("new", 'NEW', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("next", 'NEXT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("no", 'NO', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("none", 'NONE', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("not", 'NOT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("nothing", 'NOTHING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("notify", 'NOTIFY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("notnull", 'NOTNULL', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("nowait", 'NOWAIT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("null", 'NULL_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("nullif", 'NULLIF', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("nulls", 'NULLS_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("numeric", 'NUMERIC', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("object", 'OBJECT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("of", 'OF', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("off", 'OFF', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("offset", 'OFFSET', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("oids", 'OIDS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("old", 'OLD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("on", 'ON', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("only", 'ONLY', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("operator", 'OPERATOR', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("option", 'OPTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("options", 'OPTIONS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("or", 'OR', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("order", 'ORDER', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("ordinality", 'ORDINALITY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("others", 'OTHERS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("out", 'OUT_P', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("outer", 'OUTER_P', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("over", 'OVER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("overlaps", 'OVERLAPS', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("overlay", 'OVERLAY', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("overriding", 'OVERRIDING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("owned", 'OWNED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("owner", 'OWNER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("parallel", 'PARALLEL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("parser", 'PARSER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("partial", 'PARTIAL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("partition", 'PARTITION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("passing", 'PASSING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("password", 'PASSWORD', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("placing", 'PLACING', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("plans", 'PLANS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("policy", 'POLICY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("position", 'POSITION', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("preceding", 'PRECEDING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("precision", 'PRECISION', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("prepare", 'PREPARE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("prepared", 'PREPARED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("preserve", 'PRESERVE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("primary", 'PRIMARY', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("prior", 'PRIOR', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("privileges", 'PRIVILEGES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("procedural", 'PROCEDURAL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("procedure", 'PROCEDURE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("procedures", 'PROCEDURES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("program", 'PROGRAM', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("publication", 'PUBLICATION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("quote", 'QUOTE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("range", 'RANGE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("read", 'READ', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("real", 'REAL', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("reassign", 'REASSIGN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("recheck", 'RECHECK', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("recursive", 'RECURSIVE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("ref", 'REF', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("references", 'REFERENCES', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("referencing", 'REFERENCING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("refresh", 'REFRESH', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("reindex", 'REINDEX', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("relative", 'RELATIVE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("release", 'RELEASE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("rename", 'RENAME', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("repeatable", 'REPEATABLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("replace", 'REPLACE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("replica", 'REPLICA', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("reset", 'RESET', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("restart", 'RESTART', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("restrict", 'RESTRICT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("returning", 'RETURNING', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("returns", 'RETURNS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("revoke", 'REVOKE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("right", 'RIGHT', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("role", 'ROLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("rollback", 'ROLLBACK', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("rollup", 'ROLLUP', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("routine", 'ROUTINE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("routines", 'ROUTINES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("row", 'ROW', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("rows", 'ROWS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("rule", 'RULE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("savepoint", 'SAVEPOINT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("schema", 'SCHEMA', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("schemas", 'SCHEMAS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("scroll", 'SCROLL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("search", 'SEARCH', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("second", 'SECOND_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("security", 'SECURITY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("select", 'SELECT', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("sequence", 'SEQUENCE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("sequences", 'SEQUENCES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("serializable", 'SERIALIZABLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("server", 'SERVER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("session", 'SESSION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("session_user", 'SESSION_USER', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("set", 'SET', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("setof", 'SETOF', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("sets", 'SETS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("share", 'SHARE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("show", 'SHOW', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("similar", 'SIMILAR', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("simple", 'SIMPLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("skip", 'SKIP', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("smallint", 'SMALLINT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("snapshot", 'SNAPSHOT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("some", 'SOME', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("sql", 'SQL_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("stable", 'STABLE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("standalone", 'STANDALONE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("start", 'START', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("statement", 'STATEMENT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("statistics", 'STATISTICS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("stdin", 'STDIN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("stdout", 'STDOUT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("storage", 'STORAGE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("stored", 'STORED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("strict", 'STRICT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("strip", 'STRIP_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("subscription", 'SUBSCRIPTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("substring", 'SUBSTRING', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("support", 'SUPPORT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("symmetric", 'SYMMETRIC', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("sysid", 'SYSID', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("system", 'SYSTEM_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("table", 'TABLE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("tables", 'TABLES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("tablesample", 'TABLESAMPLE', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("tablespace", 'TABLESPACE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("temp", 'TEMP', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("template", 'TEMPLATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("temporary", 'TEMPORARY', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("text", 'TEXT_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("then", 'THEN', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("ties", 'TIES', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("time", 'TIME', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("timestamp", 'TIMESTAMP', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("to", 'TO', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("trailing", 'TRAILING', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("transaction", 'TRANSACTION', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("transform", 'TRANSFORM', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("treat", 'TREAT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("trigger", 'TRIGGER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("trim", 'TRIM', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("true", 'TRUE_P', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("truncate", 'TRUNCATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("trusted", 'TRUSTED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("type", 'TYPE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("types", 'TYPES_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("unbounded", 'UNBOUNDED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("uncommitted", 'UNCOMMITTED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("unencrypted", 'UNENCRYPTED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("union", 'UNION', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("unique", 'UNIQUE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("unknown", 'UNKNOWN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("unlisten", 'UNLISTEN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("unlogged", 'UNLOGGED', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("until", 'UNTIL', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("update", 'UPDATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("user", 'USER', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("using", 'USING', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("vacuum", 'VACUUM', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("valid", 'VALID', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("validate", 'VALIDATE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("validator", 'VALIDATOR', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("value", 'VALUE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("values", 'VALUES', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("varchar", 'VARCHAR', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("variadic", 'VARIADIC', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("varying", 'VARYING', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("verbose", 'VERBOSE', ScanKeywordCategory.TYPE_FUNC_NAME_KEYWORD),
    PG_KEYWORD("version", 'VERSION_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("view", 'VIEW', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("views", 'VIEWS', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("volatile", 'VOLATILE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("when", 'WHEN', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("where", 'WHERE', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("whitespace", 'WHITESPACE_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("window", 'WINDOW', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("with", 'WITH', ScanKeywordCategory.RESERVED_KEYWORD),
    PG_KEYWORD("within", 'WITHIN', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("without", 'WITHOUT', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("work", 'WORK', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("wrapper", 'WRAPPER', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("write", 'WRITE', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("xml", 'XML_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("xmlattributes", 'XMLATTRIBUTES', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlconcat", 'XMLCONCAT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlelement", 'XMLELEMENT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlexists", 'XMLEXISTS', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlforest", 'XMLFOREST', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlnamespaces", 'XMLNAMESPACES', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlparse", 'XMLPARSE', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlpi", 'XMLPI', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlroot", 'XMLROOT', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmlserialize", 'XMLSERIALIZE', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("xmltable", 'XMLTABLE', ScanKeywordCategory.COL_NAME_KEYWORD),
    PG_KEYWORD("year", 'YEAR_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("yes", 'YES_P', ScanKeywordCategory.UNRESERVED_KEYWORD),
    PG_KEYWORD("zone", 'ZONE', ScanKeywordCategory.UNRESERVED_KEYWORD),
]

export function ScanKeywordLookup(token: string): number {
    token = token.toLowerCase();
    for (let i = 0; i < keywordlist.length; i++) {
        if (keywordlist[i].kwname === token) return i;
    }

    return -1;
}

export function GetScanKeyword(n: number, keywords: string[]): string {
    return keywordlist[n].value;
}