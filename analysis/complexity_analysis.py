import csv
import re
from collections import Counter, defaultdict

import sqlparse
from ordered_set import OrderedSet

cubes_sql_sep = r'\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+ SQL Solution \+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+\+'
cubes_r_sep = r'(?:------------------------------------- R Solution ---------------------------------------)|(?:All solutions of length \d+ found)|(?:Timeout reached)|(?:\n\n)|(?:\[.*?\])'

run = "c62_16_full"
file = f"data/{run}.csv"
excluded = {"db2csv", "55-tests"}

allowed_keywords = (["ABORT", "ACTION", "ADD", "AFTER", "ALL", "ALTER", "ALWAYS", "ANALYZE", "AND", "AS", "ASC", "ATTACH", "AUTOINCREMENT", "BEFORE", "BEGIN", "BETWEEN", "BY", "CASCADE", "CASE", "CAST", "CHECK", "COLLATE", "COLUMN",
                     "COMMIT", "CONFLICT", "CONSTRAINT", "CREATE", "CROSS", "CURRENT", "CURRENT_DATE", "CURRENT_TIME", "CURRENT_TIMESTAMP", "DATABASE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE", "DESC", "DETACH", "DISTINCT", "DO",
                     "DROP", "EACH", "ELSE", "END", "ESCAPE", "EXCEPT", "EXCLUDE", "EXCLUSIVE", "EXISTS", "EXPLAIN", "FAIL", "FILTER", "FIRST", "FOLLOWING", "FOR", "FOREIGN", "FROM", "FULL", "GENERATED", "GLOB", "GROUP", "GROUPS", "HAVING",
                     "IF", "IGNORE", "IMMEDIATE", "IN", "INDEX", "INDEXED", "INITIALLY", "INNER", "INSERT", "INSTEAD", "INTERSECT", "INTO", "IS", "ISNULL", "JOIN", "KEY", "LAST", "LEFT", "LIKE", "LIMIT", "MATCH", "MATERIALIZED", "NATURAL",
                     "NO", "NOT", "NOTHING", "NOTNULL", "NULL", "NULLS", "OF", "OFFSET", "ON", "OR", "ORDER", "OTHERS", "OUTER", "OVER", "PARTITION", "PLAN", "PRAGMA", "PRECEDING", "PRIMARY", "QUERY", "RAISE", "RANGE", "RECURSIVE",
                     "REFERENCES", "REGEXP", "REINDEX", "RELEASE", "RENAME", "REPLACE", "RESTRICT", "RETURNING", "RIGHT", "ROLLBACK", "ROW", "ROWS", "SAVEPOINT", "SELECT", "SET", "TABLE", "TEMP", "TEMPORARY", "THEN", "TIES", "TO",
                     "TRANSACTION", "TRIGGER", "UNBOUNDED", "UNION", "UNIQUE", "UPDATE", "USING", "VACUUM", "VALUES", "VIEW", "VIRTUAL", "WHEN", "WHERE", "WINDOW", "WITH", "WITHOUT"] +
                    ["ABS", "ABSENT", "ACOS", "ALL", "ALLOCATE", "ALTER", "AND", "ANY", "ANY_VALUE", "ARE", "ARRAY", "ARRAY_AGG", "ARRAY_MAX_CARDINALITY", "AS", "ASENSITIVE", "ASIN", "ASYMMETRIC", "AT", "ATAN", "ATOMIC", "AUTHORIZATION",
                     "AVG", "BEGIN", "BEGIN_FRAME", "BEGIN_PARTITION", "BETWEEN", "BIGINT", "BINARY", "BLOB", "BOOLEAN", "BOTH", "BTRIM", "BY", "CALL", "CALLED", "CARDINALITY", "CASCADED", "CASE", "CAST", "CEIL", "CEILING", "CHAR",
                     "CHARACTER", "CHARACTER_LENGTH", "CHAR_LENGTH", "CHECK", "CLASSIFIER", "CLOB", "CLOSE", "COALESCE", "COLLATE", "COLLECT", "COLUMN", "COMMIT", "CONDITION", "CONNECT", "CONSTRAINT", "CONTAINS", "CONVERT", "COPY", "CORR",
                     "CORRESPONDING", "COS", "COSH", "COUNT", "COVAR_POP", "COVAR_SAMP", "CREATE", "CROSS", "CUBE", "CUME_DIST", "CURRENT", "CURRENT_CATALOG", "CURRENT_DATE", "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH",
                     "CURRENT_ROLE", "CURRENT_ROW", "CURRENT_SCHEMA", "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CURRENT_USER", "CURSOR", "CYCLE", "DATE", "DAY", "DEALLOCATE", "DEC", "DECFLOAT", "DECIMAL",
                     "DECLARE", "DEFAULT", "DEFINE", "DELETE", "DENSE_RANK", "DEREF", "DESCRIBE", "DETERMINISTIC", "DISCONNECT", "DISTINCT", "DO", "DOUBLE", "DROP", "DYNAMIC", "EACH", "ELEMENT", "ELSE", "ELSEIF", "EMPTY", "END", "END-EXEC",
                     "END_FRAME", "END_PARTITION", "EQUALS", "ESCAPE", "EVERY", "EXCEPT", "EXEC", "EXECUTE", "EXISTS", "EXP", "EXTERNAL", "EXTRACT", "FALSE", "FETCH", "FILTER", "FIRST_VALUE", "FLOAT", "FLOOR", "FOR", "FOREIGN", "FRAME_ROW",
                     "FREE", "FROM", "FULL", "FUNCTION", "FUSION", "GET", "GLOBAL", "GRANT", "GREATEST", "GROUP", "GROUPING", "GROUPS", "HANDLER", "HAVING", "HOLD", "HOUR", "IDENTITY", "IF", "IN", "INDICATOR", "INITIAL", "INNER", "INOUT",
                     "INSENSITIVE", "INSERT", "INT", "INTEGER", "INTERSECT", "INTERSECTION", "INTERVAL", "INTO", "IS", "ITERATE", "JOIN", "JSON", "JSON_ARRAY", "JSON_ARRAYAGG", "JSON_EXISTS", "JSON_OBJECT", "JSON_OBJECTAGG", "JSON_QUERY",
                     "JSON_SCALAR", "JSON_SERIALIZE", "JSON_TABLE", "JSON_TABLE_PRIMITIVE", "JSON_VALUE", "LAG", "LANGUAGE", "LARGE", "LAST_VALUE", "LATERAL", "LEAD", "LEADING", "LEAST", "LEAVE", "LEFT", "LIKE", "LIKE_REGEX", "LISTAGG",
                     "LN", "LOCAL", "LOCALTIME", "LOCALTIMESTAMP", "LOG", "LOG10", "LOOP", "LOWER", "LPAD", "LTRIM", "MATCH", "MATCHES", "MATCH_NUMBER", "MATCH_RECOGNIZE", "MAX", "MEMBER", "MERGE", "METHOD", "MIN", "MINUTE", "MOD",
                     "MODIFIES", "MODULE", "MONTH", "MULTISET", "NATIONAL", "NATURAL", "NCHAR", "NCLOB", "NEW", "NO", "NONE", "NORMALIZE", "NOT", "NTH_VALUE", "NTILE", "NULL", "NULLIF", "NUMERIC", "OCCURRENCES_REGEX", "OCTET_LENGTH", "OF",
                     "OFFSET", "OLD", "OMIT", "ON", "ONE", "ONLY", "OPEN", "OR", "ORDER", "OUT", "OUTER", "OVER", "OVERLAPS", "OVERLAY", "PARAMETER", "PARTITION", "PATTERN", "PER", "PERCENT", "PERCENTILE_CONT", "PERCENTILE_DISC",
                     "PERCENT_RANK", "PERIOD", "PORTION", "POSITION", "POSITION_REGEX", "POWER", "PRECEDES", "PRECISION", "PREPARE", "PRIMARY", "PROCEDURE", "PTF", "RANGE", "RANK", "READS", "REAL", "RECURSIVE", "REF", "REFERENCES",
                     "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "RELEASE", "REPEAT", "RESIGNAL", "RESULT", "RETURN", "RETURNS", "REVOKE", "RIGHT",
                     "ROLLBACK", "ROLLUP", "ROW", "ROWS", "ROW_NUMBER", "RPAD", "RUNNING", "SAVEPOINT", "SCOPE", "SCROLL", "SEARCH", "SECOND", "SEEK", "SELECT", "SENSITIVE", "SESSION_USER", "SET", "SHOW", "SIGNAL", "SIMILAR", "SIN", "SINH",
                     "SKIP", "SMALLINT", "SOME", "SPECIFIC", "SPECIFICTYPE", "SQL", "SQLEXCEPTION", "SQLSTATE", "SQLWARNING", "SQRT", "START", "STATIC", "STDDEV_POP", "STDDEV_SAMP", "SUBMULTISET", "SUBSET", "SUBSTRING", "SUBSTRING_REGEX",
                     "SUCCEEDS", "SUM", "SYMMETRIC", "SYSTEM", "SYSTEM_TIME", "SYSTEM_USER", "TABLE", "TABLESAMPLE", "TAN", "TANH", "THEN", "TIME", "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TRAILING", "TRANSLATE",
                     "TRANSLATE_REGEX", "TRANSLATION", "TREAT", "TRIGGER", "TRIM", "TRIM_ARRAY", "TRUE", "TRUNCATE", "UESCAPE", "UNION", "UNIQUE", "UNKNOWN", "UNNEST", "UNTIL", "UPDATE", "UPPER", "USER", "USING", "VALUE", "VALUES",
                     "VALUE_OF", "VARBINARY", "VARCHAR", "VARYING", "VAR_POP", "VAR_SAMP", "VERSIONING", "WHEN", "WHENEVER", "WHERE", "WHILE", "WIDTH_BUCKET", "WINDOW", "WITH", "WITHIN", "WITHOUT", "YEAR"])


def collect_keywords(tokens):
    keywords = []
    for token in tokens:
        if token.is_keyword:
            if token.normalized != "AS" and token.normalized != "AND" and token.normalized in allowed_keywords:
                keywords.append(token.normalized)
        if hasattr(token, "tokens"):
            keywords += collect_keywords(token.tokens)
    return keywords


all_keywords = OrderedSet()
all_ops = OrderedSet()

data = dict()

with open(file) as f:
    reader = csv.reader(f)
    header = next(reader)
    for row in reader:
        name = row[0]
        name_parts = name.split("/")
        if name_parts[0] in excluded:
            continue
        if row[6] == "0" and row[1] == "False" and row[7] == "False":
            log_file = f"data/{run}/{name}_0.log"

            with open(log_file) as log_f:
                content = log_f.read()

                try:
                    cube = re.search(r"Found solution with cube \((.*)\)", content).groups()[0]
                    cube_parsed = [c.strip().replace(",", "").split(" = ")[1] for c in cube.split(",") if c.strip() != ""]
                    if "limit" in cube_parsed:
                        cube_parsed.remove("limit")
                    all_ops |= cube_parsed
                except:
                    if "Found solution with cube True" in content:
                        cube = ""
                        cube_parsed = []
                    else:
                        cube = None
                        cube_parsed = None

                try:
                    solution = re.search(r"Solution found: \[(.*)]", content).groups()[0]
                except:
                    try:
                        solution = re.search(r"Solution found: (.*)", content).groups()[0]
                    except:
                        solution = None

                try:
                    query = re.search(rf'{cubes_sql_sep}\n((?:.|\n)*?)(?:$|{cubes_r_sep})', content).group(1).strip() + ";"
                    # query = content.split("+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++")[1].strip() + ";"
                    parsed = sqlparse.parse(query)[0]
                    tokens = parsed.tokens
                    keywords = collect_keywords(tokens)
                    all_keywords |= keywords
                except:
                    query = None
                    keywords = []

                data[name] = {
                    "keywords": Counter(keywords) if keywords is not None else defaultdict(None),
                    "ops": Counter(cube_parsed) if cube_parsed is not None else defaultdict(None),
                    "cube": cube,
                    "cube_length": len(cube_parsed) if cube_parsed is not None else None,
                    "solution": solution,
                    "query": query,
                }

    else:
        pass

with open("complexity_analysis.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerow(["name", "solution", "query", "cube_length", "kw_length"] + ["sql_kw_" + k for k in all_keywords] + ["dsl_op_" + o for o in all_ops])

    for key, d in data.items():
        row = [key, d["solution"], d["query"], d["cube_length"], sum(d["keywords"].values())] + [d["keywords"][keyword] for keyword in all_keywords] + [d["ops"][op] for op in all_ops]
        writer.writerow(row)
