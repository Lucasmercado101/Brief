/**
 * Prisma Client Error Codes
 */
enum PrismaClientError {
  /**
   * Authentication failed against database server at {database_host},
   * the provided database credentials for {database_user} are not valid.
   * Please make sure to provide valid database credentials for the database server at {database_host}.
   */
  P1000 = "P1000",

  /**
   * Can't reach database server at {database_host}:{database_port}.
   * Please make sure your database server is running at {database_host}:{database_port}.
   */
  P1001 = "P1001",

  /**
   * The database server at {database_host}:{database_port} was reached but timed out.
   * Please try again. Please make sure your database server is running at {database_host}:{database_port}.
   */
  P1002 = "P1002",

  /**
   * Database {database_file_name} does not exist at {database_file_path}.
   */
  P1003 = "P1003",

  /**
   * Database {database_name}.{database_schema_name} does not exist on the database server
   * at {database_host}:{database_port}.
   */
  P1004 = "P1004",

  /**
   * Database {database_name} does not exist on the database server
   * at {database_host}:{database_port}.
   */
  P1005 = "P1005",

  /**
   * Operations timed out after {time}.
   */
  P1008 = "P1008",

  /**
   * Database {database_name} already exists on the database server at {database_host}:{database_port}.
   */
  P1009 = "P1009",

  /**
   * User {database_user} was denied access on the database {database_name}.
   */
  P1010 = "P1010",

  /**
   * Error opening a TLS connection: {message}.
   */
  P1011 = "P1011",

  /**
   * Note: If you get error code P1012 after you upgrade Prisma to version 4.0.0 or later,
   * see the version 4.0.0 upgrade guide. A schema that was valid before version 4.0.0
   * might be invalid in version 4.0.0 and later. The upgrade guide explains how to update
   * your schema to make it valid.
   */
  P1012 = "P1012",

  /**
   * {full_error}
   * Possible P1012 error messages:
   * - "Argument {} is missing."
   * - "Function {} takes {} arguments, but received {}."
   * - ...
   */
  P1012Error = "P1012Error",

  /**
   * The provided database string is invalid. {details}.
   */
  P1013 = "P1013",

  /**
   * The underlying {kind} for model {model} does not exist.
   */
  P1014 = "P1014",

  /**
   * Your Prisma schema is using features that are not supported for the version of the database.
   * Database version: {database_version}
   * Errors:
   * {errors}
   */
  P1015 = "P1015",

  /**
   * Your raw query had an incorrect number of parameters.
   * Expected: {expected}, actual: {actual}.
   */
  P1016 = "P1016",

  /**
   * Server has closed the connection.
   */
  P1017 = "P1017"

  // ... (Continuing with other error codes)
}

/**
 * Prisma Client (Query Engine) Error Codes
 */
enum PrismaClientQueryEngineError {
  /**
   * The provided value for the column is too long for the column's type.
   * Column: {column_name}.
   */
  P2000 = "P2000",

  /**
   * The record searched for in the where condition ({model_name}.{argument_name} = {argument_value})
   * does not exist.
   */
  P2001 = "P2001",

  /**
   * Unique constraint failed on the {constraint}.
   */
  P2002 = "P2002",

  /**
   * Foreign key constraint failed on the field: {field_name}.
   */
  P2003 = "P2003",

  /**
   * A constraint failed on the database: {database_error}.
   */
  P2004 = "P2004",

  /**
   * The value {field_value} stored in the database for the field {field_name}
   * is invalid for the field's type.
   */
  P2005 = "P2005",

  /**
   * The provided value {field_value} for {model_name} field {field_name} is not valid.
   */
  P2006 = "P2006",

  /**
   * Data validation error {database_error}.
   */
  P2007 = "P2007",

  /**
   * Failed to parse the query {query_parsing_error} at {query_position}.
   */
  P2008 = "P2008",

  /**
   * Failed to validate the query: {query_validation_error} at {query_position}.
   */
  P2009 = "P2009",

  /**
   * Raw query failed. Code: {code}. Message: {message}.
   */
  P2010 = "P2010",

  /**
   * Null constraint violation on the {constraint}.
   */
  P2011 = "P2011",

  /**
   * Missing a required value at {path}.
   */
  P2012 = "P2012"

  // ... (Continuing with other error codes)
}

/**
 * Prisma Migrate (Schema Engine) Error Codes
 */
enum PrismaMigrateSchemaEngineError {
  /**
   * Failed to create database: {database_error}.
   */
  P3000 = "P3000",

  /**
   * Migration possible with destructive changes and possible data loss: {migration_engine_destructive_details}.
   */
  P3001 = "P3001",

  /**
   * The attempted migration was rolled back: {database_error}.
   */
  P3002 = "P3002",

  /**
   * The format of migrations changed, the saved migrations are no longer valid.
   * To solve this problem, please follow the steps at: https://pris.ly/d/migrate.
   */
  P3003 = "P3003",

  /**
   * The {database_name} database is a system database, it should not be altered with prisma migrate.
   * Please connect to another database.
   */
  P3004 = "P3004",

  /**
   * The database schema is not empty. Read more about how to baseline an existing production database:
   * https://pris.ly/d/migrate-baseline.
   */
  P3005 = "P3005",

  /**
   * Migration {migration_name} failed to apply cleanly to the shadow database.
   * {error_code}Error:
   * {inner_error}
   */
  P3006 = "P3006"

  // ... (Continuing with other error codes)
}

/**
 * Prisma Database Proxy Error Codes
 */
enum PrismaDatabaseProxyError {
  /**
   * This request could not be understood by the server.
   */
  P5000 = "P5000",

  /**
   * This request must be retried.
   */
  P5001 = "P5001",

  /**
   * The datasource provided is invalid:
   * - Could not parse the URL of the datasource
   * - Datasource URL must use prisma:// protocol when --data-proxy, --accelerate, or --no-engine is used
   * - No valid API key found
   */
  P5002 = "P5002",

  /**
   * Requested resource does not exist.
   */
  P5003 = "P5003",

  /**
   * The feature is not yet implemented:
   * - beforeExit event is not yet supported
   */
  P5004 = "P5004",

  /**
   * Schema needs to be uploaded.
   */
  P5005 = "P5005",

  /**
   * Unknown server error.
   */
  P5006 = "P5006",

  /**
   * Unauthorized, check your connection string.
   */
  P5007 = "P5007",

  /**
   * Usage exceeded, retry again later.
   */
  P5008 = "P5008",

  /**
   * Request timed out.
   */
  P5009 = "P5009",

  /**
   * Cannot fetch data from service.
   */
  P5010 = "P5010",

  /**
   * Request parameters are invalid.
   * Note:
   * - You see error P5000 when the server cannot understand the request.
   * - In comparison, P5011 indicates that the server understands the request but rejects it
   *   due to failed validation checks, such as parameters being out of range.
   */
  P5011 = "P5011",

  /**
   * Engine version is not supported.
   */
  P5012 = "P5012",

  /**
   * Engine not started: healthcheck timeout.
   */
  P5013 = "P5013",

  /**
   * Unknown engine startup error (contains message and logs).
   */
  P5014 = "P5014",

  /**
   * Interactive transaction error:
   * - Could not parse interactive transaction ID
   * - Could not find Query Engine for the specified host and transaction ID
   * - Could not start interactive transaction
   */
  P5015 = "P5015"
}

/**
 * Prisma Accelerate Error Codes
 */
enum PrismaAccelerateError {
  /**
   * The global timeout of Accelerate has been exceeded.
   */
  P6004 = "P6004",

  /**
   * The user supplied invalid parameters. Currently only relevant for transaction methods.
   * For example, setting a timeout that is too high.
   */
  P6005 = "P6005",

  /**
   * The global response size limit of Accelerate has been exceeded.
   */
  P6009 = "P6009"
}

/**
 * Combine all Prisma error codes into one enum
 */
const PrismaError = {
  ...PrismaClientError,
  ...PrismaClientQueryEngineError,
  ...PrismaMigrateSchemaEngineError,
  ...PrismaDatabaseProxyError,
  ...PrismaAccelerateError
};

type PrismaError = typeof PrismaError;

export default PrismaError;
