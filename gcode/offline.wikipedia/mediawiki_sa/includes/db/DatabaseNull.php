<?php
/**
 * This is the Null database abstraction layer.
 *
 * @file
 * @ingroup Database
 */

class NullField {
	private $name, $tablename, $type, $nullable, $max_length, $deferred, $deferrable, $conname;

	static function fromText($db, $table, $field) {
	global $wgDBmwschema;

		$q = <<<SQL
SELECT
 attnotnull, attlen, COALESCE(conname, '') AS conname,
 COALESCE(condeferred, 'f') AS deferred,
 COALESCE(condeferrable, 'f') AS deferrable,
 CASE WHEN typname = 'int2' THEN 'smallint'
  WHEN typname = 'int4' THEN 'integer'
  WHEN typname = 'int8' THEN 'bigint'
  WHEN typname = 'bpchar' THEN 'char'
 ELSE typname END AS typname
FROM pg_class c
JOIN pg_namespace n ON (n.oid = c.relnamespace)
JOIN pg_attribute a ON (a.attrelid = c.oid)
JOIN pg_type t ON (t.oid = a.atttypid)
LEFT JOIN pg_constraint o ON (o.conrelid = c.oid AND a.attnum = ANY(o.conkey) AND o.contype = 'f')
WHERE relkind = 'r'
AND nspname=%s
AND relname=%s
AND attname=%s;
SQL;

		$table = $db->tableName( $table );
		$res = $db->query(
			sprintf( $q,
				$db->addQuotes( $wgDBmwschema ),
				$db->addQuotes( $table ),
				$db->addQuotes( $field )
			)
		);
		$row = $db->fetchObject( $res );
		if ( !$row ) {
			return null;
		}
		$n = new NullField;
		$n->type = $row->typname;
		$n->nullable = ( $row->attnotnull == 'f' );
		$n->name = $field;
		$n->tablename = $table;
		$n->max_length = $row->attlen;
		$n->deferrable = ( $row->deferrable == 't' );
		$n->deferred = ( $row->deferred == 't' );
		$n->conname = $row->conname;
		return $n;
	}

	function name() {
		return $this->name;
	}

	function tableName() {
		return $this->tablename;
	}

	function type() {
		return $this->type;
	}

	function nullable() {
		return $this->nullable;
	}

	function maxLength() {
		return $this->max_length;
	}

	function is_deferrable() {
		return $this->deferrable;
	}

	function is_deferred() {
		return $this->deferred;
	}

	function conname() {
		return $this->conname;
	}

}

/**
 * @ingroup Database
 */
class DatabaseNull extends DatabaseBase {
	var $mInsertId = null;
	var $mLastResult = null;
	var $numeric_version = null;
	var $mAffectedRows = null;

	function __construct( $server = false, $user = false, $password = false, $dbName = false,
		$flags = 0 )
	{
          $this->mOpened = true;
          
	}

	function getType() {
		return 'null';
	}

	function cascadingDeletes() {
		return true;
	}
	function cleanupTriggers() {
		return true;
	}
	function strictIPs() {
		return true;
	}
	function realTimestamps() {
		return true;
	}
	function implicitGroupby() {
		return false;
	}
	function implicitOrderby() {
		return false;
	}
	function searchableIPs() {
		return true;
	}
	function functionalIndexes() {
		return true;
	}

	function hasConstraint( $name ) {
          return false;
	}

	static function newFromParams( $server, $user, $password, $dbName, $flags = 0 ) {
		return new DatabaseNull( $server, $user, $password, $dbName, $flags );
	}

	/**
	 * Usually aborts on failure
	 */
	function open( $server, $user, $password, $dbName ) {
          return true;
	}

	function makeConnectionString( $vars ) {
		$s = '';
		foreach ( $vars as $name => $value ) {
			$s .= "$name='" . str_replace( "'", "\\'", $value ) . "' ";
		}
		return $s;
	}


	function initial_setup( $superuser, $password, $dbName ) {
          return true;
        }

	function setup_plpgsql() {
        }

	/**
	 * Closes a database connection, if it is open
	 * Returns success, true if already closed
	 */
	function close() {
          return true;
	}

	function doQuery( $sql ) {
          return null;
	}

	function queryIgnore( $sql, $fname = '' ) {
          return null;
	}

	function freeResult( $res ) {

	}

	function fetchObject( $res ) {
          return null;
	}

	function fetchRow( $res ) {
          return null;
	}

	function numRows( $res ) {
          return 0;
	}

	function numFields( $res ) {
          return 0;
	}

	function fieldName( $res, $n ) {
          return "";
	}

	/**
	 * This must be called after nextSequenceVal
	 */
	function insertId() {
          return null;
	}

	function dataSeek( $res, $row ) {
          return null;
        }

	function lastError() {
          wfDebugDieBacktrace( "shit" );
          return "hello";
	}
	function lastErrno() {
          return 0;
	}

	function affectedRows() {
          return 0;
	}

	/**
	 * Estimate rows in dataset
	 * Returns estimated count, based on EXPLAIN output
	 * This is not necessarily an accurate estimate, so use sparingly
	 * Returns -1 if count cannot be found
	 * Takes same arguments as Database::select()
	 */
	function estimateRowCount( $table, $vars = '*', $conds='', $fname = 'DatabaseNull::estimateRowCount', $options = array() ) {
          return 0;
        }

	/**
	 * Returns information about an index
	 * If errors are explicitly ignored, returns NULL on failure
	 */
	function indexInfo( $table, $index, $fname = 'DatabaseNull::indexInfo' ) {
		return false;
	}

	function indexUnique( $table, $index, $fname = 'DatabaseNull::indexUnique' ) {
          return true;
	}

	/**
	 * INSERT wrapper, inserts an array into a table
	 *
	 * $args may be a single associative array, or an array of these with numeric keys,
	 * for multi-row insert (Null version 8.2 and above only).
	 *
	 * @param $table   String: Name of the table to insert to.
	 * @param $args    Array: Items to insert into the table.
	 * @param $fname   String: Name of the function, for profiling
	 * @param $options String or Array. Valid options: IGNORE
	 *
	 * @return bool Success of insert operation. IGNORE always returns true.
	 */
	function insert( $table, $args, $fname = 'DatabaseNull::insert', $options = array() ) {
          return true;
        }

	/**
	 * INSERT SELECT wrapper
	 * $varMap must be an associative array of the form array( 'dest1' => 'source1', ...)
	 * Source items may be literals rather then field names, but strings should be quoted with Database::addQuotes()
	 * $conds may be "*" to copy the whole table
	 * srcTable may be an array of tables.
	 * @todo FIXME: implement this a little better (seperate select/insert)?
	 */
	function insertSelect( $destTable, $srcTable, $varMap, $conds, $fname = 'DatabaseNull::insertSelect',
		$insertOptions = array(), $selectOptions = array() )
	{
          return true;
	}

	function tableName( $name ) {
          return "";
	}

	/**
	 * Return the next in a sequence, save the value for retrieval via insertId()
	 */
	function nextSequenceValue( $seqName ) {
          return null;
	}

	/**
	 * Return the current value of a sequence. Assumes it has been nextval'ed in this session.
	 */
	function currentSequenceValue( $seqName ) {
          return 1;
	}

	/**
	 * REPLACE query wrapper
	 * Null simulates this with a DELETE followed by INSERT
	 * $row is the row to insert, an associative array
	 * $uniqueIndexes is an array of indexes. Each element may be either a
	 * field name or an array of field names
	 *
	 * It may be more efficient to leave off unique indexes which are unlikely to collide.
	 * However if you do this, you run the risk of encountering errors which wouldn't have
	 * occurred in MySQL
	 */
	function replace( $table, $uniqueIndexes, $rows, $fname = 'DatabaseNull::replace' ) {
          return;
	}

	# DELETE where the condition is a join
	function deleteJoin( $delTable, $joinTable, $delVar, $joinVar, $conds, $fname = 'DatabaseNull::deleteJoin' ) {
          return;
	}

	# Returns the size of a text field, or -1 for "unlimited"
	function textFieldSize( $table, $field ) {
          return -1;
	}

	function limitResult( $sql, $limit, $offset = false ) {
          return "";
	}

	function wasDeadlock() {
          return false;
	}

	function duplicateTableStructure( $oldName, $newName, $temporary = false, $fname = 'DatabaseNull::duplicateTableStructure' ) {
          return true;
	}

	function timestamp( $ts = 0 ) {
		return wfTimestamp( TS_NULL, $ts );
	}

	/**
	 * Return aggregated value function call
	 */
	function aggregateValue( $valuedata, $valuename = 'value' ) {
          return nulll;
	}

	function reportQueryError( $error, $errno, $sql, $fname, $tempIgnore = false ) {
		// Ignore errors during error handling to avoid infinite recursion
          
	}

	/**
	 * @return string wikitext of a link to the server software's web site
	 */
	public static function getSoftwareLink() {
		return '[http://www.nullql.org/ NullQL]';
	}

	/**
	 * @return string Version information from the database
	 */
	function getServerVersion() {
          return "7.8.8";
	}

	/**
	 * Query whether a given relation exists (in the given schema, or the
	 * default mw one if not given)
	 */
	function relationExists( $table, $types, $schema = false ) {
          return true;
	}

	/**
	 * For backward compatibility, this function checks both tables and
	 * views.
	 */
	function tableExists( $table, $schema = false ) {
          return true;
	}

	function sequenceExists( $sequence, $schema = false ) {
          return true;
	}

	function triggerExists( $table, $trigger ) {
          return true;
	}

	function ruleExists( $table, $rule ) {
          return true;
	}

	function constraintExists( $table, $constraint ) {
          return true;
	}

	/**
	 * Query whether a given schema exists. Returns the name of the owner
	 */
	function schemaExists( $schema ) {
          return true;
	}

	function fieldInfo( $table, $field ) {
          return null;
	}

	/**
	 * pg_field_type() wrapper
	 */
	function fieldType( $res, $index ) {
          return null;
	}

	/* Not even sure why this is used in the main codebase... */
	function limitResultForUpdate( $sql, $num ) {
          return null;
	}

	function setup_database() {
          return;
	}

	function encodeBlob( $b ) {
          return null;
	}

	function decodeBlob( $b ) {
          return null;
	}

	function strencode( $s ) { # Should not be called by us
            return null;
	}

	function addQuotes( $s ) {
          return "";
	}

	function quote_ident( $s ) {
          return "";
	}

	/**
	 * Null specific version of replaceVars.
	 * Calls the parent version in Database.php
	 *
	 * @private
	 *
	 * @param $ins String: SQL string, read from a stream (usually tables.sql)
	 *
	 * @return string SQL string
	 */
	protected function replaceVars( $ins ) {
          return "";
	}

	/**
	 * Various select options
	 *
	 * @private
	 *
	 * @param $options Array: an associative array of options to be turned into
	 *              an SQL query, valid keys are listed in the function.
	 * @return array
	 */
	function makeSelectOptions( $options ) {
          return null;
	}

	function setFakeMaster( $enabled = true ) {}

	function getDBname() {
		return $this->mDBname;
	}

	function getServer() {
		return $this->mServer;
	}

	function buildConcat( $stringList ) {
		return implode( ' || ', $stringList );
	}

	public function getSearchEngine() {
		return 'SearchNull';
	}
} // end DatabaseNull class
