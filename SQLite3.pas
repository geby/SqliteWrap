unit SQLite3;

{
  Simplified interface for SQLite.

  V2.1.0  2012-12-19
    Added Online-Backup API
    Extended Error Codes

  V2.0.0  2010-07-29
    Ported to D2009 Unicode by Roger Lascelles (support@veecad.com)

  History
  Reworked by Lukáš Gebauer at http://www.ararat.cz/doku.php/en:sqlitewrap.
  Updated for Sqlite 3 by Tim Anderson (tim@itwriting.com)
  Note: NOT COMPLETE for version 3, just minimal functionality
  Adapted from file created by Pablo Pissanetzky (pablo@myhtpc.net)
  which was based on SQLite.pas by Ben Hochstrasser (bhoc@surfeu.ch)

  Require: Delphi 6+, FreePascal
  Sqlite 3.7.1+
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$H+}            (* use AnsiString *)
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

const
{$IF Defined(MSWINDOWS)}
  SQLiteDLL = 'sqlite3.dll';
{$ELSEIF Defined(DARWIN)}
  SQLiteDLL = 'libsqlite3.dylib';
  {$linklib libsqlite3}
{$ELSEIF Defined(UNIX)}
  SQLiteDLL = 'sqlite3.so';
{$IFEND}

const
  SQLITE_OK          =  0; // Successful result
  //Standard error codes
  SQLITE_ERROR       =  1; // SQL error or missing database
  SQLITE_INTERNAL    =  2; // An internal logic error in SQLite
  SQLITE_PERM        =  3; // Access permission denied
  SQLITE_ABORT       =  4; // Callback routine requested an abort
  SQLITE_BUSY        =  5; // The database file is locked
  SQLITE_LOCKED      =  6; // A table in the database is locked
  SQLITE_NOMEM       =  7; // A malloc() failed
  SQLITE_READONLY    =  8; // Attempt to write a readonly database
  SQLITE_INTERRUPT   =  9; // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR       = 10; // Some kind of disk I/O error occurred
  SQLITE_CORRUPT     = 11; // The database disk image is malformed
  SQLITE_NOTFOUND    = 12; // (Internal Only) Table or record not found
  SQLITE_FULL        = 13; // Insertion failed because database is full
  SQLITE_CANTOPEN    = 14; // Unable to open the database file
  SQLITE_PROTOCOL    = 15; // Database lock protocol error
  SQLITE_EMPTY       = 16; // Database is empty
  SQLITE_SCHEMA      = 17; // The database schema changed
  SQLITE_TOOBIG      = 18; // Too much data for one row of a table
  SQLITE_CONSTRAINT  = 19; // Abort due to contraint violation
  SQLITE_MISMATCH    = 20; // Data type mismatch
  SQLITE_MISUSE      = 21; // Library used incorrectly
  SQLITE_NOLFS       = 22; // Uses OS features not supported on host
  SQLITE_AUTH        = 23; // Authorization denied
  SQLITE_FORMAT      = 24; // Auxiliary database format error
  SQLITE_RANGE       = 25; // 2nd parameter to sqlite3_bind out of range
  SQLITE_NOTADB      = 26; // File opened that is not a database file
  SQLITE_ROW         = 100; // sqlite3_step() has another row ready
  SQLITE_DONE        = 101; // sqlite3_step() has finished executing

  //Extended error codes
  SQLITE_IOERR_READ              = SQLITE_IOERR or (1 shl 8);
  SQLITE_IOERR_SHORT_READ        = SQLITE_IOERR or (2 shl 8);
  SQLITE_IOERR_WRITE             = SQLITE_IOERR or (3 shl 8);
  SQLITE_IOERR_FSYNC             = SQLITE_IOERR or (4 shl 8);
  SQLITE_IOERR_DIR_FSYNC         = SQLITE_IOERR or (5 shl 8);
  SQLITE_IOERR_TRUNCATE          = SQLITE_IOERR or (6 shl 8);
  SQLITE_IOERR_FSTAT             = SQLITE_IOERR or (7 shl 8);
  SQLITE_IOERR_UNLOCK            = SQLITE_IOERR or (8 shl 8);
  SQLITE_IOERR_RDLOCK            = SQLITE_IOERR or (9 shl 8);
  SQLITE_IOERR_DELETE            = SQLITE_IOERR or (10 shl 8);
  SQLITE_IOERR_BLOCKED           = SQLITE_IOERR or (11 shl 8);
  SQLITE_IOERR_NOMEM             = SQLITE_IOERR or (12 shl 8);
  SQLITE_IOERR_ACCESS            = SQLITE_IOERR or (13 shl 8);
  SQLITE_IOERR_CHECKRESERVEDLOCK = SQLITE_IOERR or (14 shl 8);
  SQLITE_IOERR_LOCK              = SQLITE_IOERR or (15 shl 8);
  SQLITE_IOERR_CLOSE             = SQLITE_IOERR or (16 shl 8);
  SQLITE_IOERR_DIR_CLOSE         = SQLITE_IOERR or (17 shl 8);
  SQLITE_IOERR_SHMOPEN           = SQLITE_IOERR or (18 shl 8);
  SQLITE_IOERR_SHMSIZE           = SQLITE_IOERR or (19 shl 8);
  SQLITE_IOERR_SHMLOCK           = SQLITE_IOERR or (20 shl 8);
  SQLITE_IOERR_SHMMAP            = SQLITE_IOERR or (21 shl 8);
  SQLITE_IOERR_SEEK              = SQLITE_IOERR or (22 shl 8);
  SQLITE_IOERR_DELETE_NOENT      = SQLITE_IOERR or (23 shl 8);
  SQLITE_LOCKED_SHAREDCACHE      = SQLITE_LOCKED or  (1 shl 8);
  SQLITE_BUSY_RECOVERY           = SQLITE_BUSY   or  (1 shl 8);
  SQLITE_CANTOPEN_NOTEMPDIR      = SQLITE_CANTOPEN or (1 shl 8);
  SQLITE_CANTOPEN_ISDIR          = SQLITE_CANTOPEN or (2 shl 8);
  SQLITE_CANTOPEN_FULLPATH       = SQLITE_CANTOPEN or (3 shl 8);
  SQLITE_CORRUPT_VTAB            = SQLITE_CORRUPT or (1 shl 8);
  SQLITE_READONLY_RECOVERY       = SQLITE_READONLY or (1 shl 8);
  SQLITE_READONLY_CANTLOCK       = SQLITE_READONLY or (2 shl 8);
  SQLITE_ABORT_ROLLBACK          = SQLITE_ABORT or (2 shl 8);

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE_TEXT    = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  SQLITE_UTF8     = 1;
  SQLITE_UTF16    = 2;
  SQLITE_UTF16BE  = 3;
  SQLITE_UTF16LE  = 4;
  SQLITE_ANY      = 5;

  SQLITE_STATIC    {: TSQLite3Destructor} = Pointer(0);
  SQLITE_TRANSIENT {: TSQLite3Destructor} = Pointer(-1);

type
  TSQLiteDB = Pointer;
  TSQLiteResult = ^PAnsiChar;
  TSQLiteStmt = Pointer;
  TSQLiteBackup = Pointer;

type
  PPAnsiCharArray = ^TPAnsiCharArray; 
  TPAnsiCharArray = array[0 .. (MaxInt div SizeOf(PAnsiChar))-1] of PAnsiChar;

type
  TSQLiteExecCallback = function(UserData: Pointer; NumCols: integer; ColValues:
    PPAnsiCharArray; ColNames: PPAnsiCharArray): integer; cdecl;
  TSQLiteBusyHandlerCallback = function(UserData: Pointer; P2: integer): integer; cdecl;

  //function prototype for define own collate
  TCollateXCompare = function(UserData: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;

function SQLite3_Initialize(): integer; cdecl; external SQLiteDLL name 'sqlite3_initialize';
function SQLite3_Shutdown(): integer; cdecl; external SQLiteDLL name 'sqlite3_shutdown';

function SQLite3_Open(filename: PAnsiChar; var db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_open';
function SQLite3_Close(db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_close';
function SQLite3_Exec(db: TSQLiteDB; SQLStatement: PAnsiChar; CallbackPtr: TSQLiteExecCallback; UserData: Pointer; var ErrMsg: PAnsiChar): integer; cdecl; external SQLiteDLL name 'sqlite3_exec';
function SQLite3_Version(): PAnsiChar; cdecl; external SQLiteDLL name 'sqlite3_libversion';
function SQLite3_ErrMsg(db: TSQLiteDB): PAnsiChar; cdecl; external SQLiteDLL name 'sqlite3_errmsg';
function SQLite3_ErrCode(db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_errcode';
function SQLite3_Extended_ErrCode(db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_extended_errcode';
function SQLite3_ErrStr(ErrorCode: integer): PAnsiChar; cdecl; external SQLiteDLL name 'sqlite3_errstr';
procedure SQlite3_Free(P: PAnsiChar); cdecl; external SQLiteDLL name 'sqlite3_free';
function SQLite3_GetTable(db: TSQLiteDB; SQLStatement: PAnsiChar; var ResultPtr: TSQLiteResult; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PAnsiChar): integer; cdecl; external SQLiteDLL name 'sqlite3_get_table';
procedure SQLite3_FreeTable(Table: TSQLiteResult); cdecl; external SQLiteDLL name 'sqlite3_free_table';
function SQLite3_Complete(P: PAnsiChar): boolean; cdecl; external SQLiteDLL name 'sqlite3_complete';
function SQLite3_LastInsertRowID(db: TSQLiteDB): int64; cdecl; external SQLiteDLL name 'sqlite3_last_insert_rowid';
procedure SQLite3_Interrupt(db: TSQLiteDB); cdecl; external SQLiteDLL name 'sqlite3_interrupt';
procedure SQLite3_BusyHandler(db: TSQLiteDB; CallbackPtr: TSQLiteBusyHandlerCallback; UserData: Pointer); cdecl; external SQLiteDLL name 'sqlite3_busy_handler';
procedure SQLite3_BusyTimeout(db: TSQLiteDB; TimeOut: integer); cdecl; external SQLiteDLL name 'sqlite3_busy_timeout';
function SQLite3_Changes(db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_changes';
function SQLite3_TotalChanges(db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_total_changes';
function SQLite3_Prepare(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl; external SQLiteDLL name 'sqlite3_prepare';
function SQLite3_Prepare_v2(db: TSQLiteDB; SQLStatement: PAnsiChar; nBytes: integer; var hStmt: TSqliteStmt; var pzTail: PAnsiChar): integer; cdecl; external SQLiteDLL name 'sqlite3_prepare_v2';
function SQLite3_ColumnCount(hStmt: TSqliteStmt): integer; cdecl; external SQLiteDLL name 'sqlite3_column_count';
function SQLite3_ColumnName(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl; external SQLiteDLL name 'sqlite3_column_name';
function SQLite3_ColumnDeclType(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl; external SQLiteDLL name 'sqlite3_column_decltype';
function SQLite3_Step(hStmt: TSqliteStmt): integer; cdecl; external SQLiteDLL name 'sqlite3_step';
function SQLite3_DataCount(hStmt: TSqliteStmt): integer; cdecl; external SQLiteDLL name 'sqlite3_data_count';

function SQLite3_ColumnBlob(hStmt: TSqliteStmt; ColNum: integer): pointer; cdecl; external SQLiteDLL name 'sqlite3_column_blob';
function SQLite3_ColumnBytes(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; external SQLiteDLL name 'sqlite3_column_bytes';
function SQLite3_ColumnDouble(hStmt: TSqliteStmt; ColNum: integer): double; cdecl; external SQLiteDLL name 'sqlite3_column_double';
function SQLite3_ColumnInt(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; external SQLiteDLL name 'sqlite3_column_int';
function SQLite3_ColumnText(hStmt: TSqliteStmt; ColNum: integer): PAnsiChar; cdecl; external SQLiteDLL name 'sqlite3_column_text';
function SQLite3_ColumnType(hStmt: TSqliteStmt; ColNum: integer): integer; cdecl; external SQLiteDLL name 'sqlite3_column_type';
function SQLite3_ColumnInt64(hStmt: TSqliteStmt; ColNum: integer): Int64; cdecl; external SQLiteDLL name 'sqlite3_column_int64';
function SQLite3_Finalize(hStmt: TSqliteStmt): integer; cdecl; external SQLiteDLL name 'sqlite3_finalize';
function SQLite3_Reset(hStmt: TSqliteStmt): integer; cdecl; external SQLiteDLL name 'sqlite3_reset';
function SQLite3_Get_Autocommit(db: TSQLiteDB): integer; cdecl; external SQLiteDLL name 'sqlite3_get_autocommit';
function SQLite3_enable_load_extension(db: TSQLiteDB; OnOff: integer): integer; cdecl; external SQLiteDLL name 'sqlite3_enable_load_extension';

const
  SQLITE_FCNTL_LOCKSTATE       = 1;
  SQLITE_GET_LOCKPROXYFILE     = 2;
  SQLITE_SET_LOCKPROXYFILE     = 3;
  SQLITE_LAST_ERRNO            = 4;
  SQLITE_FCNTL_SIZE_HINT       = 5;
  SQLITE_FCNTL_CHUNK_SIZE      = 6;
  SQLITE_FCNTL_FILE_POINTER    = 7;
  SQLITE_FCNTL_SYNC_OMITTED    = 8;
function SQLite3_file_control(db: TSQLiteDB; filename: PAnsiChar; option: integer; data: pointer): integer; cdecl; external SQLiteDLL name 'sqlite3_file_control';

// In the SQL strings input to sqlite3_prepare() and sqlite3_prepare16(),
// one or more literals can be replace by a wildcard "?" or ":N:" where
// N is an integer.  These value of these wildcard literals can be set
// using the routines listed below.
//
// In every case, the first parameter is a pointer to the sqlite3_stmt
// structure returned from sqlite3_prepare().  The second parameter is the
// index of the wildcard.  The first "?" has an index of 1.  ":N:" wildcards
// use the index N.
//
// The fifth parameter to sqlite3_bind_blob(), sqlite3_bind_text(), and
//sqlite3_bind_text16() is a destructor used to dispose of the BLOB or
//text after SQLite has finished with it.  If the fifth argument is the
// special value SQLITE_STATIC, then the library assumes that the information
// is in static, unmanaged space and does not need to be freed.  If the
// fifth argument has the value SQLITE_TRANSIENT, then SQLite makes its
// own private copy of the data.
// 
// The sqlite3_bind_* routine must be called before sqlite3_step() after
// an sqlite3_prepare() or sqlite3_reset().  Unbound wildcards are interpreted
// as NULL.
//

type
  TSQLite3Destructor = procedure(Ptr: Pointer); cdecl;

function sqlite3_bind_blob(hStmt: TSqliteStmt; ParamNum: integer;
  ptrData: pointer; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer;
cdecl; external SQLiteDLL name 'sqlite3_bind_blob';
function sqlite3_bind_text(hStmt: TSqliteStmt; ParamNum: integer;
  Text: PAnsiChar; numBytes: integer; ptrDestructor: TSQLite3Destructor): integer;
cdecl; external SQLiteDLL name 'sqlite3_bind_text';
function sqlite3_bind_double(hStmt: TSqliteStmt; ParamNum: integer; Data: Double): integer;
  cdecl; external SQLiteDLL name 'sqlite3_bind_double';
function sqlite3_bind_int(hStmt: TSqLiteStmt; ParamNum: integer; Data: integer): integer;
  cdecl; external SQLiteDLL name 'sqlite3_bind_int';
function sqlite3_bind_int64(hStmt: TSqliteStmt; ParamNum: integer; Data: int64): integer;
  cdecl; external SQLiteDLL name 'sqlite3_bind_int64';
function sqlite3_bind_null(hStmt: TSqliteStmt; ParamNum: integer): integer;
  cdecl; external SQLiteDLL name 'sqlite3_bind_null';

function sqlite3_bind_parameter_index(hStmt: TSqliteStmt; zName: PAnsiChar): integer;
  cdecl; external SQLiteDLL name 'sqlite3_bind_parameter_index';
function sqlite3_clear_bindings(hStmt: TSqliteStmt): integer;
  cdecl; external SQLiteDLL name 'sqlite3_clear_bindings';

function sqlite3_enable_shared_cache(Value: integer): integer; cdecl; external SQLiteDLL name 'sqlite3_enable_shared_cache';

//user collate definiton
function SQLite3_create_collation(db: TSQLiteDB; Name: PAnsiChar; eTextRep: integer;
  UserData: pointer; xCompare: TCollateXCompare): integer; cdecl; external SQLiteDLL name 'sqlite3_create_collation';

//Backup API
function sqlite3_backup_init(Dest: TSQLiteDB; const DestName: PAnsiChar;
  Source: TSQLiteDB; const SourceName: PAnsiChar): TSQLiteBackup; cdecl; external SQLiteDLL name 'sqlite3_backup_init';
function sqlite3_backup_step(p: TSQLiteBackup; nPage: integer): integer; cdecl; external SQLiteDLL name 'sqlite3_backup_step';
function sqlite3_backup_finish(p: TSQLiteBackup): integer; cdecl; external SQLiteDLL name 'sqlite3_backup_finish';
function sqlite3_backup_remaining(p: TSQLiteBackup): integer; cdecl; external SQLiteDLL name 'sqlite3_backup_remaining';
function sqlite3_backup_pagecount(p: TSQLiteBackup): integer; cdecl; external SQLiteDLL name 'sqlite3_backup_pagecount';

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): String;
function SQLiteErrorType(SQLiteErrorCode: Integer): String;

implementation

uses
  SysUtils;

function SQLiteFieldType(SQLiteFieldTypeCode: Integer): String;
begin
  case SQLiteFieldTypeCode of
    SQLITE_INTEGER: Result := 'Integer';
    SQLITE_FLOAT: Result := 'Float';
    SQLITE_TEXT: Result := 'Text';
    SQLITE_BLOB: Result := 'Blob';
    SQLITE_NULL: Result := 'Null';
  else
    Result := 'Unknown SQLite Field Type Code "' + IntToStr(SQLiteFieldTypeCode) + '"';
  end;
end;

function SQLiteErrorType(SQLiteErrorCode: Integer): String;
begin
  case SQLiteErrorCode of
    SQLITE_OK          : Result :='SQLITE_OK';
    SQLITE_ERROR       : Result :='SQLITE_ERROR';
    SQLITE_INTERNAL    : Result :='SQLITE_INTERNAL';
    SQLITE_PERM        : Result :='SQLITE_PERM';
    SQLITE_ABORT       : Result :='SQLITE_ABORT';
    SQLITE_BUSY        : Result :='SQLITE_BUSY';
    SQLITE_LOCKED      : Result :='SQLITE_LOCKED';
    SQLITE_NOMEM       : Result :='SQLITE_NOMEM';
    SQLITE_READONLY    : Result :='SQLITE_READONLY';
    SQLITE_INTERRUPT   : Result :='SQLITE_INTERRUPT';
    SQLITE_IOERR       : Result :='SQLITE_IOERR';
    SQLITE_CORRUPT     : Result :='SQLITE_CORRUPT';
    SQLITE_NOTFOUND    : Result :='SQLITE_NOTFOUND';
    SQLITE_FULL        : Result :='SQLITE_FULL';
    SQLITE_CANTOPEN    : Result :='SQLITE_CANTOPEN';
    SQLITE_PROTOCOL    : Result :='SQLITE_PROTOCOL';
    SQLITE_EMPTY       : Result :='SQLITE_EMPTY';
    SQLITE_SCHEMA      : Result :='SQLITE_SCHEMA';
    SQLITE_TOOBIG      : Result :='SQLITE_TOOBIG';
    SQLITE_CONSTRAINT  : Result :='SQLITE_CONSTRAINT';
    SQLITE_MISMATCH    : Result :='SQLITE_MISMATCH';
    SQLITE_MISUSE      : Result :='SQLITE_MISUSE';
    SQLITE_NOLFS       : Result :='SQLITE_NOLFS';
    SQLITE_AUTH        : Result :='SQLITE_AUTH';
    SQLITE_FORMAT      : Result :='SQLITE_FORMAT';
    SQLITE_RANGE       : Result :='SQLITE_RANGE';
    SQLITE_NOTADB      : Result :='SQLITE_NOTADB';
    SQLITE_ROW         : Result :='SQLITE_ROW';
    SQLITE_DONE        : Result :='SQLITE_DONE';
    SQLITE_IOERR_READ              : Result :='SQLITE_IOERR_READ';
    SQLITE_IOERR_SHORT_READ        : Result :='SQLITE_IOERR_SHORT_READ';
    SQLITE_IOERR_WRITE             : Result :='SQLITE_IOERR_WRITE';
    SQLITE_IOERR_FSYNC             : Result :='SQLITE_IOERR_FSYNC';
    SQLITE_IOERR_DIR_FSYNC         : Result :='SQLITE_IOERR_DIR_FSYNC';
    SQLITE_IOERR_TRUNCATE          : Result :='SQLITE_IOERR_TRUNCATE';
    SQLITE_IOERR_FSTAT             : Result :='SQLITE_IOERR_FSTAT';
    SQLITE_IOERR_UNLOCK            : Result :='SQLITE_IOERR_UNLOCK';
    SQLITE_IOERR_RDLOCK            : Result :='SQLITE_IOERR_RDLOCK';
    SQLITE_IOERR_DELETE            : Result :='SQLITE_IOERR_DELETE';
    SQLITE_IOERR_BLOCKED           : Result :='SQLITE_IOERR_BLOCKED';
    SQLITE_IOERR_NOMEM             : Result :='SQLITE_IOERR_NOMEM';
    SQLITE_IOERR_ACCESS            : Result :='SQLITE_IOERR_ACCESS';
    SQLITE_IOERR_CHECKRESERVEDLOCK : Result :='SQLITE_IOERR_CHECKRESERVEDLOCK';
    SQLITE_IOERR_LOCK              : Result :='SQLITE_IOERR_LOCK';
    SQLITE_IOERR_CLOSE             : Result :='SQLITE_IOERR_CLOSE';
    SQLITE_IOERR_DIR_CLOSE         : Result :='SQLITE_IOERR_DIR_CLOSE';
    SQLITE_IOERR_SHMOPEN           : Result :='SQLITE_IOERR_SHMOPEN';
    SQLITE_IOERR_SHMSIZE           : Result :='SQLITE_IOERR_SHMSIZE';
    SQLITE_IOERR_SHMLOCK           : Result :='SQLITE_IOERR_SHMLOCK';
    SQLITE_IOERR_SHMMAP            : Result :='SQLITE_IOERR_SHMMAP';
    SQLITE_IOERR_SEEK              : Result :='SQLITE_IOERR_SEEK';
    SQLITE_IOERR_DELETE_NOENT      : Result :='SQLITE_IOERR_DELETE_NOENT';
    SQLITE_LOCKED_SHAREDCACHE      : Result :='SQLITE_LOCKED_SHAREDCACHE';
    SQLITE_BUSY_RECOVERY           : Result :='SQLITE_BUSY_RECOVERY';
    SQLITE_CANTOPEN_NOTEMPDIR      : Result :='SQLITE_CANTOPEN_NOTEMPDIR';
    SQLITE_CANTOPEN_ISDIR          : Result :='SQLITE_CANTOPEN_ISDIR';
    SQLITE_CANTOPEN_FULLPATH       : Result :='SQLITE_CANTOPEN_FULLPATH';
    SQLITE_CORRUPT_VTAB            : Result :='SQLITE_CORRUPT_VTAB';
    SQLITE_READONLY_RECOVERY       : Result :='SQLITE_READONLY_RECOVERY';
    SQLITE_READONLY_CANTLOCK       : Result :='SQLITE_READONLY_CANTLOCK';
    SQLITE_ABORT_ROLLBACK          : Result :='SQLITE_ABORT_ROLLBACK';
  else
    Result := 'Unknown SQLite Error Code "' + IntToStr(SQLiteErrorCode) + '"';
  end;
end;

end.

