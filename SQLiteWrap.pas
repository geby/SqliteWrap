{: @abstract(SQLite 3.x object based wrapper)

Can be used for object based access to SQLite3 databases.

Note: Requires Sqlite 3.7.1 and higher!

Designed for Delphi 6+ and Freepascal, Unicode support for Delphi 2009+

  V3.0.0
    Rework Unicode support. Library used on <D2007 and >D2009 returning same
    Ansistring data. But on >D2009 you can read and write data by new Unicode
    based methods too. With minimum implicit chartset conversions for the best
    speed.

  V2.2.1
    Fixed "No argument for format 's'" error.

  V2.2.0
    Changed error handling, extended error codes are used.
    Usage of Online Backup API

  V2.1.2
    Fixed raising of Exceptions.

  V2.1.1
    Fixed call of ExecSQL for a statement what returning some rows.

  V2.1.0
    Added BLOB parameters and improved BLOB handling.
    Added TotalChanges property.
    Some bugfixes.

  V2.0.0  29 June 2010
    Ported to D2009 Unicode by Roger Lascelles (support@veecad.com)

  V1.0.0
    by Lukáš Gebauer at https://github.com/geby/SqliteWrap
    based on work by Tim Anderson (tim@itwriting.com)

UNICODE HANDLING:
Delphi 2009+
  Pass data as native UnicodeString. Datas are converted to SQLite native
  UTF-8 internally.

pre-Delphi 2009
  Pass data as UTF8String (it is AnsiString contains UTF8 datas) and you must
  convert data to UTF-8 explicitly!
  Pasing data by UTF8String typed variable made your source forward compatible
  with Delphi 2009+.


Sample usage:
@longcode(#
procedure sample;
var
  database: TSqliteDatabase;
  tab: TSqliteTable;
  s: utf8string;
begin
  database := TSqliteDatabase.Create('somedatabase.db3');
  try
    database.AddParamInt(':key', 123456);
    tab := database.GetTable('SELECT * FROM some_table WHERE ROWID=:key');
    try
      while not tab.EOF do
      begin
        s := tab.FieldAsString(tab.FieldIndex['ROWID']);
        //do something with 'S' variable...
        //...
        //...then go to nexr row.
        tab.next;
      end;
    finally
      tab.free;
    end;
  finally
    database.free;
  end;
end;
#)
}

unit SQLiteWrap;

interface
{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

{$IFDEF UNICODE}
  {$IFNDEF FPC}
    {$DEFINE SQUNI}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SQLite3, Classes, SysUtils;

type
  {: @abstract(Exception Class for SQLite based errors)}
  ESQLiteException = class(Exception)
  end;

  {: @abstract(Class for storing deferred parameters)
  Do not use it, it is used internally only!
  }
  TSQliteParam = class
  public
    name: UTF8String;
    valuetype: integer;
    valueinteger: int64;
    valuefloat: double;
    valuedata: UTF8String;
  end;

  {: @abstract(procedural prototype for @link(OnQuery) hook.)}
  THookQuery = procedure(Sender: TObject; const SQL: String) of object;

  TSQLiteTable = class;

  {: @abstract(Class for handling SQLite database)}
  TSQLiteDatabase = class(TObject)
  private
    fDB: TSQLiteDB;
    fParams: TList;
    FOnQuery: THookQuery;
    FBackupStop: boolean;
    procedure DBRaiseError(DBHandle: TSQLiteDB; s, SQL: String);
    procedure RaiseError(const s, SQL: String);
    procedure SetParams(const Stmt: TSQLiteStmt);
  protected
    procedure DoQuery(const value: String);
  public
    {: Class constructor. Pass filename of database. If databas not exists, then new one is created.
    If you pass empty string as filename, then temporary database is created.
    If you pass ':memory:' as filename, then temporary database in memory is created.

    Warning:
      Pass FileName as AnsiString (if you are not using Unicode Delphi version)
      or as UnicodeString (on Delphi 2009 and higher)!
    }
    constructor Create(const FileName: String);
    {: Class descructor. Call Free instead.}
    destructor Destroy; override;
    {: Run SQL command without result.
       You can call before functions AddParam* for set query parameters.}
    procedure ExecSQL(const SQL : String);
    {: Run SQL command with result.
       You can call before functions AddParam* for set query parameters.
       If you set PerpareOnly, then query is just prepared but first row is not fetched!}
    function GetTable(const SQL: String; PrepareOnly: Boolean = false): TSQLiteTable;
    {: Run SQL command and number from first field in first row is returned.
       You can call before functions AddParam* for set query parameters.}
    function GetTableValue(const SQL: String): int64;
    {: Run SQL command and value from first field in first row is returned.
       You can call before functions AddParam* for set query parameters.}
    function GetTableString(const SQL: String): AnsiString;
{$IFDEF SQUNI}
    function GetTableStringUnicode(const SQL: String): Utf8String;
{$ENDIF}
    {: Run SQL command and values from first field in each row is filled to stringlist.
       You can call before functions AddParam* for set query parameters.}
    procedure GetTableStrings(const SQL: String; const Value: TStrings);
{$IFDEF SQUNI}
    procedure GetTableStringsUnicode(const SQL: String; const Value: TStrings);
{$ENDIF}
    {: Return @True if database is in transaction state.}
    function InTransaction: Boolean;
    {: Start transaction. You can modify transaction type by Param parameter.
       If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested.}
    procedure Start(const name:String; const param: String = '');
    {: Commit transaction.
       If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested.}
    procedure Commit(const name:String);
    {: Rollback transaction.
       If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested.}
    procedure Rollback(const name:String);
    {: Get ROWID of last inserted row.}
    function LastInsertRowID: int64;
    {: Return number of modified rows by last query.}
    function LastChangedRows: int64;
    {: Return number of modified rows starting by opened database connection.}
    function TotalChanges: int64;
    {: Set wait timeout. if database is locked, then it wait this timeout.
       If database is not released within this timeout, then error is returned.}
    procedure SetTimeout(Value: integer);
    {: Return SQLite engine version.}
    function Version: String;
    {: Enable/disable suport for loaded extension libraries.}
    procedure EnableLoadExtension(value: boolean);
    {: Set chunk size for databaze file grow.}
    procedure SetChunkSize(DatabaseName: ansistring; value: integer);
    {: Add custom sorting procedure as new Collate.}
    procedure AddCustomCollate(name: String; xCompare: TCollateXCompare);
    {: Add collate named SYSTEM for correct data sorting by user's locale}
    Procedure AddSystemCollate;
    {: Clear all query parameters.}
    procedure ParamsClear;
    {: Add named query parameter of integer type.}
    procedure AddParamInt(const name: String; value: int64);
    {: Add named query parameter of floating-point type.}
    procedure AddParamFloat(const name: String; value: double);
    {: Add named query parameter of string or binary type.}
    procedure AddParamText(const name: String; const value: AnsiString);
{$IFDEF SQUNI}
    procedure AddParamTextUnicode(const name: String; const Value: Utf8String);
{$ENDIF}
    {: Add named query parameter with null value.}
    procedure AddParamNull(const name: String);
    {: Add named query parameter of BLOB type from the memory buffer.}
    procedure AddParamBlobPtr(const name: String; buffer: pointer; len: integer);
    {: Add named query parameter of BLOB type from the binary string.}
    procedure AddParamBlobText(const name: String; const value: AnsiString);
    {: Add named query parameter of BLOB type from the stream.}
    procedure AddParamBlob(const name: String; const value: TStream; len: integer);
    function DBLoadHandle(SourceDB: TSQliteDB): integer;
    function DBLoadFile(const FileName: String): integer;
    function DBSaveHandle(DestinationDB: TSQliteDB): integer;
    function DBSaveFile(const FileName: String): integer;
    function DBBackupHandle(DestinationDB: TSQliteDB): integer;
    function DBBackupFile(const FileName: String): integer;
    procedure DbBackupStop;
    {: SQLite database handler.}
    property DB: TSQLiteDB read fDB;
    {: Debug hook for log all called queries.}
    property OnQuery: THookQuery read FOnQuery write FOnQuery;
  end;

  {: @abstract(Class for handling SQLite query result)}
  TSQLiteTable = class(TObject)
  private
    fColCount: cardinal;
    fCols: TStringList;
    fRow: cardinal;
    fEOF: boolean;
    fStmt: TSQLiteStmt;
    fDB: TSQLiteDatabase;
    fSQL: String;
    function GetFields(I: cardinal): AnsiString;
    function GetFieldsUnicode(I: cardinal): RawByteString;
    function GetColumns(I: integer): String;
    function GetFieldByName(FieldName: String): String;
    function GetFieldIndex(FieldName: String): integer;
  public
    {: Class constructor. Called internally by @link(TSqliteDatabase)}
    constructor Create(const DB: TSQLiteDatabase; const SQL: String; PrepareOnly: Boolean = false);
    {: Class descructor. Call Free instead.}
    destructor Destroy; override;
    {: Read field from current row as integer.}
    function FieldAsInteger(I: cardinal): int64;
    {: Read field from current row as blob to memory stream.}
    function FieldAsBlob(I: cardinal): TMemoryStream;
    {: Read field from current row as pointer to memory.}
    function FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): Pointer;
    {: Read field from current row as blob to AnsiString.}
    function FieldAsBlobText(I: cardinal): AnsiString;
    {: Test if field from current row contains null value.}
    function FieldIsNull(I: cardinal): boolean;
    {: Read field from current row as string.}
    function FieldAsString(I: cardinal): AnsiString;
{$IFDEF SQUNI}
    function FieldAsStringUnicode(I: cardinal): Utf8String;
{$ENDIF}
    {: Read field from current row as floating-point.}
    function FieldAsDouble(I: cardinal): double;
    {: Go to next row.}
    function Next: boolean;
    {: Reset all query params.}
    procedure ParamsClear;
    {: Reset current result set. After this you can set new query parameters values
       and call prepared query again by @link(next)}
    procedure Reset;
    {: Add named query parameter of integer type.}
    procedure AddParamInt(const name: String; value: int64);
    {: Add named query parameter of floating-point type.}
    procedure AddParamFloat(const name: String; value: double);
    {: Add named query parameter of string or binary type.}
    procedure AddParamText(const name: String; const value: AnsiString);
{$IFDEF SQUNI}
    procedure AddParamTextUnicode(const name: String; const Value: Utf8String);
{$ENDIF}
    {: Add named query parameter with null value.}
    procedure AddParamNull(const name: String);
    {: Add named query parameter of BLOB type from memory buffer.}
    procedure AddParamBlobPtr(const name: String; buffer: pointer; len: integer);
    {: Add named query parameter of BLOB type from binary string.}
    procedure AddParamBlobText(const name: String; const value: AnsiString);
    {: Add named query parameter of BLOB type from stream.}
    procedure AddParamBlob(const name: String; const value: TStream; len: integer);
    {: Return value of some field in current row.}
    property Fields[I: cardinal]: AnsiString read GetFields;
    {: Return value of named field in current row.}
    property FieldByName[FieldName: String]: String read GetFieldByName;
    {: Return index of some named field.}
    property FieldIndex[FieldName: String]: integer read GetFieldIndex;
    {: Return field type of some field.}
    property Columns[I: integer]: String read GetColumns;
    {: Indicate last row in result set.}
    property EOF: boolean read FEOF;
    {: Return number of fields in row.}
    property ColCount: cardinal read fColCount;
    {: Number of current row.}
    property Row: cardinal read fRow;
  end;

implementation

resourcestring
  c_unknown = 'Unknown error';
  c_failopen = 'Failed to open database';
  c_error = slinebreak + '%s [%d]: %s'+slinebreak+'%s';
  c_nomessage = 'Unknown error description';
  c_errorsql = 'Error executing SQL';
  c_errorprepare = 'Could not prepare SQL statement';
  c_errorexec = 'Error executing SQL statement';
  c_errorempty = 'Field %s Not found. Empty dataset';
  c_errorfield = 'Field not found in dataset: %s';
  c_errordata = 'Could not retrieve data';
  c_backuperror = 'Could not copy database';

{$IFDEF MSWINDOWS}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len,
    PWideChar(Buf2), Buf2Len) - 2;
end;
{$ENDIF}

{ TSQLiteDatabase }

constructor TSQLiteDatabase.Create(const FileName: String);
var
  Msg: PAnsiChar;
  iResult: integer;
  s: String;
begin
  inherited Create;
  fParams := TList.Create;
  fDb := nil;
  {$IFDEF SQUNI}
  iResult := SQLite3_Open(PAnsiChar(UTF8String(FileName)), Fdb);
  {$ELSE}
  iResult := SQLite3_Open(PAnsiChar(AnsiToUtf8(FileName)), Fdb);
  {$ENDIF}
  if iResult <> SQLITE_OK then
  begin
    try
      if fDB <> nil then
        RaiseError(c_failopen, FileName)
      else
      begin
        Msg := sqlite3_errstr(iResult);
        s := c_nomessage;
        if Msg <> nil then
        begin
          //Msg is UTF8!
          {$IFDEF SQUNI}
          s := String(UTF8String(Msg));
          {$ELSE}
          s := Utf8ToAnsi(Msg);
          {$ENDIF}
        end;
        raise ESqliteException.CreateFmt(c_failopen + c_error, [SQLiteErrorType(iResult), iResult, s, FileName]);
      end;
    finally
      sqlite3_close(fDB); //harmless if fDB = nul
    end;
  end;
end;

function TSQLiteDatabase.DBBackupFile(const FileName: String): integer;
var
  rc: integer;
  sdb: TSQLiteDB;
begin
  sdb := nil;
  {$IFDEF SQUNI}
  rc := SQLite3_Open(PAnsiChar(UTF8String(FileName)), sdb);
  {$ELSE}
  rc := SQLite3_Open(PAnsiChar(AnsiToUtf8(FileName)), sdb);
  {$ENDIF}
  try
    if rc = SQLITE_OK then
      rc := DBBackupHandle(sdb);
  finally
    sqlite3_close(sdb);
  end;
  Result := rc;
end;

function TSQLiteDatabase.DBBackupHandle(DestinationDB: TSQliteDB): integer;
var
  backup: TSQLiteBackup;
  rc: integer;
begin
  FBackupStop := false;
  backup := sqlite3_backup_init(DestinationDB, 'main', self.fDB, 'main');
  if backup <> nil then
  begin
    repeat
      rc := sqlite3_backup_step(backup, 64);
      if (rc = SQLITE_OK) or (rc = SQLITE_BUSY) or (rc = SQLITE_LOCKED) then
        sleep(10);
      if FBackupStop then
        Break;
    until not((rc = SQLITE_OK) or (rc = SQLITE_BUSY) or (rc = SQLITE_LOCKED));
    sqlite3_backup_finish(backup);
  end;
  rc := sqlite3_errcode(DestinationDB);
  Result := rc;
end;

procedure TSQLiteDatabase.DbBackupStop;
begin
  FBackupStop := true;
end;

function TSQLiteDatabase.DBLoadFile(const FileName: String): integer;
var
  rc: integer;
  sdb: TSQLiteDB;
begin
  sdb := nil;
  {$IFDEF SQUNI}
  rc := SQLite3_Open(PAnsiChar(UTF8String(FileName)), sdb);
  {$ELSE}
  rc := SQLite3_Open(PAnsiChar(AnsiToUtf8(FileName)), sdb);
  {$ENDIF}
  try
    if rc = SQLITE_OK then
      rc := DBLoadHandle(sdb);
  finally
    sqlite3_close(sdb);
  end;
  Result := rc;
end;

function TSQLiteDatabase.DBLoadHandle(SourceDB: TSQliteDB): integer;
var
  backup: TSQLiteBackup;
  rc: integer;
begin
  backup := sqlite3_backup_init(self.fDB, 'main', SourceDB, 'main');
  if backup <> nil then
  begin
    sqlite3_backup_step(backup, -1);
    sqlite3_backup_finish(backup);
  end;
  rc := sqlite3_errcode(self.fDB);
  Result := rc;
end;

function TSQLiteDatabase.DBSaveFile(const FileName: String): integer;
var
  rc: integer;
  sdb: TSQLiteDB;
begin
  sdb := nil;
  {$IFDEF SQUNI}
  rc := SQLite3_Open(PAnsiChar(UTF8String(FileName)), sdb);
  {$ELSE}
  rc := SQLite3_Open(PAnsiChar(AnsiToUtf8(FileName)), sdb);
  {$ENDIF}
  try
    if rc = SQLITE_OK then
      rc := DBSaveHandle(sdb);
  finally
    sqlite3_close(sdb);
  end;
  Result := rc;
end;

function TSQLiteDatabase.DBSaveHandle(DestinationDB: TSQliteDB): integer;
var
  backup: TSQLiteBackup;
  rc: integer;
begin
  backup := sqlite3_backup_init(DestinationDB, 'main', self.fDB, 'main');
  if backup <> nil then
  begin
    sqlite3_backup_step(backup, -1);
    sqlite3_backup_finish(backup);
  end;
  rc := sqlite3_errcode(DestinationDB);
  Result := rc;
end;

destructor TSQLiteDatabase.Destroy;
var
  r: integer;
  i: integer;
const
  close_timeout = 5000;
  close_delay = 100;
begin
  if Assigned(fDB) then
  begin
    i := 0;
    repeat
      r := SQLite3_Close(fDB);
      if r <> SQLITE_OK then
      begin
        sleep(100);
        inc(i);
      end;
      if (i * close_delay) >= close_timeout then
        Break;
    until r = SQLITE_OK;
  end;
  ParamsClear;
  fParams.Free;
  inherited;
end;

function TSQLiteDatabase.LastInsertRowID: int64;
begin
  Result := Sqlite3_LastInsertRowID(self.fDB);
end;

function TSQLiteDatabase.LastChangedRows: int64;
begin
  Result := SQLite3_Changes(self.fDB);
end;

function TSQLiteDatabase.TotalChanges: int64;
begin
  Result := SQLite3_TotalChanges(self.fDB);
end;

procedure TSQLiteDatabase.DBRaiseError(DBHandle: TSQLiteDB; s, SQL: String);
var
  Msg: PAnsiChar;
  m: string;
  ret : integer;
begin
  ret := sqlite3_extended_errcode(DBHandle);
  if (ret = SQLITE_OK) or (ret = SQLITE_DONE) then
    exit;
  Msg := sqlite3_errmsg(DBHandle);
  m := c_nomessage;
  if Msg <> nil then
  begin
    //Msg is UTF8!
    {$IFDEF SQUNI}
    m := String(UTF8String(Msg));
    {$ELSE}
    m := Utf8ToAnsi(Msg);
    {$ENDIF}
  end;
  SQL := StringReplace(SQL, '%', '%%', [rfReplaceAll]);
  raise ESqliteException.CreateFmt(s + c_error, [SQLiteErrorType(ret), ret, m, SQL]);
end;

procedure TSQLiteDatabase.RaiseError(const s, SQL: String);
begin
  DBRaiseError(self.fDB, s, SQL);
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
  iStepResult: integer;
begin
  try
    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(UTF8String(SQL)), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError(c_errorsql, SQL);
    if (Stmt = nil) then
      RaiseError(c_errorprepare, SQL);
    DoQuery(SQL);
    SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    if not(iStepResult in [SQLITE_DONE, SQLITE_ROW]) then
      begin
      SQLite3_reset(stmt);
      RaiseError(c_errorexec, SQL);
      end;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

function TSQLiteDatabase.GetTable(const SQL: string; PrepareOnly: Boolean = false): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(Self, SQL, PrepareOnly);
end;

function TSQLiteDatabase.GetTableValue(const SQL: String): int64;
var
  Table: TSQLiteTable;
begin
  Result := 0;
  Table := self.GetTable(SQL);
  try
    if not Table.EOF then
      Result := Table.FieldAsInteger(0);
  finally
    Table.Free;
  end;
end;

function TSQLiteDatabase.GetTableString(const SQL: String): AnsiString;
var
  Table: TSQLiteTable;
begin
  Result := '';
  Table := self.GetTable(SQL);
  try
    if not Table.EOF then
      Result := Table.FieldAsString(0);
  finally
    Table.Free;
  end;
end;

procedure TSQLiteDatabase.GetTableStrings(const SQL: String;
  const Value: TStrings);
var
  Table: TSQLiteTable;
begin
  Value.Clear;
  Table := self.GetTable(SQL);
  try
    while not table.EOF do
    begin
      Value.Add(Table.FieldAsString(0));
      table.Next;
    end;
  finally
    Table.Free;
  end;
end;

{$IFDEF SQUNI}
procedure TSQLiteDatabase.GetTableStringsUnicode(const SQL: String;
  const Value: TStrings);
var
  Table: TSQLiteTable;
begin
  Value.Clear;
  Table := self.GetTable(SQL);
  try
    while not table.EOF do
    begin
      Value.Add(string(Table.FieldAsStringUnicode(0)));
      table.Next;
    end;
  finally
    Table.Free;
  end;
end;
{$ENDIF}

{$IFDEF SQUNI}
function TSQLiteDatabase.GetTableStringUnicode(const SQL: String): Utf8String;
var
  Table: TSQLiteTable;
begin
  Result := '';
  Table := self.GetTable(SQL);
  try
    if not Table.EOF then
      Result := Table.FieldAsStringUnicode(0);
  finally
    Table.Free;
  end;
end;
{$ENDIF}

procedure TSQLiteDatabase.Start(const name:String; const param: String = '');
var
  s: String;
begin
  if name = '' then
  begin
    s := 'BEGIN';
    if param <> '' then
      s := s + ' ' + param;
    self.ExecSQL(s);
  end
  else
    self.ExecSQL('SAVEPOINT ' + name);
end;

procedure TSQLiteDatabase.Commit(const name:String);
begin
  if name = '' then
    self.ExecSQL('COMMIT')
  else
    self.ExecSQL('RELEASE ' + name);
end;

procedure TSQLiteDatabase.Rollback(const name:String);
begin
  if name = '' then
    self.ExecSQL('ROLLBACK')
  else
    self.ExecSQL('ROLLBACK TO ' + name);
end;

procedure TSQLiteDatabase.SetTimeout(Value: integer);
begin
  SQLite3_BusyTimeout(self.fDB, Value);
end;

function TSQLiteDatabase.Version: String;
begin
  Result := String(UTF8String(SQLite3_Version));
end;

procedure TSQLiteDatabase.AddCustomCollate(name: String;
  xCompare: TCollateXCompare);
begin
  sqlite3_create_collation(fdb, PAnsiChar(UTF8String(name)), SQLITE_UTF8, nil, xCompare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
  {$IFDEF MSWINDOWS}
  sqlite3_create_collation(fdb, 'SYSTEM', SQLITE_UTF16LE, nil, @SystemCollate);
  {$ENDIF}
end;

procedure TSQLiteDatabase.ParamsClear;
var
  n: integer;
begin
  for n := fParams.Count - 1 downto 0 do
    TSQliteParam(fparams[n]).free;
  fParams.Clear;
end;

procedure TSQLiteDatabase.AddParamInt(const name: String; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String( name );
  par.valuetype := SQLITE_INTEGER;
  par.valueinteger := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamFloat(const name: String; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String( name );
  par.valuetype := SQLITE_FLOAT;
  par.valuefloat := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamText(const name: String; const value: AnsiString);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String( name );
  par.valuetype := SQLITE_TEXT;
  setlength(par.valuedata, length(value));
  if length(value) > 0 then
    move(PAnsiChar(Value)^, pointer(par.valuedata)^, length(Value));
  fParams.Add(par);
end;

{$IFDEF SQUNI}
procedure TSQLiteDatabase.AddParamTextUnicode(const name: String; const Value: Utf8String);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String(name);
  par.valuetype := SQLITE_TEXT;
  par.valuedata := value;
  fParams.Add(par);
end;
{$ENDIF}

procedure TSQLiteDatabase.AddParamNull(const name: String);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String( name );
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamBlobPtr(const name: String; buffer: pointer;
  len: integer);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String( name );
  par.valuetype := SQLITE_BLOB;
  setlength(par.valuedata, len);
  if len > 0 then
    move(buffer^, pointer(par.valuedata)^, len);
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamBlobText(const name: String; const value: AnsiString);
begin
  AddParamBlobPtr(name, PAnsiChar(value), length(value));
end;

procedure TSQLiteDatabase.AddParamBlob(const name: String;
  const value: TStream; len: integer);
var
  buffer: AnsiString;
  x: integer;
begin
  setlength(buffer, len);
  x := value.Read(pointer(buffer)^, len);
  setlength(buffer, x);
  AddParamBlobText(name, buffer);
end;

procedure TSQLiteDatabase.SetParams(const Stmt: TSQLiteStmt);
var
  n: integer;
  i: integer;
  par: TSQliteParam;
  s, t: string;
begin
  try
    for n := 0 to fParams.Count - 1 do
    begin
      par := TSQliteParam(fParams[n]);
      if par.name = '' then
        continue;
      i := sqlite3_bind_parameter_index(Stmt, @par.name[1]);
      if i > 0 then
      begin
        if assigned(OnQuery) then
        begin
          t := 'NULL';
          s := 'NULL';
          case par.valuetype of
            SQLITE_INTEGER:
              begin
                t := 'INTEGER';
                s := inttostr(par.valueinteger);
              end;
            SQLITE_FLOAT:
              begin
                t := 'FLOAT';
                s := floattostr(par.valuefloat);
              end;
            SQLITE_TEXT:
              begin
                t := 'TEXT';
                s := string(par.valuedata);
              end;
            SQLITE_BLOB:
              begin
                t := 'BLOB';
                s := 'BLOB';
              end;
          end;
          OnQuery(Self, format('Parameter: %s (%s) = %s',[par.name, t, s]));
        end;
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(Stmt, i, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(Stmt, i, par.valuefloat);
          SQLITE_TEXT:
            if par.valuedata = '' then
              sqlite3_bind_text(Stmt, i, PAnsiChar(par.valuedata),
                0, SQLITE_TRANSIENT)
            else
              sqlite3_bind_text(Stmt, i, @par.valuedata[1],
                length(par.valuedata), SQLITE_TRANSIENT);
          SQLITE_BLOB:
            if par.valuedata = '' then
              sqlite3_bind_blob(Stmt, i, PAnsiChar(par.valuedata),
                0, SQLITE_TRANSIENT)
            else
              sqlite3_bind_blob(Stmt, i, @par.valuedata[1],
                length(par.valuedata), SQLITE_TRANSIENT);
          SQLITE_NULL:
            sqlite3_bind_null(Stmt, i);
        end;
      end;
    end;
  finally
    ParamsClear;
  end;
end;

procedure TSQLiteDatabase.DoQuery(const value: String);
begin
  if assigned(OnQuery) then
    OnQuery(Self, Value);
end;

function TSQLiteDatabase.InTransaction: Boolean;
begin
  Result := SQLite3_Get_Autocommit(FDB) = 0;
end;

procedure TSQLiteDatabase.EnableLoadExtension(value: boolean);
var
  v: integer;
begin
  if value then
    v := 1
  else
    v := 0;
  SQLite3_enable_load_extension(FDB, v);
end;

procedure TSQLiteDatabase.SetChunkSize(DatabaseName: ansistring; value: integer);
begin
  SQLite3_file_control(FDB, PAnsiChar(DatabaseName), SQLITE_FCNTL_CHUNK_SIZE, @value);
end;

{ TSQLiteTable }

procedure TSQLiteTable.AddParamFloat(const name: String; value: double);
var
  i: integer;
begin
  if name = '' then
    exit;
  i := sqlite3_bind_parameter_index(FStmt, @UTF8String(name)[1]);
  if i > 0 then
    sqlite3_bind_double(FStmt, i, value);
end;

procedure TSQLiteTable.AddParamInt(const name: String; value: int64);
var
  i: integer;
begin
  if name = '' then
    exit;
  i := sqlite3_bind_parameter_index(FStmt, @UTF8String(name)[1]);
  if i > 0 then
    sqlite3_bind_int64(FStmt, i, value);
end;

procedure TSQLiteTable.AddParamNull(const name: String);
var
  i: integer;
begin
  if name = '' then
    exit;
  i := sqlite3_bind_parameter_index(FStmt, @UTF8String(name)[1]);
  if i > 0 then
    sqlite3_bind_null(FStmt, i);
end;

procedure TSQLiteTable.AddParamText(const name : string; const value: AnsiString);
var
  i: integer;
begin
  if name = '' then
    exit;
  i := sqlite3_bind_parameter_index(FStmt, @UTF8String(name)[1] );
  if i > 0 then
    sqlite3_bind_text(FStmt, i, pansichar(value), length(value), SQLITE_TRANSIENT);
end;

{$IFDEF SQUNI}
procedure TSQLiteTable.AddParamTextUnicode(const name: String;
  const Value: Utf8String);
var
  i: integer;
begin
  if name = '' then
    exit;
  i := sqlite3_bind_parameter_index(FStmt, @UTF8String(name)[1] );
  if i > 0 then
  begin
    if value = '' then
      sqlite3_bind_text(FStmt, i, pansichar(value), 0, SQLITE_TRANSIENT)
    else
      sqlite3_bind_text(FStmt, i, @value[1], length(value), SQLITE_TRANSIENT);
  end;
end;
{$ENDIF}

procedure TSQLiteTable.AddParamBlobPtr(const name: String; buffer: pointer; len: integer);
var
  i: integer;
begin
  if name = '' then
    exit;
  i := sqlite3_bind_parameter_index(FStmt, @UTF8String(name)[1]);
  if i > 0 then
    sqlite3_bind_blob(FStmt, i, buffer, len, SQLITE_TRANSIENT);
end;

procedure TSQLiteTable.AddParamBlobText(const name: String; const value: AnsiString);
begin
  if name = '' then
    exit;
  AddParamBlobPtr(name, PAnsiChar(value), length(value));
end;

procedure TSQLiteTable.AddParamBlob(const name: String;
  const value: TStream; len: integer);
var
  buffer: AnsiString;
  x: integer;
begin
  if name = '' then
    exit;
  setlength(buffer, len);
  x := value.Read(pointer(buffer)^, len);
  setlength(buffer, x);
  AddParamBlobText(name, buffer);
end;

constructor TSQLiteTable.Create(const DB: TSQLiteDatabase; const SQL: String; PrepareOnly: Boolean = false);
var
  NextSQLStatement: PAnsiChar;
  i: integer;
begin
  inherited create;
  self.fDB := db;
  self.fEOF := false;
  self.fRow := 0;
  self.fColCount := 0;
  self.fSQL := SQL;
  if Sqlite3_Prepare_v2(DB.fDB, PAnsiChar(UTF8String(SQL)), -1, fStmt, NextSQLStatement) <> SQLITE_OK then
    DB.RaiseError(c_errorsql, SQL);
  if (fStmt = nil) then
    DB.RaiseError(c_errorprepare, SQL);
  DB.DoQuery(SQL);
  //get data types
  fCols := TStringList.Create;
  fCols.Capacity := 32;
  fColCount := SQLite3_ColumnCount(fstmt);
  for i := 0 to Pred(fColCount) do
    // AnsiUpperCase operates on UNICODE strings but according to Ansi Collation Rules!
    fCols.Add( AnsiUpperCase(String(UTF8String(Sqlite3_ColumnName(fstmt, i)))));
  if not PrepareOnly then
  begin
    DB.SetParams(fStmt);
    Next;
  end;
end;

destructor TSQLiteTable.Destroy;
begin
  if Assigned(fStmt) then
    Sqlite3_Finalize(fstmt);
  if Assigned(fCols) then
    fCols.Free;
  inherited;
end;

function TSQLiteTable.FieldAsBlob(I: cardinal): TMemoryStream;
var
  iNumBytes: integer;
  ptr: pointer;
begin
  Result := TMemoryStream.Create;
  iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
  if iNumBytes > 0 then
  begin
    ptr := Sqlite3_ColumnBlob(fstmt, i);
    Result.writebuffer(ptr^, iNumBytes);
    Result.Position := 0;
  end;
end;

function TSQLiteTable.FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): Pointer;
begin
  iNumBytes := Sqlite3_ColumnBytes(fstmt, i);
  Result := Sqlite3_ColumnBlob(fstmt, i);
end;

function TSQLiteTable.FieldAsBlobText(I: cardinal): AnsiString;
var
  MemStream: TMemoryStream;
  Buffer: PAnsiChar;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);
  if MemStream <> nil then
     try
      if MemStream.Size > 0 then
      begin
        MemStream.position := 0;
        Buffer := MemStream.Memory;
        SetString(Result, Buffer, MemStream.size);
      end;
     finally
     MemStream.Free;
     end
end;

function TSQLiteTable.FieldAsDouble(I: cardinal): double;
begin
  Result := Sqlite3_ColumnDouble(fstmt, i);
end;

function TSQLiteTable.FieldAsInteger(I: cardinal): int64;
begin
  Result := Sqlite3_ColumnInt64(fstmt, i);
end;

function TSQLiteTable.FieldAsString(I: cardinal): AnsiString;
begin
  Result := self.GetFields(I);
end;

{$IFDEF SQUNI}
function TSQLiteTable.FieldAsStringUnicode(I: cardinal): Utf8String;
begin
  Result := self.GetFieldsUnicode(I);
end;
{$ENDIF}

function TSQLiteTable.FieldIsNull(I: cardinal): boolean;
begin
  Result := Sqlite3_ColumnText(fstmt, i) = nil;
end;

function TSQLiteTable.GetColumns(I: integer): String;
begin
  Result := fCols[I];
end;

function TSQLiteTable.GetFieldByName(FieldName: String): String;
begin
  Result := GetFields(self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(FieldName: String): integer;
begin
  if (fCols = nil) then
  begin
    raise ESqliteException.Create(format(c_errorempty, [fieldname]));
    exit;
  end;
  if (fCols.count = 0) then
  begin
    raise ESqliteException.Create(format(c_errorempty, [fieldname]));
    exit;
  end;
  Result := fCols.IndexOf(AnsiUpperCase(FieldName));
  if (result < 0) then
  begin
    raise ESqliteException.Create(format(c_errorfield, [fieldname]))
  end;
end;

function TSQLiteTable.GetFields(I: cardinal): AnsiString;
begin
  Result := Sqlite3_ColumnText(fstmt, i);
end;

{$IFDEF SQUNI}
function TSQLiteTable.GetFieldsUnicode(I: cardinal): RawByteString;
begin
  Result := Sqlite3_ColumnText(fstmt, i);
  SetCodePage(Result, CP_UTF8, false);
end;
{$ENDIF}

function TSQLiteTable.Next: boolean;
var
  iStepResult: integer;
begin
  fEOF := true;
  iStepResult := Sqlite3_step(fStmt);
  case iStepResult of
    SQLITE_ROW:
      begin
        fEOF := false;
        inc(fRow);
      end;
    SQLITE_DONE:
      // we are on the end of dataset
      // return EOF=true only
      ;
  else
    begin
    SQLite3_reset(fStmt);
    fDB.RaiseError(c_errordata, fSQL);
    end;
  end;
  Result := not fEOF;
end;

procedure TSQLiteTable.ParamsClear;
begin
  sqlite3_clear_bindings(FStmt);
end;

procedure TSQLiteTable.Reset;
begin
  SQLite3_Reset(fstmt);
end;

initialization
  SQLite3_Initialize;

finalization
  SQLite3_Shutdown;
end.

