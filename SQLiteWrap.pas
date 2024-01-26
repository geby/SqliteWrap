{: @abstract(SQLite 3.x object based wrapper)

Can be used for object based access to SQLite3 databases.

Note: Requires Sqlite 3.6.8 and higher!

Sample usage:
@longcode(#
procedure sample;
var
  database: TSqliteDatabase;
  tab: TSqliteTable;
  s: string;
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

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SQLite3, Classes, SysUtils;

type
  {: @abstract(Exception Class for SQLite based errors)}
  ESQLiteException = class(Exception)
  end;

  {: @abstract(Class for storing deffered parameters)
  Do not use it, it is used internally only!
  }
  TSQliteParam = class
  public
    name: AnsiString;
    valuetype: integer;
    valueinteger: int64;
    valuefloat: double;
    valuedata: AnsiString;
  end;

  {: @abstract(procedural prototype for @link(OnQuery) hook.)}
  THookQuery = procedure(Sender: TObject; const SQL: AnsiString) of object;

  TSQLiteTable = class;

  {: @abstract(Class for handling SQLite database)}
  TSQLiteDatabase = class(TObject)
  private
    fDB: TSQLiteDB;
    fParams: TList;
    FOnQuery: THookQuery;
    procedure RaiseError(const s, SQL: AnsiString);
    procedure SetParams(const Stmt: TSQLiteStmt);
  protected
    procedure DoQuery(const value: AnsiString);
  public
    {: Class constructor. Pass filename of database. If databas not exists, then new one is created.
    If you pass empty string as filename, then temporary database is created.
    If you pass ':memory:' as filename, then temporary database in memory is created.
    }
    constructor Create(const FileName: AnsiString);
    {: Class descructor. Call Free instead.}
    destructor Destroy; override;
    {: Run SQL command without result.
       You can call before functions AddParam* for set query parameters.}
    procedure ExecSQL(const SQL: Ansistring);
    {: Run SQL command with result.
       You can call before functions AddParam* for set query parameters.
       If you set PerpareOnly, then query is just prepared but first row is not fetched!}
    function GetTable(const SQL: Ansistring; PrepareOnly: Boolean = false): TSQLiteTable;
    {: Run SQL command and number from first field in first row is returned.
       You can call before functions AddParam* for set query parameters.}
    function GetTableValue(const SQL: Ansistring): int64;
    {: Run SQL command and value from first field in first row is returned.
       You can call before functions AddParam* for set query parameters.}
    function GetTableString(const SQL: Ansistring): AnsiString;
    {: Run SQL command and values from first field in each row is filled to stringlist.
       You can call before functions AddParam* for set query parameters.}
    procedure GetTableStrings(const SQL: Ansistring; const Value: TStrings);
    {: Return @True if database is in transaction state.}
    function InTransaction: Boolean;
    {: Start transaction. You can modify transaction type by Param parameter.
       If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested.}
    procedure Start(const name:AnsiString; const param: AnsiString = '');
    {: Commit transaction.
       If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested.}
    procedure Commit(const name:AnsiString);
    {: Rollback transaction.
       If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested.}
    procedure Rollback(const name:AnsiString);
    {: Get ROWID of last inserted row.}
    function LastInsertRowID: int64;
    {: Return number of modified rows by last query.}
    function LastChangedRows: int64;
    {: Set wait timeout. if database is locked, then it wait this timeout.
       If database is not released within this timeout, then error is returned.}
    procedure SetTimeout(Value: integer);
    {: Return SQLite engine version.}
    function Version: AnsiString;
    {: Add custom sorting procedure as new Collate.}
    procedure AddCustomCollate(name: AnsiString; xCompare: TCollateXCompare);
    {: Add collate named SYSTEM for correct data sorting by user's locale}
    Procedure AddSystemCollate;
    {: Clear all query parameters.}
    procedure ParamsClear;
    {: Add named query parameter of integer type.}
    procedure AddParamInt(const name: AnsiString; value: int64);
    {: Add named query parameter of floating-point type.}
    procedure AddParamFloat(const name: AnsiString; value: double);
    {: Add named query parameter of string or binary type.}
    procedure AddParamText(const name: AnsiString; const value: AnsiString);
    {: Add named query parameter with null value.}
    procedure AddParamNull(const name: AnsiString);
    {: SQLite database handler.}
    property DB: TSQLiteDB read fDB;
  published
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
    fSQL: AnsiString;
    function GetFields(I: cardinal): AnsiString;
    function GetColumns(I: integer): AnsiString;
    function GetFieldByName(FieldName: AnsiString): AnsiString;
    function GetFieldIndex(FieldName: AnsiString): integer;
  public
    {: Class constructor. Called internally by @link(TSqliteDatabase)}
    constructor Create(const DB: TSQLiteDatabase; const SQL: Ansistring; PrepareOnly: Boolean = false);
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
    procedure AddParamInt(const name: AnsiString; value: int64);
    {: Add named query parameter of floating-point type.}
    procedure AddParamFloat(const name: AnsiString; value: double);
    {: Add named query parameter of string or binary type.}
    procedure AddParamText(const name: AnsiString; const value: AnsiString);
    {: Add named query parameter with null value.}
    procedure AddParamNull(const name: AnsiString);
    {: Return value of some field in current row.}
    property Fields[I: cardinal]: AnsiString read GetFields;
    {: Return value of named field in current row.}
    property FieldByName[FieldName: AnsiString]: AnsiString read GetFieldByName;
    {: Return index of some named field.}
    property FieldIndex[FieldName: AnsiString]: integer read GetFieldIndex;
    {: Return field type of some field.}
    property Columns[I: integer]: AnsiString read GetColumns;
  published
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
  c_failopen = 'Failed to open database "%s" : %s';
  c_error = '.' + slinebreak + 'Error [%d]: %s.'+slinebreak+'"%s": %s';
  c_nomessage = 'No message';
  c_errorsql = 'Error executing SQL';
  c_errorprepare = 'Could not prepare SQL statement';
  c_errorexec = 'Error executing SQL statement';
  c_errorempty = 'Field %s Not found. Empty dataset';
  c_errorfield = 'Field not found in dataset: %s';
  c_errordata = 'Could not retrieve data';

{$IFDEF WIN32}
function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
    Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len,
    PWideChar(Buf2), Buf2Len) - 2;
end;
{$ENDIF}

{ TSQLiteDatabase }

constructor TSQLiteDatabase.Create(const FileName: AnsiString);
var
  Msg: PAnsiChar;
  iResult: integer;
  s: AnsiString;
begin
  inherited Create;
  fParams := TList.Create;
  Msg := nil;
  fDb := nil;
  try
    iResult := SQLite3_Open(PAnsiChar(AnsiToUtf8(FileName)), Fdb);
    if iResult <> SQLITE_OK then
    begin
      s := c_unknown;
      if Assigned(Fdb) then
      begin
        Msg := Sqlite3_ErrMsg(Fdb);
        s := Msg;
      end;
      raise ESqliteException.CreateFmt(c_failopen, [FileName, s]);
    end;
  finally
    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;
end;

destructor TSQLiteDatabase.Destroy;
begin
  if Assigned(fDB) then
    SQLite3_Close(fDB);
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

procedure TSQLiteDatabase.RaiseError(const s, SQL: AnsiString);
var
  Msg: PAnsiChar;
  ret : integer;
begin
  Msg := nil;
  ret := sqlite3_errcode(self.fDB);
  if ret <> SQLITE_OK then
    Msg := sqlite3_errmsg(self.fDB);
  if Msg <> nil then
    raise ESqliteException.CreateFmt(s + c_error, [ret, SQLiteErrorStr(ret),SQL, Msg])
  else
    raise ESqliteException.CreateFmt(s, [SQL, c_nomessage]);
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: Ansistring);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
  iStepResult: integer;
begin
  try
    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(SQL), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError(c_errorsql, SQL);
    if (Stmt = nil) then
      RaiseError(c_errorprepare, SQL);
    DoQuery(SQL);
    SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    if (iStepResult <> SQLITE_DONE) then
      begin
      SQLite3_reset(stmt);
      RaiseError(c_errorexec, SQL);
      end;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(stmt);
  end;
end;

function TSQLiteDatabase.GetTable(const SQL: Ansistring; PrepareOnly: Boolean = false): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(Self, SQL, PrepareOnly);
end;

function TSQLiteDatabase.GetTableValue(const SQL: Ansistring): int64;
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

function TSQLiteDatabase.GetTableString(const SQL: Ansistring): AnsiString;
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

procedure TSQLiteDatabase.GetTableStrings(const SQL: Ansistring;
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

procedure TSQLiteDatabase.Start(const name:AnsiString; const param: AnsiString = '');
var
  s: AnsiString;
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

procedure TSQLiteDatabase.Commit(const name:AnsiString);
begin
  if name = '' then
    self.ExecSQL('COMMIT')
  else
    self.ExecSQL('RELEASE ' + name);
end;

procedure TSQLiteDatabase.Rollback(const name:AnsiString);
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

function TSQLiteDatabase.Version: AnsiString;
begin
  Result := SQLite3_Version;
end;

procedure TSQLiteDatabase.AddCustomCollate(name: AnsiString;
  xCompare: TCollateXCompare);
begin
  sqlite3_create_collation(fdb, PAnsiChar(name), SQLITE_UTF8, nil, xCompare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
  {$IFDEF WIN32}
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

procedure TSQLiteDatabase.AddParamInt(const name: AnsiString; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_INTEGER;
  par.valueinteger := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamFloat(const name: AnsiString; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_FLOAT;
  par.valuefloat := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamText(const name: AnsiString; const value: AnsiString);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_TEXT;
  par.valuedata := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamNull(const name: AnsiString);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := name;
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.SetParams(const Stmt: TSQLiteStmt);
var
  n: integer;
  i: integer;
  par: TSQliteParam;
begin
  try
    for n := 0 to fParams.Count - 1 do
    begin
      par := TSQliteParam(fParams[n]);
      i := sqlite3_bind_parameter_index(Stmt, PAnsiChar(par.name));
      if i > 0 then
      begin
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(Stmt, i, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(Stmt, i, par.valuefloat);
          SQLITE_TEXT:
            sqlite3_bind_text(Stmt, i, PAnsiChar(par.valuedata),
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

procedure TSQLiteDatabase.DoQuery(const value: AnsiString);
begin
  if assigned(OnQuery) then
    OnQuery(Self, Value);
end;

function TSQLiteDatabase.InTransaction: Boolean;
begin
  Result := SQLite3_Get_Autocommit(FDB) = 0; 
end;

{ TSQLiteTable }

procedure TSQLiteTable.AddParamFloat(const name: AnsiString;
  value: double);
var
  i: integer;
begin
  i := sqlite3_bind_parameter_index(FStmt, PAnsiChar(name));
  if i > 0 then
    sqlite3_bind_double(FStmt, i, value);
end;

procedure TSQLiteTable.AddParamInt(const name: AnsiString; value: int64);
var
  i: integer;
begin
  i := sqlite3_bind_parameter_index(FStmt, PAnsiChar(name));
  if i > 0 then
    sqlite3_bind_int64(FStmt, i, value);
end;

procedure TSQLiteTable.AddParamNull(const name: AnsiString);
var
  i: integer;
begin
  i := sqlite3_bind_parameter_index(FStmt, PAnsiChar(name));
  if i > 0 then
    sqlite3_bind_null(FStmt, i);
end;

procedure TSQLiteTable.AddParamText(const name, value: AnsiString);
var
  i: integer;
begin
  i := sqlite3_bind_parameter_index(FStmt, PAnsiChar(name));
  if i > 0 then
    sqlite3_bind_text(FStmt, i, PAnsiChar(value), length(value), SQLITE_TRANSIENT);
end;

constructor TSQLiteTable.Create(const DB: TSQLiteDatabase; const SQL: Ansistring; PrepareOnly: Boolean = false);
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
  if Sqlite3_Prepare_v2(DB.fDB, PAnsiChar(SQL), -1, fStmt, NextSQLStatement) <> SQLITE_OK then
    DB.RaiseError(c_errorsql, SQL);
  if (fStmt = nil) then
    DB.RaiseError(c_errorprepare, SQL);
  DB.DoQuery(SQL);
  //get data types
  fCols := TStringList.Create;
  fColCount := SQLite3_ColumnCount(fstmt);
  for i := 0 to Pred(fColCount) do
    fCols.Add(AnsiUpperCase(Sqlite3_ColumnName(fstmt, i)));
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
        {$IFDEF UNICODE}
        Buffer := AnsiStralloc(MemStream.Size + 1);
        {$ELSE}
        Buffer := Stralloc(MemStream.Size + 1);
        {$ENDIF}
        MemStream.readbuffer(Buffer[0], MemStream.Size);
        (Buffer + MemStream.Size)^ := chr(0);
        SetString(Result, Buffer, MemStream.size);
        strdispose(Buffer);
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

function TSQLiteTable.FieldIsNull(I: cardinal): boolean;
begin
  Result := Sqlite3_ColumnText(fstmt, i) = nil;
end;

function TSQLiteTable.GetColumns(I: integer): AnsiString;
begin
  Result := fCols[I];
end;

function TSQLiteTable.GetFieldByName(FieldName: AnsiString): AnsiString;
begin
  Result := GetFields(self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(FieldName: AnsiString): integer;
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

