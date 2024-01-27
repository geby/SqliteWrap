# SqliteWrap
This is very simple [Sqlite 3](https://www.sqlite.org) wrapper for Delphi and FreePascal. It is based on [Tim Anderson's Simplewrapper](http://www.itwriting.com/blog/a-simple-delphi-wrapper-for-sqlite-3).
I was using his wrapper for long time, include my contributions. However it lost simplicity later and contains lot of duplicated features.

I start with code cleaning and create my own wrapper code as separate project. It have similar interface is Tim's wrapper, but is is very close to Sqlite API.

[![Paypal donate](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=8U9Z2YP9J4DEA) 

# Features
* Require SQLite3 at least version 3.7.1.
* It is not component, just units. Include this wraper to your project uses and create classes for handling of database.
* It is not integrated into Delphi database model.
* Very lightweight code.
* You can use any SQL command and walk through result set.
* You can use prepared queries.
* You can use parametrized queries.
* You can use transactions, include savepoints!
* You can define your own UDF (User defined SQL functions).
* You can define your own collates.
* Sqlite Backup API supported.

# Sample
```
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
        //...then go to next row.
        tab.next;
      end;
    finally
      tab.free;
    end;
  finally
    database.free;
  end;
end;
```
