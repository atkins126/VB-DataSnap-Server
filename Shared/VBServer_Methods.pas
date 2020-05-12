unit VBServer_Methods;

interface

uses
  System.SysUtils, System.Classes, System.Json, System.StrUtils, System.Types,
  DataSnap.DSServer, System.Win.Registry, Winapi.Windows, System.IOUtils,
  System.Variants, System.IniFiles,

  DataSnap.DSAuth, Data.FireDACJSONReflect,

  MyClasses,

  FireDAC.Stan.StorageJSON, DataSnap.DSProviderDataModuleAdapter,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.StorageBin,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.UI, {FireDAC.Phys.IBDef,}FireDAC.Phys.IBBase,
  FireDAC.Phys.IB, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.Phys.IBWrapper;

type
  {$METHODINFO ON}
  TVBServerMethods = class(TDSServerModule)
    conFB: TFDConnection;
    FDGUIxWaitCursor: TFDGUIxWaitCursor;
    FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink;
    FDStanStorageJSONLink: TFDStanStorageJSONLink;
    FDStanStorageBinLink: TFDStanStorageBinLink;
    qrySQL: TFDQuery;
    cmdGeneric: TFDCommand;
    sprGeneric: TFDStoredProc;
    trnFB: TFDTransaction;
    qryInsert: TFDQuery;
    procedure DSServerModuleCreate(Sender: TObject);
    procedure DSServerModuleDestroy(Sender: TObject);
    procedure conFBBeforeConnect(Sender: TObject);
    procedure conFBError(ASender, AInitiator: TObject; var AException: Exception);
  private
    { Private declarations }
    FErrorMsg: string;
    FObjectList: TStringList;

    function ParseServerErrorMsg(ServerErrorMsg: string): string;
  public
    { Public declarations }
    function GetData(Request, ParameterList, Generatorname, Tablename, DataSetName: string; var Response: string): TFDJSONDataSets;
    function ExecuteSQLCommand(Request: string): string;
    function ExecuteStoredProcedure(ProcedureName, ParameterList: string): string;
    function GetFileVersion(Request: string; var Response: string): string;
    function DownloadFile(Request: string; var Response: string; var Size: Int64): TStream;
    function InsertRecord(Request: string; var Response: string): string;
    function EchoString(Request: string; var Response: string): string;
    function GetNextID(GeneratorName: string): string;
    function GetUseCount(Request: string): string;
    function GetFieldValue(Request: string; FieldType: TFieldType): string;

    function ApplyDataUpdates(const DeltaList: TFDJSONDeltas; var ReplyMessage: string;
      GeneratorName, TableName: string; ScriptID: Integer): string;
  end;
  {$METHODINFO Off}

  Query = class(TFDQuery);

implementation

{$R *.dfm}

uses
  RUtils,
  CommonServiceValues;

{ TVBServerMethods }

procedure TVBServerMethods.DSServerModuleCreate(Sender: TObject);
begin
  FObjectList := TStringList.Create;
end;

procedure TVBServerMethods.DSServerModuleDestroy(Sender: TObject);
begin
  FObjectList.Free;
end;

function TVBServerMethods.InsertRecord(Request: string; var Response: string): string;
var
  InputList: TStringList;
begin
  try
    qrySQL.Open(Request);
    Response := 'RESPONSE=SUCCESS';
    Result := IntToStr(qrySQL.FieldByName('ID').AsInteger);
  except on E: Exception do
    begin
      Result := 'RESPONSE=ERROR' + PIPE + 'ERROR_MESSAGE=' + E.Message;
      conFB.Rollback;
    end;
  end;
end;

function TVBServerMethods.GetData(Request, ParameterList, Generatorname, Tablename, DataSetName: string; var Response: string): TFDJSONDataSets;
var
  InputList, ScriptIDList, ParameterListing, SingleParameter, DataSetNameList: TStringList;
  I, X: Integer;
  Query: TFDQuery;
  SQL, TheDataSetName: string;
begin
  { Notes to developer:

    2. The request for data comes from the client in the form of delimited string
       values.

    3. InputList contains the full un-parsed request string.

    4. ScriptIDList contains the list of ID's used to retrieve the SQL statement
       needed to fetch data from the server.
  }

  Response := '';
  InputList := RUtils.CreateStringList(PIPE, DOUBLE_QUOTE);
  ScriptIDList := RUtils.CreateStringList(COMMA, DOUBLE_QUOTE);
  ParameterListing := RUtils.CreateStringList(PIPE, DOUBLE_QUOTE);
  SingleParameter := RUtils.CreateStringList(SEMI_COLON, DOUBLE_QUOTE);
  DataSetNameList := RUtils.CreateStringList(SEMI_COLON, DOUBLE_QUOTE);

  try
    InputList.DelimitedText := Request;
    ParameterListing.DelimitedText := ParameterList;
    SingleParameter.DelimitedText := ParameterListing.Values['PARAMETER_LIST'];

    for I := SingleParameter.Count - 1 downto 0 do
      if Length(SingleParameter[I]) = 0 then
        SingleParameter.Delete(I);

    // Create the Dataset list that will contain the data to be passed back to
    // the client.
    Result := TFDJSONDataSets.Create;
    FObjectList.Clear;

    ScriptIDList.DelimitedText := InputList.Values['SQL_STATEMENT_ID'];
    // Iterate the list of ID's to retriev the SQL Statements.
    for I := 0 to ScriptIDList.Count - 1 do
    begin
      qrySQL.Open(Format(SQL_STATEMENT, [ScriptIDList[I]]));
      // Code added by CVG on 26/06/2019. This is to accommodate datasets
      // with different names but getting their data from the same DB table
      // on the client side.
      DataSetNameList.DelimitedText := qrySQL.FieldByName('DATASET_NAME').AsString;
      for X := 0 to DataSetNameList.Count - 1 do
        if SameText(DataSetName, DataSetNameList[X]) then
        begin
          TheDataSetName := DataSetname; // qrySQL.FieldByName('DATASET_NAME').AsString;
          Break;
        end;

      Query := TFDQuery.Create(nil);
      Query.Name := TheDataSetName;
      // End of code added by CVG on 26/06/2019

      // Code deprecated by CVG on 26/06/2019
//      Query := TFDQuery.Create(nil);
//      Query.Name := qrySQL.FieldByName('DATASET_NAME').AsString;

      // Set generator name so that we can send the newly generted GEN_ID
      // back to the client.
      if Length(Trim(Generatorname)) > 0 then
        Query.UpdateOptions.Generatorname := Generatorname;

      // NB!! Must set the table/view name here.
      if Length(Trim(Tablename)) > 0 then
        Query.UpdateOptions.UpdateTableName := Tablename;

      Query.Connection := conFB;
      Query.FilterOptions := [foCaseInsensitive];
      Query.FormatOptions.DataSnapCompatibility := True;

      FObjectList.Add(ComponentToString(Query));
      Query := (StringToComponent(FObjectList[I]) as TFDQuery);
      SQL := qrySQL.FieldByName('SQL_STATEMENT').AsString;

      if SingleParameter.Count > 0 then
        SQL := Format(SQL, BuildFormatArray(SingleParameter.Count, SingleParameter));

      Query.Open(SQL);
      Response := 'DATA_FOUND';

      if Query.IsEmpty then
        Response := 'NO_DATA';
      // Add the data to the dataset list to be returned to the client.
      TFDJSONDataSetsWriter.ListAdd(Result, Query.Name, Query);
    end;
  finally
    InputList.Free;
    ScriptIDList.Free;
    ParameterListing.Free;
    SingleParameter.Free;
  end;
end;

function TVBServerMethods.ApplyDataUpdates(const DeltaList: TFDJSONDeltas; var ReplyMessage: string;
  GeneratorName, TableName: string; ScriptID: Integer): string;
var
  LApply: IFDJSONDeltasApplyUpdates;
  ListCount, I, ErrorCount: Integer;
  S, SQL {, FErrorMsg}: string;
  Query: TFDQuery;
  ApplyErrors: TFDJSONErrors;
  ErrorList: TStringList;
begin
  Result := '';
  FErrorMsg := '';
  ErrorCount := 0;
  ErrorList := RUtils.CreateStringList(COMMA, DOUBLE_QUOTE);

  // Create the apply object and populate it with the delta from the
  // FDMemTable(s) generated by the client.
  LApply := TFDJSONDeltasApplyUpdates.Create(DeltaList);
  // Get a count of the number of datasets to be updated.
  ListCount := LApply.Count;

//  conFB.StartTransaction;
  FObjectList.Clear;

  try
    for I := 0 to ListCount - 1 do
    begin
      // Get the Query name for the dataset that is used to transact.
      S := LApply.Items[I].Key;

//      SQL := Format(SQL_DATASET_NAME, [AnsiQuotedStr(AnsiUpperCase(S), '''')]);
      SQL := Format(SQL_STATEMENT, [ScriptID.ToString]);

      // Get the SQL statement used to generate field data.
//      qrySQL.Open(Format(SQL_DATASET_NAME, [AnsiQuotedStr(AnsiUpperCase(S), '''')]));
      qrySQL.Open(Format(SQL_STATEMENT, [ScriptID.ToString]));
      SQL := qrySQL.FieldByName('SQL_STATEMENT').AsString;
      // Create the query that will be used to perform the transaction.
      Query := TFDQuery.Create(nil);
      // Setup various properties.
      Query.Name := S;
      Query.Connection := conFB;
      Query.FilterOptions := [foCaseInsensitive];
      Query.FormatOptions.DataSnapCompatibility := True;

      // Render the query component to it's string representation.
      FObjectList.Add(ComponentToString(Query));
      // Now set the query to the correct one for applying updates.
      Query := (StringToComponent(FObjectList[I]) as TFDQuery);

      if Length(Trim(GeneratorName)) > 0 then
        Query.UpdateOptions.Generatorname := GeneratorName;

      // NB!! Must set the table/view name here.
      if Length(Trim(TableName)) > 0 then
        Query.UpdateOptions.UpdateTableName := TableName;

      // Add the SQL text for this query. This is required for FireDAC to be
      // able to update the correct table in the DB.
      Query.SQL.Add(SQL);
      // Do the actual updates for this dataset.

//      ErrorCount := LApply.ApplyUpdates(S, Query.Command);
//      if ErrorCount > 0 then
//        raise Exception.Create('An error occurred when posting data. Server error message:');

//      try
      ErrorCount := LApply.ApplyUpdates(S, Query.Command);
      ApplyErrors := LApply.Errors;
      ErrorList.AddStrings(ApplyErrors.Strings)

//      Result := ErrorList.Text;
//      Result := ParseServerErrorMsg(ErrorList.Text);

//      if ErrorCount = 0 then
//        conFB.Commit
//      else
//        conFB.Rollback;

//      except
//        begin
////          FErrorMsg := FErrorMsg + ' ' + E.Message;
//          conFB.Rollback;
//        end;
//      end;

//      try
//        ErrorCount := LApply.ApplyUpdates(S, Query.Command);
////      except on E: Exception do
//      except on E: EIBNativeException {EFDDBEngineException} do // on E: EIBNativeException do
//        begin
//          FErrorMsg := FErrorMsg + ' ' + E.Message;
//          conFB.Rollback;
//        end;
//      end;
    end;

    if ErrorCount > 0 then
      Result := 'RESPONSE=ERROR|ERROR_MESSAGE=' + ParseServerErrorMsg(ErrorList.Text)
    else
      Result := 'RESPONSE=SUCCESS';

//    if ErrorCount = 0 then
//      conFB.Commit
//    else
//      conFB.Rollback;
  finally
//    Result := FErrorMsg;
  end;
end;

function TVBServerMethods.ParseServerErrorMsg(ServerErrorMsg: string): string;
var
  Posn: Integer;
begin
  Result := ServerErrorMsg;
  Posn := Pos(AnsiUpperCase('violation of primary'), AnsiUpperCase(ServerErrorMsg));

  if Posn > 0 then
  begin
    Result := 'Violation of primary key on table: %s ';
    Posn := Pos(AnsiUpperCase('problematic key value'), AnsiUpperCase(ServerErrorMsg));
    if Posn > 0 then
      Result := Result + AnsiRightStr(ServerErrorMsg, Length(ServerErrorMsg) - Posn + 1);
  end;
end;

function TVBServerMethods.EchoString(Request: string; var Response: string): string;
var
  R: Integer;
begin
  R := Random(100000);
  Result := Request + ' ' + R.ToString;
  R := Random(100000);
  Response := Response + ' ' + R.ToString;
end;

function TVBServerMethods.ExecuteSQLCommand(Request: string): string;
//var
//  InputList: TStringList;
begin
//  InputList := RUtils.CreateStringList(PIPE, DOUBLE_QUOTE);
//  InputList.DelimitedText := Request;

  cmdGeneric.CommandText.Clear;
  cmdGeneric.CommandText.Add(Request);
//  cmdGeneric.CommandText.Add(InputList.Values['REQUEST']);
//  try
  conFB.StartTransaction;
  try
    cmdGeneric.Execute;
    conFB.Commit;
    Result := 'RESPONSE=SUCCESS';
  except on E: Exception do
    begin
      Result := 'RESPONSE=ERROR' + PIPE + 'ERROR_MESSAGE=' + E.Message;
      conFB.Rollback;
    end;
  end;
//  finally
//    InputList.Free;
//  end;
end;

function TVBServerMethods.ExecuteStoredProcedure(ProcedureName, ParameterList: string): string;
var
  InputList: TStringList;
begin
  Result := 'RESPONSE=SUCCESS';

  InputList := RUtils.CreateStringList(COMMA, DOUBLE_QUOTE);
  try
    if Length(Trim(ParameterList)) > 0 then
    begin
      InputList.DelimitedText := ParameterList;
      sprGeneric.Close;
      sprGeneric.StoredProcName := '';
      sprGeneric.Params.Clear;
      sprGeneric.StoredProcName := ProcedureName;
      sprGeneric.Prepare;

      if SameText(ProcedureName, 'SP_GEN_BILLABLE_SUMMARY_TABLE') then
      begin
        sprGeneric.ParamByName('USER_ID').Value := InputList[0];
        sprGeneric.ParamByName('THE_PERIOD').Value := InputList[1];
      end

      else if SameText(ProcedureName, 'SP_DELETE_ZERO_BILLABLE_VALUES') then
        sprGeneric.ParamByName('USER_ID').Value := InputList[0];
    end;
  finally
    InputList.Free;
  end;

  try
    sprGeneric.Execute;
  except on E: Exception do
      Result := 'RESPONSE=ERROR|ERROR_MESSAGE=' + E.Message;
  end;
end;

procedure TVBServerMethods.conFBBeforeConnect(Sender: TObject);
var
  ConIniFile: TIniFile;
  IniList: TStringList;
  SectionName: string;
begin
//  conFB.Params.Clear;
//  conFB.Params.LoadFromFile('C:\Data\Firebird\VB\ConnectionDefinitions.ini');
//  conFB.Params.LoadFromFile(CONNECTION_DEFINITION_FILE { + 'ConnectionDefinitions.ini'});

  IniList := RUtils.CreateStringList(COMMA, DOUBLE_QUOTE);
  SectionName := 'VB';
  ConIniFile := TIniFile.Create(CONNECTION_DEFINITION_FILE);

  try
    if SameText(RUtils.GetComputer, 'CVG-NB') then
      SectionName := 'VB Dev';

    ConIniFile.ReadSectionValues(SectionName, IniList);
    conFB.Params.Values['DriverID'] := IniList.Values['DriverID'];
    conFB.Params.Values['Server'] := IniList.Values['Server'];
    conFB.Params.Values['Database'] := IniList.Values['Database'];
    conFB.Params.Values['User_Name'] := IniList.Values['User_Name'];
    conFB.Params.Values['Password'] := IniList.Values['Password'];
    conFB.Params.Values['Protocol'] := IniList.Values['Protocol'];
    conFB.Params.Values['Pooled'] := IniList.Values['Pooled'];
    conFB.Params.Values['CharacterSet'] := IniList.Values['CharacterSet'];
    conFB.Params.Values['CreateDatabase'] := IniList.Values['CreateDatabase'];
    conFB.ResourceOptions.AssignedValues := [rvAutoReconnect];
    conFB.ResourceOptions.AutoReconnect := StringToBoolean(IniList.Values['ResourceOptions.AutoReconnect']);
  finally
    IniList.Free;
    ConIniFile.Free;
  end;
end;

procedure TVBServerMethods.conFBError(ASender, AInitiator: TObject; var AException: Exception);
var
  MyException: EFDDBEngineException;
begin
  if AException is EFDDBEngineException then
  begin
    MyException := EFDDBEngineException(AException);

    if MyException.Kind = ekRecordLocked then
      MyException.Message := 'Please, try the operation later. At moment, the record is busy'
    else if (MyException.Kind = ekUKViolated) and SameText(MyException[0].ObjName, 'UniqueKey_Orders') then
      MyException.Message := 'Please, provide the unique order information. It seems, your order was already put';
  end;
end;

function TVBServerMethods.GetFieldValue(Request: string; FieldType: TFieldType): string;
begin

//  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
//    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
//    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
//    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
//    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
//    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval, // 38..41
//    ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream, //42..48
//    ftTimeStampOffset, ftObject, ftSingle); //49..51

  qrySQL.Open(Request);

  case FieldType of
    ftString, ftWideString:
      begin

      end;

    ftSmallint, ftInteger, ftWord:
      begin

      end;

    ftBoolean:
      begin

      end;

    ftFloat, ftCurrency, ftExtended, ftBCD:
      begin

      end;

    ftDate, ftTime, ftDateTime:
      begin

      end;
  end;
end;

function TVBServerMethods.GetFileVersion(Request: string; var Response: string): string;
var
  InputList, IniList: TStringList;
  SourceFileTimeStamp, TargetFileTimeStamp: TDateTime;
  Folder, {aFileName, }SourceFileName, SourceTimeStampStr, TargetTimeStampStr: string;
  IniFile: TIniFile;
  ComputerName: string;
//  RegKey: TRegistry;
begin
  InputList := RUtils.CreateStringList(PIPE, DOUBLE_QUOTE);
  IniList := RUtils.CreateStringList(COMMA, DOUBLE_QUOTE);
//  RegKey := TRegistry.Create(KEY_ALL_ACCESS);
//  RegKey := System.Win.Registry.TRegistry.Create(KEY_ALL_ACCESS or KEY_WRITE or KEY_WOW64_64KEY);
//  RegKey.RootKey := HKEY_CURRENT_USER;
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSServiceX.ini');

  try
    InputList.DelimitedText := Request;
//    SL.Add('RootKey: ' + IntToStr(RegKey.RootKey));
//    SL.SaveToFile('C:\Data\Download Info.txt');

    IniFile.ReadSectionValues(KEY_RESOURCE, IniList);
    ComputerName := RUtils.GetComputer;

    if not IniFile.ValueExists(KEY_RESOURCE, 'VB Shell Repository') then
      IniFile.WriteString(KEY_RESOURCE, 'VB Shell Repository', '\\' + ComputerName + '\Data\VB\Repository\');

    Folder := IniFile.ReadString(KEY_RESOURCE, 'VB Shell Repository', '\\' + ComputerName + '\Data\VB\Repository\');

//    if not IniFile.ValueExists(KEY_RESOURCE, 'VB Shell Repository') then
//      IniFile.WriteString(KEY_RESOURCE, 'VB Shell Repository', '\\VBSERVER\Data\VB\Repository\');
//
//    Folder := IniFile.ReadString(KEY_RESOURCE, 'VB Shell Repository', '\\VBSERVER\Data\VB\Repository\');
//    SLIni.Add('Folder; ' + Folder);
//    SLIni.SaveToFile('C:\Data\File Info.txt');

    // Make sure these folders exist
    TDirectory.CreateDirectory(Folder);

////    RegKey.OpenKey(KEY_RESOURCE, True);
//    RegKey.OpenKey(IntToStr(Regkey.RootKey) + KEY_RESOURCE, True);
//    SL.Add('KEY_RESOURCE: ' + KEY_RESOURCE);
//    SL.SaveToFile('C:\Data\Download Info.txt');
//
//    if not RegKey.ValueExists('VB Shell Repository') then
//      RegKey.WriteString('VB Shell Repository', '\\VBSERVER\Data\VB\Repository\');
//
//    Folder := RegKey.ReadString('VB Shell Repository');
//    SL.SaveToFile('C:\Data\Download Info.txt');
//
//    // Make sure these folders exist
//    TDirectory.CreateDirectory(Folder);
//    RegKey.CloseKey;

    SourceFileName := Folder + InputList.Values['FILE_NAME'];
    if not TFile.Exists(SourceFileName, True) then
    begin
      Response := 'RESPONSE=FILE_NOT_FOUND';
      Exit;
    end;

//    SourceFileName := Folder + SL.Values['FILE_NAME'];
    // Get the target file name sent by the client. This is the file on the
    // client side that needs to be updated.
    // Get the timestamp of this file.
    //  Note to developer: DON'T USE THE DateTimeToStr function!! For some reason
    // even if the locale settings for date format have been set it still reads
    // this info in mm/dd/yyyy and NOT dd/mm/yyyy format.
    TargetFileTimeStamp := VarToDateTime(InputList.Values['TARGET_FILE_TIMESTAMP']);
    TargetTimeStampStr := FormatDateTime('yyyy-MM-dd hh:mm:ss', TargetFileTimeStamp);
    // If the source file cannot be found, then abort.
//    if not TFile.Exists(SourceFileName, True) then
//    begin
//      // Return message to client and abort.
//      Result := 'RESPONSE=FILE_NOT_FOUND';
//      Exit;
//    end;
    // Get timestamp of source file.
    FileAge(SourceFileName, SourceFileTimeStamp);
    // Get string representation of source file timestamp.
    SourceTimeStampStr := FormatDateTime('yyyy-MM-dd hh:mm:ss', SourceFileTimeStamp);
    // Compare source & target file timestamps. Send this info back to the client.
    // Need to send the new version timestamp back to client so that once the
    // new file has been copied the correct timestamp is applied to the newly
    // copied file. This is because when you write a stream to file the current
    // OS timestamp is applied and this is NOT what we want. (See client for
    // more info on this subject).
    if SourceTimeStampStr > TargetTimeStampStr then
      Result := 'RESPONSE=FOUND_NEW_VERSION' + PIPE + 'FILE_TIMESTAMP=' + SourceTimeStampStr
    else
      Result := 'RESPONSE=NEW_VERSION_NOT_FOUND';
  finally
    InputList.Free;
    IniList.Free;
    IniFile.Free;
//    RegKey.Free;
  end;
end;

function TVBServerMethods.GetNextID(GeneratorName: string): string;
const
  LAST_ID = 'SELECT GEN_ID(%s, 1) AS ID FROM RDB$DATABASE';
begin
  qrySQL.Close;
  qrySQL.Open(Format(LAST_ID, [GeneratorName]));
  Result := IntToStr(qrySQL.FieldByName('ID').AsInteger);
end;

function TVBServerMethods.GetUseCount(Request: string): string;
begin
  qrySQL.Close;
  qrySQL.Open(Request);
  Result := IntToStr(qrySQL.FieldByName('USE_COUNT').AsInteger);
end;

function TVBServerMethods.DownloadFile(Request: string; var Response: string; var Size: Int64): TStream;
var
  Folder, aFileName, ComputerName: string;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSServiceX.ini');
  Result := nil;
  ComputerName := RUtils.GetComputer;

  if not IniFile.ValueExists(KEY_RESOURCE, 'VB Shell Repository') then
    IniFile.WriteString(KEY_RESOURCE, 'VB Shell Repository', '\\' + ComputerName + '\Data\VB\Repository\');

  Folder := IniFile.ReadString(KEY_RESOURCE, 'VB Shell Repository', '\\' + ComputerName + '\Data\VB\Repository\');
  // Make sure these folders exist
  TDirectory.CreateDirectory(Folder);
  aFileName := Folder + Request;

  if not TFile.Exists(aFileName, True) then
  begin
    Response := 'RESPONSE=FILE_NOT_FOUND';
    Exit;
  end;

  Result := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  Size := Result.Size;
  Result.Position := 0;
end;

initialization
  // Specify full path to source otherwise it will try to use the RegisterClass
  // method from the WinApi.Windows unit.
  System.Classes.RegisterClass(Query);

end.

