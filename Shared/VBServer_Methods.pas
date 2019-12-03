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
    procedure DSServerModuleCreate(Sender: TObject);
    procedure DSServerModuleDestroy(Sender: TObject);
    procedure conFBBeforeConnect(Sender: TObject);
    procedure conFBError(ASender, AInitiator: TObject; var AException: Exception);
  private
    { Private declarations }
    FErrorMsg: string;
    FSLObject: TStringList;

    property SLObject: TStringList read FSLObject write FSLObject;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
  public
    { Public declarations }
    function GetData(Request, ParameterList, Generatorname, Tablename, DataSetName: string; var Response: string): TFDJSONDataSets;
    function ApplyDataUpdates(const DeltaList: TFDJSONDeltas; var Response: string; Generatorname, Tablename: string): string;
    function ExecuteSQLCommand(Request: string; var Reponse: string): string;
    function GetFileVersion(Request: string; var Response: string): string;
    function DownloadFile(Request: string; var Response: string; var Size: Int64): TStream;
    function TestType(Request: string; var Response: string): string;
    function EchoString(Request: string; var Response: string): string;
    function ExecuteStoredProcedure(ProcedureName, ParameterList: string): string;
// SOAP Method for downloading files
//    function DownloadFile(Request: string; var Response: string): TByteDynArray;
  end;
{$METHODINFO Off}

  Query = class(TFDQuery);

implementation

{$R *.dfm}

uses
  RUtils,
  CommonServiceValues;

{ TVBServerMethods }

//function TVBServerMethods.GetData(Request, ParameterList: string; var Response: string): TFDJSONDataSets;
//var
//  SL: TStringList;
//begin
//  SL := RUtils.CreateStringList(SL, PIPE);
//  SL.DelimitedText := Request;
//  Result := SL.Values['REQUEST'];
//  Response := 'Request = ' + Request + '  RESPONSE=SUCCESS';
//end;

procedure TVBServerMethods.DSServerModuleCreate(Sender: TObject);
begin
  FSLObject := TStringList.Create;
end;

procedure TVBServerMethods.DSServerModuleDestroy(Sender: TObject);
begin
  FSLObject.Free;
end;

function TVBServerMethods.TestType(Request: string; var Response: string): string;
var
  SL: TStringList;
begin
  SL := RUtils.CreateStringList(PIPE, SINGLE_QUOTE);
  SL.DelimitedText := Request;
  Result := SL.Values['REQUEST'];
  Response := 'SUCCESS';
end;

function TVBServerMethods.GetData(Request, ParameterList, Generatorname, Tablename, DataSetName: string; var Response: string): TFDJSONDataSets;
var
  SL, SLScriptID, SLParameterList, SLParameter, SLDataSetName: TStringList;
  I, X: Integer;
  Query: TFDQuery;
  SQL, TheDataSetName: string;
begin
  { Notes to developer:

    1. See the SCRIPT_GROUP and SCRIPT tables.

    2. The request for data comes from the client in the form of delimited string
       values.

    3. SL contains the full un-parsed request string.

    4. SLScriptID contains the list of ID's used to retrieve the SQL statement
       needed to fetch data from the server.
  }
  Response := '';
  SL := RUtils.CreateStringList(PIPE, SINGLE_QUOTE);
  SLScriptID := RUtils.CreateStringList(COMMA, SINGLE_QUOTE);
  SLParameterList := RUtils.CreateStringList(PIPE, SINGLE_QUOTE);
  SLParameter := RUtils.CreateStringList(SEMI_COLON, SINGLE_QUOTE);
  SLDataSetName := RUtils.CreateStringList(SEMI_COLON, SINGLE_QUOTE);

  try
    SL.DelimitedText := Request;
    SLParameterList.DelimitedText := ParameterList;
    SLParameter.DelimitedText := SLParameterList.Values['PARAMETER_LIST'];

    for I := SLParameter.Count - 1 downto 0 do
      if Length(SLParameter[I]) = 0 then
        SLParameter.Delete(I);

    // Create the Dataset list that will contain the data to be passed back to
    // the client.
    Result := TFDJSONDataSets.Create;
    FSLObject.Clear;

    SLScriptID.DelimitedText := SL.Values['SQL_STATEMENT_ID'];
    // Iterate the list of ID's to retriev the SQL Statements.
    for I := 0 to SLScriptID.Count - 1 do
    begin
      qrySQL.Open(Format(SQL_STATEMENT, [SLScriptID[I]]));
      // Code added by CVG on 26/06/2019. This is to accommodate datasets
      // with different names but getting their data from the same DB table
      // on the client side.
      SLDataSetName.DelimitedText := qrySQL.FieldByName('DATASET_NAME').AsString;
      for X := 0 to SLDataSetName.Count - 1 do
        if SameText(DataSetName, SLDataSetName[X]) then
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

      FSLObject.Add(ComponentToString(Query));
      Query := (StringToComponent(FSLObject[I]) as TFDQuery);
      SQL := qrySQL.FieldByName('SQL_STATEMENT').AsString;

      if SLParameter.Count > 0 then
        SQL := Format(SQL, BuildFormatArray(SLParameter.Count, SLParameter));

      Query.Open(SQL);
      if Query.IsEmpty then
        Response := 'NO_DATA';
      // Add the data to the dataset list to be returned to the client.
      TFDJSONDataSetsWriter.ListAdd(Result, Query.Name, Query);
    end;
  finally
    SL.Free;
    SLScriptID.Free;
    SLParameterList.Free;
    SLParameter.Free;
  end;
end;

function TVBServerMethods.ApplyDataUpdates(const DeltaList: TFDJSONDeltas; var Response: string; Generatorname, Tablename: string): string;
var
  LApply: IFDJSONDeltasApplyUpdates;
  ListCount, I: Integer;
  S, SQL {, ErrorMsg}: string;
  Query: TFDQuery;
begin
  Result := '';
  ErrorMsg := '';

  // Create the apply object and populate it with the delta from the
  // FDMemTable(s) generated by the client.
  LApply := TFDJSONDeltasApplyUpdates.Create(DeltaList);
  // Get a count of the number of datasets to be updated.
  ListCount := LApply.Count;

  conFB.StartTransaction;
  FSLObject.Clear;

  try
    for I := 0 to ListCount - 1 do
    begin
      // Get the Query name for the dataset that is used to transact.
      S := LApply.Items[I].Key;
      // Get the SQL statement used to generate field data.
      qrySQL.Open(Format(SQL_DATASET_NAME, [AnsiQuotedStr(AnsiUpperCase(S), '''')]));
      SQL := qrySQL.FieldByName('SQL_STATEMENT').AsString;
      // Create the query that will be used to perform the transaction.
      Query := TFDQuery.Create(nil);
      // Setup various properties.
      Query.Name := S;
      Query.Connection := conFB;
      Query.FilterOptions := [foCaseInsensitive];
      Query.FormatOptions.DataSnapCompatibility := True;

      // Render the query component to it's string representation.
      FSLObject.Add(ComponentToString(Query));
      // Now set the query to the correct one for applying updates.
      Query := (StringToComponent(FSLObject[I]) as TFDQuery);

      if Length(Trim(Generatorname)) > 0 then
        Query.UpdateOptions.Generatorname := Generatorname;

      // NB!! Must set the table/view name here.
      if Length(Trim(Tablename)) > 0 then
        Query.UpdateOptions.UpdateTableName := Tablename;

      // Add the SQL text for this query. This is required for FireDAC to be
      // able to update the correct table in the DB.
      Query.SQL.Add(SQL);
      // Do the actual updates for this dataset.
      try
        LApply.ApplyUpdates(S, Query.Command);
//      except on E: Exception do
      except on E: EFDDBEngineException do // on E: EIBNativeException do
        begin
          ErrorMsg := ErrorMsg + ' ' + E.Message;
          conFB.Rollback;
        end;
      end;
      conFB.Commit;
    end;
  finally
    Result := FErrorMsg;
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

function TVBServerMethods.ExecuteSQLCommand(Request: string; var Reponse: string): string;
var
  SL: TStringList;
begin
  SL := RUtils.CreateStringList(PIPE, SINGLE_QUOTE);
  SL.DelimitedText := Request;

  cmdGeneric.CommandText.Clear;
  cmdGeneric.CommandText.Add(SL.Values['REQUEST']);
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
end;

function TVBServerMethods.ExecuteStoredProcedure(ProcedureName, ParameterList: string): string;
var
  SL: TStringList;
begin
  Result := 'RESPONSE=SUCCESS';

  SL := RUtils.CreateStringList(COMMA, SINGLE_QUOTE);
  try
    if Length(Trim(ParameterList)) > 0 then
    begin
      SL.DelimitedText := ParameterList;
      sprGeneric.Close;
      sprGeneric.StoredProcName := '';
      sprGeneric.Params.Clear;
      sprGeneric.StoredProcName := ProcedureName;
      sprGeneric.Prepare;

//    for I := 0 to SL.Count - 1 do
//    begin
//      Param := sprGeneric.Params.Add;
//      Param.ParamType := ptInput;
//      Param.DataType := ftInteger;
//      Param.Value := SL[I];
//    end;
//
//  sprGeneric.Params[0].Name := 'USER_ID';
//  sprGeneric.Params[1].Name := 'THE_PERIOD';

      if SameText(ProcedureName, 'SP_GEN_BILLABLE_SUMMARY_TABLE') then
      begin
        sprGeneric.ParamByName('USER_ID').Value := SL[0];
        sprGeneric.ParamByName('THE_PERIOD').Value := SL[1];
      end

      else if SameText(ProcedureName, 'SP_DELETE_ZERO_BILLABLE_VALUES') then
        sprGeneric.ParamByName('USER_ID').Value := SL[0];
    end;
  finally
    SL.Free;
  end;

  try
//      conFB.StartTransaction;
    sprGeneric.Execute;
//      conFB.Commit;
  except on E: Exception do
    begin
      Result := 'RESPONSE=ERROR|ERROR_MSG=' + E.Message;
//        conFB.Rollback;
    end;
  end;
end;

procedure TVBServerMethods.conFBBeforeConnect(Sender: TObject);
var
  ConIniFile: TIniFile;
  SLIni: TStringList;
  SectionName: string;
begin
//  conFB.Params.Clear;
//  conFB.Params.LoadFromFile('C:\Data\Firebird\VB\ConnectionDefinitions.ini');
//  conFB.Params.LoadFromFile(CONNECTION_DEFINITION_FILE { + 'ConnectionDefinitions.ini'});

  SLIni := RUtils.CreateStringList(COMMA, SINGLE_QUOTE);
  SectionName := 'VB';
    ConIniFile := TIniFile.Create(CONNECTION_DEFINITION_FILE);

  try
    if SameText(RUtils.GetComputer, 'CVG-NB') then
      SectionName := 'VB Dev';

    ConIniFile.ReadSectionValues(SectionName, SLIni);
    conFB.Params.Values['Driver'] := SLIni.Values['DriverID'];
    conFB.Params.Values['Server'] := SLIni.Values['Server'];
    conFB.Params.Values['Database'] := SLIni.Values['Database'];
    conFB.Params.Values['User_Name'] := SLIni.Values['User_Name'];
    conFB.Params.Values['Password'] := SLIni.Values['Password'];
    conFB.Params.Values['Protocol'] := SLIni.Values['Protocol'];
    conFB.Params.Values['Pooled'] := SLIni.Values['Pooled'];
    conFB.Params.Values['CharacterSet'] := SLIni.Values['CharacterSet'];
    conFB.Params.Values['CreateDatabase'] := SLIni.Values['CreateDatabase'];
    conFB.ResourceOptions.AssignedValues := [rvAutoReconnect];
    conFB.ResourceOptions.AutoReconnect := StringToBoolean(SLIni.Values['ResourceOptions.AutoReconnect']);
  finally
    SLIni.Free;
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

function TVBServerMethods.GetFileVersion(Request: string; var Response: string): string;
var
  SL, SLIni: TStringList;
  SourceFileTimeStamp, TargetFileTimeStamp: TDateTime;
  Folder, {aFileName, }SourceFileName, SourceTimeStampStr, TargetTimeStampStr: string;
  IniFile: TIniFile;
  ComputerName: string;
//  RegKey: TRegistry;
begin
  SL := RUtils.CreateStringList(PIPE, SINGLE_QUOTE);
  SLIni := RUtils.CreateStringList(COMMA, SINGLE_QUOTE);
//  RegKey := TRegistry.Create(KEY_ALL_ACCESS);
//  RegKey := System.Win.Registry.TRegistry.Create(KEY_ALL_ACCESS or KEY_WRITE or KEY_WOW64_64KEY);
//  RegKey.RootKey := HKEY_CURRENT_USER;
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSServiceX.ini');

  try
    SL.DelimitedText := Request;
//    SL.Add('RootKey: ' + IntToStr(RegKey.RootKey));
//    SL.SaveToFile('C:\Data\Download Info.txt');

    IniFile.ReadSectionValues(KEY_RESOURCE, SLIni);
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

    SourceFileName := Folder + SL.Values['FILE_NAME'];
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
    TargetFileTimeStamp := VarToDateTime(SL.Values['TARGET_FILE_TIMESTAMP']);
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
    SL.Free;
    SLIni.Free;
    IniFile.Free;
//    RegKey.Free;
  end;
end;

function TVBServerMethods.DownloadFile(Request: string; var Response: string; var Size: Int64): TStream;
var
  SL, SLIni: TStringList;
  Folder, aFileName, ComputerName: string;
  IniFile: TIniFile;
begin
  SL := RUtils.CreateStringList(PIPE, SINGLE_QUOTE);
  SLIni := RUtils.CreateStringList(COMMA, SINGLE_QUOTE);
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'VSServiceX.ini');
  Response := '';
  Result := nil;

  try
    SL.DelimitedText := Request;
    ComputerName := RUtils.GetComputer;

    if not IniFile.ValueExists(KEY_RESOURCE, 'VB Shell Repository') then
      IniFile.WriteString(KEY_RESOURCE, 'VB Shell Repository', '\\' + ComputerName + '\Data\VB\Repository\');

    Folder := IniFile.ReadString(KEY_RESOURCE, 'VB Shell Repository', '\\' + ComputerName + '\Data\VB\Repository\');
    // Make sure these folders exist
    TDirectory.CreateDirectory(Folder);
    aFileName := Folder + SL.Values['FILE_NAME'];

    if not TFile.Exists(aFileName, True) then
    begin
      Response := 'RESPONSE=FILE_NOT_FOUND';
      Exit;
    end;

    Result := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
    Size := Result.Size;
    Result.Position := 0;
  finally
    SL.Free;
    SLIni.Free;
  end;
end;

// SOAP Method for downloading files
//function TVBServerMethods.DownloadFile(Request: string; var Response: string): TByteDynArray;
//var
//  MemStream: TMemoryStream;
//  SL: TStringList;
//  Folder, aFileName: string;
//  RegKey: TRegistry;
//begin
//  SL := RUtils.CreateStringList(SL, PIPE);
//  RegKey := TRegistry.Create(KEY_ALL_ACCESS);
//  MemStream := TMemoryStream.Create;
//  try
//    SL.DelimitedText := Request;
//    //    ProcessRegistry;
//    RegKey.RootKey := HKEY_CURRENT_USER;
//    RegKey.OpenKey(KEY_COMMON_RESOURCE_FILES, True);
//
//    if not RegKey.ValueExists('RC Shell Resource Files') then
//      RegKey.WriteString('RC Shell Resource Files', '\\RC-FS-2012\Apps\RC Shell\');
//
//    Folder := RegKey.ReadString('RC Shell Resource Files') + Folder;
//    // Make sure these folders exist
////    TDirectory.CreateDirectory(Folder + 'Applications\');
////    TDirectory.CreateDirectory(Folder + 'Installers\');
//    TDirectory.CreateDirectory(Folder + 'Resources\');
//    //    TDirectory.CreateDirectory(Folder + 'Documents\');
//    RegKey.CloseKey;
//
//    case StrToInt(SL.Values['FILE_TYPE']) of
//      1: Folder := Folder + 'Applications\';
//      2: Folder := Folder + 'Installers\';
//      3: Folder := Folder + 'Resources\';
//      4: Folder := Folder + 'Documents\';
//    end;
//
//    aFileName := Folder + SL.Values['FILE_NAME'];
//    if not TFile.Exists(aFileName, True) then
//    begin
//      // Return a zero length byte array to client.
//      SetLength(Result, 0);
//      Exit;
//    end;
//
//    // Load file into stream
//    MemStream.LoadFromFile(aFileName);
//    // Set size of byte array to send back to client
//    SetLength(Result, MemStream.Size);
//    // Make sure we begin reading at beginning of stream
//    MemStream.Position := 0;
//    // Read stream into byte array
//    MemStream.Read(Result[0], MemStream.Size);
//  finally
//    MemStream.Position := 0;
//    MemStream.Free;
//    SL.Free;
//    RegKey.Free;
//  end;
//end;

initialization
  // Specify full path to source otherwise it will try to use the RegistrClass
  // method from the WinApi.Windows unit.
  System.Classes.RegisterClass(Query);

end.

