unit VBServer_Container;

interface

uses
  System.SysUtils, System.Classes, System.Win.Registry, WinApi.Windows,

  DataSnap.DSCommonServer, DataSnap.DSTCPServerTransport, DataSnap.DSHTTPCommon,
  DataSnap.DSHTTP, DataSnap.DSServer, IPPeerServer, IPPeerAPI, DataSnap.DSAuth,
  DbxSocketChannelNative, DbxCompressionFilter;

type
  TVBServerContainer = class(TDataModule)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    DSHTTPService: TDSHTTPService;
    DSAuthenticationManager: TDSAuthenticationManager;
    DSServerClass: TDSServerClass;
    procedure DSServerClassGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DSAuthenticationManagerUserAuthenticate(Sender: TObject;
      const Protocol, Context, User, Password: string; var valid: Boolean;
      UserRoles: TStrings);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  VBServerContainer: TVBServerContainer;

implementation

{$R *.dfm}

uses
  VBServer_Methods, CommonServiceValues;

procedure TVBServerContainer.DSServerClassGetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := VBServer_Methods.TVBServerMethods;
end;

procedure TVBServerContainer.DataModuleCreate(Sender: TObject);
var
  RegKey: TRegistry;
  SL: TStringList;
  LS: TObject;
begin
  RegKey := TRegistry.Create(KEY_ALL_ACCESS or KEY_WRITE or KEY_WOW64_64KEY);
  RegKey.RootKey := HKEY_CURRENT_USER;
  RegKey.OpenKey(KEY_DATASNAP, True);
  SL := TStringList.Create;
  SL.Delimiter := PIPE;
  SL.StrictDelimiter := True;
  try
{$IFDEF DEBUG}
    try
      if not Regkey.ValueExists('VB Shell Dev TCP Port') then
        RegKey.WriteString('VB Shell Dev TCP Port', '20220');

      if not RegKey.ValueExists('VB Shell Dev HTTP Port') then
        RegKey.WriteString('VB Shell Dev HTTP Port', '20225');

      DSTCPServerTransport.Port := StrToInt(RegKey.ReadString('VB Shell Dev TCP Port'));
      DSHTTPService.DSPort := StrToInt(RegKey.ReadString('VB Shell Dev TCP Port'));
      DSHTTPService.HttpPort := StrToInt(RegKey.ReadString('VB Shell Dev HTTP Port'));
      SL.DelimitedText := 'Debug Mode' + '|' +
        'PORT = ' + DSTCPServerTransport.Port.ToString + '|' +
        'DSPort = ' + DSHTTPService.DSPort.ToString + '|' +
        ' DSHTTPPort = ' + DSHTTPService.HttpPort.ToString;
      SL.SaveToFile('C:\Data\Ports.txt');
      RegKey.CloseKey;
    finally
      RegKey.Free;
    end;
{$ELSE}
    try
      if not Regkey.ValueExists('VB Shell TCP Port') then
        RegKey.WriteString('VB Shell TCP Port', '20210');

      if not RegKey.ValueExists('VB Shell HTTP Port') then
        RegKey.WriteString('VB Shell HTTP Port', '20215');

      DSTCPServerTransport.Port := StrToInt(RegKey.ReadString('VB Shell TCP Port'));
      DSHTTPService.DSPort := StrToInt(RegKey.ReadString('VB Shell TCP Port'));
      DSHTTPService.HttpPort := StrToInt(RegKey.ReadString('VB Shell HTTP Port'));
      SL.DelimitedText := 'Release Mode' + '|' +
        'PORT = ' + DSTCPServerTransport.Port.ToString + '|' +
        'DSPort = ' + DSHTTPService.DSPort.ToString + '|' +
        ' DSHTTPPort = ' + DSHTTPService.HttpPort.ToString;
      SL.SaveToFile('C:\Data\Ports.txt');
      RegKey.CloseKey;
    finally
      RegKey.Free;
    end;
{$ENDIF}
    DSServer.Start
  finally
    SL.Free;
  end;
end;

procedure TVBServerContainer.DataModuleDestroy(Sender: TObject);
begin
  DSServer.Stop;
end;

procedure TVBServerContainer.DSAuthenticationManagerUserAuthenticate(
  Sender: TObject; const Protocol, Context, User, Password: string;
  var valid: Boolean; UserRoles: TStrings);
begin
  { TODO : Validate the client user and password.
    If role-based authorization is needed, add role names to the UserRoles parameter  }
  valid := True;
end;

end.



