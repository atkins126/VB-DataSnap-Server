unit VBServer_Container;

interface

uses
  System.SysUtils, System.Classes, Vcl.SvcMgr, System.Win.Registry,

  CommonServiceValues,

  DataSnap.DSTCPServerTransport, DataSnap.DSHTTPCommon, DataSnap.DSHTTP,
  DataSnap.DSServer, DataSnap.DSCommonServer, IPPeerServer, IPPeerAPI,
  DataSnap.DSAuth, DbxSocketChannelNative, DbxCompressionFilter;

type
  TVBDSServerX = class(TService)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    DSHTTPService: TDSHTTPService;
    DSAuthenticationManager: TDSAuthenticationManager;
    DSServerClass: TDSServerClass;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceAfterInstall(Sender: TService);

    procedure DSServerClassGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);

    procedure DSAuthenticationManagerUserAuthorize(Sender: TObject;
      EventObject: TDSAuthorizeEventObject; var valid: Boolean);

    procedure DSAuthenticationManagerUserAuthenticate(Sender: TObject;
      const Protocol, Context, User, Password: string; var valid: Boolean;
      UserRoles: TStrings);
    procedure ServiceCreate(Sender: TObject);
  private
    { Private declarations }
  protected
    function DoStop: Boolean; override;
    function DoPause: Boolean; override;
    function DoContinue: Boolean; override;
    procedure DoInterrogate; override;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  VBDSServerX: TVBDSServerX;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  Winapi.Windows,
  VBServer_Methods;

procedure TVBDSServerX.DSServerClassGetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := VBServer_Methods.TVBServerMethods;
end;

procedure TVBDSServerX.DSAuthenticationManagerUserAuthenticate(
  Sender: TObject; const Protocol, Context, User, Password: string;
  var valid: Boolean; UserRoles: TStrings);
begin
  { TODO : Validate the client user and password.
    If role-based authorization is needed, add role names to the UserRoles parameter  }
  valid := True;
end;

procedure TVBDSServerX.DSAuthenticationManagerUserAuthorize(
  Sender: TObject; EventObject: TDSAuthorizeEventObject;
  var valid: Boolean);
begin
  { TODO : Authorize a user to execute a method.
    Use values from EventObject such as UserName, UserRoles, AuthorizedRoles and DeniedRoles.
    Use DSAuthenticationManager1.Roles to define Authorized and Denied roles
    for particular server methods. }
  valid := True;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  VBDSServerX.Controller(CtrlCode);
end;

function TVBDSServerX.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

function TVBDSServerX.DoContinue: Boolean;
begin
  Result := inherited;
  DSServer.Start;
end;

procedure TVBDSServerX.DoInterrogate;
begin
  inherited;
end;

function TVBDSServerX.DoPause: Boolean;
begin
  DSServer.Stop;
  Result := inherited;
end;

function TVBDSServerX.DoStop: Boolean;
begin
  DSServer.Stop;
  Result := inherited;
end;

procedure TVBDSServerX.ServiceStart(Sender: TService; var Started: Boolean);
begin
  DSServer.Start;
end;

procedure TVBDSServerX.ServiceAfterInstall(Sender: TService);
var
  RegKey: TRegistry;
begin
  RegKey := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    RegKey.RootKey := HKEY_LOCAL_MACHINE;
    // Name here comes from the System.Classes unit.
    if RegKey.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      RegKey.WriteString('Description',
        'VB DataSnap service(X). This is the primary service' +
        ' used to give clients access to the VB service remote invokable methods.');
      RegKey.CloseKey;
    end;
  finally
    RegKey.Free;
  end;
end;

procedure TVBDSServerX.ServiceCreate(Sender: TObject);
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
      SL.SaveToFile('C:\Data\PortsX.txt');
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
      SL.SaveToFile('C:\Data\PortsX.txt');
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

end.

