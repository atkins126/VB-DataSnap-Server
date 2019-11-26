unit VBServer_Container;

interface

uses
  System.SysUtils, System.Classes, Vcl.SvcMgr, System.Win.Registry,
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

procedure TVBDSServerX.ServiceStart(Sender: TService; var Started: Boolean);
begin
  DSServer.Start;
end;
end.

