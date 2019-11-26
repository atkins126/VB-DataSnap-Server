unit VBServer_Container;

interface

uses
  System.SysUtils, System.Classes, DataSnap.DSTCPServerTransport,
  DataSnap.DSHTTPCommon, DataSnap.DSHTTP, DataSnap.DSServer, DataSnap.DSCommonServer,
  IPPeerServer, IPPeerAPI, DataSnap.DSAuth, DbxSocketChannelNative, DbxCompressionFilter;

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
  private
    { Private declarations }
  public
  end;

var
  VBServerContainer: TVBServerContainer;

implementation

{$R *.dfm}

uses
  VBServer_Methods;

procedure TVBServerContainer.DSServerClassGetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := VBServer_Methods.TVBServerMethods;
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

