program VBDSService;

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Main_Frm in 'General\Main_Frm.pas' {MainFrm},
  RUtils in '..\..\..\Lib\RUtils.pas',
  Vcl.Themes,
  Vcl.Styles,
  MyClasses in '..\..\..\Lib\MyClasses.pas',
  CommonServiceValues in '..\..\..\Lib\CommonServiceValues.pas',
  VBServer_Methods in 'Shared\VBServer_Methods.pas' {VBServerMethods: TDSServerModule},
  VBServer_Container in 'Service\VBServer_Container.pas' {VBServerContainer: TService};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

