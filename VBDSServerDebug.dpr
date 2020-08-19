program VBDSServerDebug;

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Main_Frm in 'General\Main_Frm.pas' {MainFrm},
  VBServer_Methods in 'Shared\VBServer_Methods.pas' {VBServerMethods: TDSServerModule},
  VBServer_Container in 'General\VBServer_Container.pas' {VBServerContainer: TDataModule},
  Vcl.Themes,
  Vcl.Styles,
  CommonServiceValues in '..\..\..\Lib\CommonServiceValues.pas',
  RUtils in '..\..\..\Lib\RUtils.pas',
  MyClasses in '..\..\..\Lib\Classes\MyClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.Title := 'VB Apps';
  Application.CreateForm(TVBServerContainer, VBServerContainer);
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

