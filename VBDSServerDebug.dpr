program VBDSServerDebug;

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  Main_Frm in 'General\Main_Frm.pas' {MainFrm},
  VBServer_Methods in 'Shared\VBServer_Methods.pas' {VBServerMethods: TDSServerModule},
  VBServer_Container in 'General\VBServer_Container.pas' {VBServerContainer: TDataModule},
  RUtils in '..\..\..\Lib\RUtils.pas',
  Vcl.Themes,
  Vcl.Styles,
  MyClasses in '..\..\..\Lib\MyClasses.pas',
  CommonServiceValues in '..\..\..\Lib\CommonServiceValues.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Iceberg Classico');
  Application.CreateForm(TVBServerContainer, VBServerContainer);
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

