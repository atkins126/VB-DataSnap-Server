program VBDSServiceX;

uses
  Vcl.SvcMgr,
  RUtils in '..\..\..\Lib\RUtils.pas',
  CommonServiceValues in '..\..\..\Lib\CommonServiceValues.pas',
  VBServer_Methods in 'Shared\VBServer_Methods.pas' {VBServerMethods: TDSServerModule},
  VBServer_Container in 'Service\VBServer_Container.pas' {VBDSServerX: TService},
  MyClasses in '..\..\..\Lib\Classes\MyClasses.pas';

{$R *.res}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;

  Application.CreateForm(TVBDSServerX, VBDSServerX);
  Application.Run;
end.

