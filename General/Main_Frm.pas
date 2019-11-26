unit Main_Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Vcl.Controls, Vcl.Forms,
  System.Variants, IPPeerServer, System.Classes, Vcl.Graphics, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Imaging.jpeg, Vcl.ExtCtrls, dxGDIPlusClasses{, JclSysInfo};

type
  TMainFrm = class(TForm)
    cxImage2: TImage;
    lblServerDescription: TLabel;
    lblVersion: TLabel;
    lblComplieDateTime: TLabel;
    lblTCPPort: TLabel;
    lblHTTPPort: TLabel;
    lblInfo: TLabel;
    lblLocalMachineName: TLabel;
    lblLocLIPAddress: TLabel;
    Image1: TImage;
    Bevel1: TBevel;
    lblTitle: TLabel;
    lblVersionInfo: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

uses
  RUtils,
  VBServer_Container;

procedure TMainFrm.FormCreate(Sender: TObject);
var
  AppVersion: string;
  FileDT: TDateTime;
const
  LABEL_COLOUR = $6F3700;
begin
  Caption := Application.Title;
  lblTitle.Caption := 'van Brakel && Associates';
  lblServerDescription.Caption := Application.Title;
  lblTitle.Font.Color := LABEL_COLOUR;
  lblServerDescription.Font.Color := LABEL_COLOUR;
  lblInfo.Font.Color := LABEL_COLOUR;
  lblTCPPort.Font.Color := LABEL_COLOUR;
  lblHTTPPort.Font.Color := LABEL_COLOUR;
  lblLocalMachineName.Font.Color := LABEL_COLOUR;
  lblLocLIPAddress.Font.Color := LABEL_COLOUR;
  lblVersion.Font.Color := LABEL_COLOUR;
  lblComplieDateTime.Font.Color := LABEL_COLOUR;
  lblVersionInfo.Font.Color := LABEL_COLOUR;

  AppVersion := GetBuildInfo(Application.ExeName, rbLongFormat);
  FileAge(Application.ExeName, FileDT);
  lblVersion.Caption := 'Ver: ' + AppVersion;
  lblComplieDateTime.Caption := 'Complie Timestamp: ' + FormatDateTime('dd/mm/yyyy  HH:NN', FileDT);

  lblVersion.Update;
  lblComplieDateTime.Update;
  Constraints.MaxHeight := Height;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;

  lblHTTPPort.Caption := 'HTTP Port: ' + VBServerContainer.DSHTTPService.HttpPort.ToString;
  lblTCPPort.Caption := 'TC:/IP Port: ' + VBServerContainer.DSHTTPService.DSPort.ToString;

  lblLocalMachineName.Caption := 'Local Machine: ' + RUtils.GetComputer;
  lblLocLIPAddress.Caption := 'Local IP (V4): ' + RUtils.GetIPAddress(RUtils.GetComputer);


//  lblLocalMachineName.Caption := 'Local Machine: ' + JclSysInfo.GetLocalComputerName;
//  lblLocLIPAddress.Caption := 'Local IP (V4): ' + JclSysInfo.GetIPAddress(JclSysInfo.GetLocalComputerName);
end;

end.

