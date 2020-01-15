program OverviewDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {frmMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
