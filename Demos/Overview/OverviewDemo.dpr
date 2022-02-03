program OverviewDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  untKsInputList in 'untKsInputList.pas' {frmKsInputList},
  untSideMenu in 'untSideMenu.pas' {frmMenu},
  untHome in 'untHome.pas' {frmHome},
  untKsSegmentButtons in 'untKsSegmentButtons.pas' {frmKsSegmentButtons},
  untKsTabControl in 'untKsTabControl.pas' {frmKsTabControl};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmHome, frmHome);
  Application.CreateForm(TfrmKsInputList, frmKsInputList);
  Application.CreateForm(TfrmKsSegmentButtons, frmKsSegmentButtons);
  Application.CreateForm(TfrmKsTabControl, frmKsTabControl);
  Application.CreateForm(TfrmMenu, frmMenu);
  Application.Run;
end.


