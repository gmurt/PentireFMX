unit untKsSegmentButtons;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  ksSegmentButtons, FMX.StdCtrls, FMX.Controls.Presentation, ksToolBar;

type
  TfrmKsSegmentButtons = class(TForm)
    ksToolbar1: TksToolbar;
    ToolBar1: TToolBar;
    ksSegmentButtons1: TksSegmentButtons;
    ksSegmentButtons2: TksSegmentButtons;
    ksSegmentButtons3: TksSegmentButtons;
    ksSegmentButtons4: TksSegmentButtons;
    procedure ksToolbar1MenuButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKsSegmentButtons: TfrmKsSegmentButtons;

implementation

{$R *.fmx}

uses untSideMenu;

procedure TfrmKsSegmentButtons.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);
end;

end.
