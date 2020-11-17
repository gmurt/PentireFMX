unit untKsTabControl;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksTabControl,
  FMX.Controls.Presentation, ksToolBar, FMX.StdCtrls, ksSegmentButtons,
  FMX.Layouts;

type
  TfrmKsTabControl = class(TForm)
    ksToolbar1: TksToolbar;
    ksTabControl1: TksTabControl;
    ksTabItem0: TksTabItem;
    q: TLayout;
    Button1: TButton;
    ksSegmentButtons1: TksSegmentButtons;
    ksSegmentButtons2: TksSegmentButtons;
    procedure Button1Click(Sender: TObject);
    procedure ksSegmentButtons2SelectSegment(Sender: TObject; AIndex: Integer;
      AButton: TksSegmentButton);
    procedure ksSegmentButtons1SelectSegment(Sender: TObject; AIndex: Integer;
      AButton: TksSegmentButton);
    procedure ksToolbar1MenuButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKsTabControl: TfrmKsTabControl;

implementation

{$R *.fmx}

uses untSideMenu;

procedure TfrmKsTabControl.Button1Click(Sender: TObject);
var
  AIcon: TksTabItemIcon;
begin
  AIcon := TksTabItemIcon(Random(Ord(High(TksTabItemIcon))));
  ksTabControl1.AddTab.StandardIcon := AIcon;
end;

procedure TfrmKsTabControl.ksSegmentButtons1SelectSegment(Sender: TObject;
  AIndex: Integer; AButton: TksSegmentButton);
begin
  case AIndex of
    0: ksTabControl1.TabPosition := ksTbpBottom;
    1: ksTabControl1.TabPosition := ksTbpTop;
    2: ksTabControl1.TabPosition := ksTbpNone;
  end;
end;

procedure TfrmKsTabControl.ksSegmentButtons2SelectSegment(Sender: TObject;
  AIndex: Integer; AButton: TksSegmentButton);
begin
  case ksSegmentButtons2.ItemIndex of
    0: ksTabControl1.Appearence.Theme := TksTabBarTheme.ksTbLightTabs;
    1: ksTabControl1.Appearence.Theme := TksTabBarTheme.ksTbDarkTabs;
  end;
end;

procedure TfrmKsTabControl.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);
end;

end.
