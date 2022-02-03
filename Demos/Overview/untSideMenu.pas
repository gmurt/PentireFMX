unit untSideMenu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksSideMenu;

type
  TfrmMenu = class(TForm)
    ksSideMenu1: TksSideMenu;
    procedure FormCreate(Sender: TObject);
    procedure ksSideMenu1SelectItem(Sender: TObject; AMenuItemID: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMenu: TfrmMenu;

implementation

uses untHome, untKsInputList, untKsSegmentButtons, untksTabControl;

{$R *.fmx}

procedure TfrmMenu.FormCreate(Sender: TObject);
begin
  ksSideMenu1.Items.Add('HOME', 'Home', nil);
  ksSideMenu1.Items.Add('KSINPUTLIST', 'TksInputList', nil);
  ksSideMenu1.Items.Add('KSSEGMENTBUTTONS', 'TksSegmentButtons', nil);
  ksSideMenu1.Items.Add('KSTABCONTROL', 'TksTabControl', nil);
end;

procedure TfrmMenu.ksSideMenu1SelectItem(Sender: TObject; AMenuItemID: string);
var
  ATarget: TCommonCustomForm;
begin
  ATarget := nil;
  if AMenuItemID = 'HOME' then ATarget := frmHome;
  if AMenuItemID = 'KSINPUTLIST' then ATarget := frmKsInputList;
  if AMenuItemID = 'KSSEGMENTBUTTONS' then ATarget := frmKsSegmentButtons;
  if AMenuItemID = 'KSTABCONTROL' then ATarget := frmKsTabControl;
  if Assigned(ATarget) then
    ksSideMenu1.CloseMenu(ATarget);
end;

end.
