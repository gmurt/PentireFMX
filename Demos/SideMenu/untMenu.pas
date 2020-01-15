unit untMenu;

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

uses Unit1, Unit2, Unit3;

{$R *.fmx}

procedure TfrmMenu.FormCreate(Sender: TObject);
begin
  ksSideMenu1.Items.Add('FORM1', 'Form 1', nil);
  ksSideMenu1.Items.Add('FORM2', 'Form 2', nil);
  ksSideMenu1.Items.Add('FORM3', 'Form 3', nil);
end;

procedure TfrmMenu.ksSideMenu1SelectItem(Sender: TObject; AMenuItemID: string);
begin
  if AMenuItemID = 'FORM1' then ksSideMenu1.CloseMenu(Form1);
  if AMenuItemID = 'FORM2' then ksSideMenu1.CloseMenu(Form2);
  if AMenuItemID = 'FORM3' then ksSideMenu1.CloseMenu(Form3);
end;

end.
