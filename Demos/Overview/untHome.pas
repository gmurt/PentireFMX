unit untHome;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, ksToolBar;

type
  TfrmHome = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Layout2: TLayout;
    Image1: TImage;
    Image2: TImage;
    ksToolbar1: TksToolbar;
    procedure ksToolbar1MenuButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmHome: TfrmHome;

implementation

uses untSideMenu, ksFormStack;

{$R *.fmx}

procedure TfrmHome.FormCreate(Sender: TObject);
begin
  GlobalFormStack.Push(Self, nil);
end;

procedure TfrmHome.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);
end;

end.
