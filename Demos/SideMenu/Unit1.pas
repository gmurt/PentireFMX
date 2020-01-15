unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksSideMenu, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses untMenu;

{$R *.fmx}

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);
end;

end.
