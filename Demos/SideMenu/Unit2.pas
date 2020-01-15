unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm2 = class(TForm)
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
  Form2: TForm2;

implementation

{$R *.fmx}

uses untMenu;

procedure TForm2.SpeedButton1Click(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);
end;

end.
