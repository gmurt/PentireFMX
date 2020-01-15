unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm3 = class(TForm)
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
  Form3: TForm3;

implementation

{$R *.fmx}

uses untMenu;

procedure TForm3.SpeedButton1Click(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);

end;

end.
