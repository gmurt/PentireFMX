unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.InertialMovement,
  FMX.Objects, ksImageViewerExt, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm14 = class(TForm)
    ksImageViewerExt1: TksImageViewerExt;
    ToolBar1: TToolBar;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private


    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

var
  Form14: TForm14;

implementation

{$R *.fmx}

{ TForm14 }

procedure TForm14.Button1Click(Sender: TObject);
begin
  ksImageViewerExt1.ZoomIn;

end;

procedure TForm14.Button2Click(Sender: TObject);
begin
  ksImageViewerExt1.ZoomOut;
end;

constructor TForm14.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TForm14.Destroy;
begin
  inherited;
end;

procedure TForm14.FormCreate(Sender: TObject);
begin
  ksImageViewerExt1.Align := TAlignLayout.Client;
end;

end.
