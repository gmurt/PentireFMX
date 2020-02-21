program ImageViewerDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form14};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
