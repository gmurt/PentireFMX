unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  ksInputList, FMX.Objects, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.Types, FMX.ListBox;

type
  TfrmMain = class(TForm)
    Image1: TImage;
    ToolBar3: TToolBar;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ksInputList1: TksInputList;
    ToolBar1: TToolBar;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);

    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ksInputList1ItemButtonClick(Sender: TObject;
      AItem: TksInputListButtonItem; AID: string);
    procedure ksInputList1ItemTrackBarChange(Sender: TObject;
      AItem: TksInputListTrackBarItem; AID: string; AValue: Single);
    procedure ksInputList1ItemSwitchChanged(Sender: TObject;
      AItem: TksInputListSwitchItem; AID: string; AIsChecked: Boolean);
  private
    { Private declarations }
  public
    FClipboard: string;
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses System.UIConsts, Json, FMX.DialogService;

{$R *.fmx}

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ksInputList1.Reset;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  AJson: TJsonobject;
begin
  AJson := TJsonobject.Create;
  try
    ksInputList1.SaveToJson(AJson, True, True);
    FClipboard := AJson.ToString;
  finally
    AJson.Free;

  end;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  AJson: TJsonobject;
begin
  AJson := TJSONObject.ParseJSONValue(FClipboard) as TJSONObject;;
  try
    ksInputList1.LoadFromJson(AJson, True, True);
  finally
    AJson.Free;
  end;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  ksInputList1.ClearItems;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
var
  ICount: integer;
begin
  for ICount := 1 to 3 do
  begin
  //ksInputList1.BeginUpdate;
  ksInputList1.Items.AddSeperator('SETTINGS');
  ksInputList1.Items.AddItemSelector('LOOKUP_1', Image1.Bitmap, 'Selector Item', '4', ['1','2','3','4','5','6','7','8','9','10']);
  ksInputList1.Items.AddEditBoxItem('PHONEEDIT_'+ICount.ToString, Image1.Bitmap, 'Phone Keyboard', '', 'NUMBER', TVirtualKeyboardType.PhonePad);
  ksInputList1.Items.AddEditBoxItem('URLEDIT_'+ICount.ToString, Image1.Bitmap, 'URL Keyboard', '', 'URL', TVirtualKeyboardType.URL);
  ksInputList1.Items.AddSwitchItem('SWITCH_1', Image1.Bitmap, 'Item 3', False);
  ksInputList1.Items.AddSwitchItem('SWITCH_2', Image1.Bitmap, 'Item 4', True);
  ksInputList1.Items.AddCheckBoxItem('CHECKBOX_1', Image1.Bitmap, 'Item 5', False);
  ksInputList1.Items.AddCheckBoxItem('CHECKBOX_2', Image1.Bitmap, 'Item 6', True);
  ksInputList1.Items.AddButtonItem('BUTTON_1', Image1.Bitmap, 'Item 7', 'Test');
  ksInputList1.Items.AddTrackBar('TRACKBAR_1', Image1.Bitmap, 'Item 8', 50, 100);
  //ksInputList1.EndUpdate;
end;
end;

procedure TfrmMain.ksInputList1ItemButtonClick(Sender: TObject;
  AItem: TksInputListButtonItem; AID: string);
begin
  ShowMessage('Button clicked: '+AID);
end;

procedure TfrmMain.ksInputList1ItemSwitchChanged(Sender: TObject;
  AItem: TksInputListSwitchItem; AID: string; AIsChecked: Boolean);
begin
  AItem.Title := (AID+'  '+BoolToStr(AIsChecked, True));
end;

procedure TfrmMain.ksInputList1ItemTrackBarChange(Sender: TObject;
  AItem: TksInputListTrackBarItem; AID: string; AValue: Single);
begin
  Caption := FloatToStr(AValue);
  AItem.Title := AValue.ToString;
end;


end.
