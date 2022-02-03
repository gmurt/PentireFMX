unit untKsInputList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  ksInputList, FMX.Objects, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.Types, FMX.ListBox,
  ksToolBar, FMX.Gestures;

type
  TfrmKsInputList = class(TForm)
    Image1: TImage;
    ksInputList1: TksInputList;
    ToolBar1: TToolBar;
    ksToolbar1: TksToolbar;
    GestureManager1: TGestureManager;
    procedure FormCreate(Sender: TObject);
    procedure ksInputList1ItemButtonClick(Sender: TObject;
      AItem: TksInputListButtonItem; AID: string);
    procedure ksInputList1ItemTrackBarChange(Sender: TObject;
      AItem: TksInputListTrackBarItem; AID: string; AValue: Single);
    procedure ksInputList1ItemSwitchChanged(Sender: TObject;
      AItem: TksInputListSwitchItem; AID: string; AIsChecked: Boolean);
    procedure ksToolbar1MenuButtonClick(Sender: TObject);
    procedure ksInputList1ItemClick(Sender: TObject;
      AItem: TksBaseInputListItem; AID: string);
    procedure ksToolbar1Click(Sender: TObject);
  private
    { Private declarations }
  public
    FClipboard: string;
    { Public declarations }
  end;

var
  frmKsInputList: TfrmKsInputList;

implementation

uses System.UIConsts, Json, FMX.DialogService, untSideMenu;

{$R *.fmx}

procedure TfrmKsInputList.FormCreate(Sender: TObject);
var
  ICount: integer;
begin
  for ICount := 1 to 30 do
  begin
    ksInputList1.Items.AddSeperator('SETTINGS');
    ksInputList1.Items.AddItemSelector('LOOKUP_1', Image1.Bitmap, 'Selector Item', '4', ['1','2','3','4','5','6','7','8','9','10']);
    ksInputList1.Items.AddEditBoxItem('PHONEEDIT_'+ICount.ToString, Image1.Bitmap, 'Phone Keyboard', '', 'NUMBER', TVirtualKeyboardType.PhonePad);
    ksInputList1.Items.AddEditBoxItem('EDIT_'+ICount.ToString, Image1.Bitmap, 'Normal Keyboard', '', 'TEXT', TVirtualKeyboardType.Default);
    ksInputList1.Items.AddSwitchItem('SWITCH_1', Image1.Bitmap, 'Item 3', False);
    ksInputList1.Items.AddSwitchItem('SWITCH_2', Image1.Bitmap, 'Item 4', True);
    ksInputList1.Items.AddCheckBoxItem('CHECKBOX_1', Image1.Bitmap, 'Item 5', False);
    ksInputList1.Items.AddCheckBoxItem('CHECKBOX_2', Image1.Bitmap, 'Item 6', True);
    ksInputList1.Items.AddButtonItem('BUTTON_1', Image1.Bitmap, 'Item 7', 'Test');
  end;
end;

procedure TfrmKsInputList.ksInputList1ItemButtonClick(Sender: TObject;
  AItem: TksInputListButtonItem; AID: string);
begin
  ShowMessage('Button clicked: '+AID);
end;

procedure TfrmKsInputList.ksInputList1ItemClick(Sender: TObject;
  AItem: TksBaseInputListItem; AID: string);
begin
  //AItem.BackgroundColor := claBlue;
end;

procedure TfrmKsInputList.ksInputList1ItemSwitchChanged(Sender: TObject;
  AItem: TksInputListSwitchItem; AID: string; AIsChecked: Boolean);
begin
  AItem.Title := (AID+'  '+BoolToStr(AIsChecked, True));
end;

procedure TfrmKsInputList.ksInputList1ItemTrackBarChange(Sender: TObject;
  AItem: TksInputListTrackBarItem; AID: string; AValue: Single);
begin
  Caption := FloatToStr(AValue);
  AItem.Title := AValue.ToString;
end;


procedure TfrmKsInputList.ksToolbar1Click(Sender: TObject);
begin
  ksInputList1.ShowOnScreenControls2;
end;

procedure TfrmKsInputList.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  frmMenu.ksSideMenu1.OpenMenu(Self);
end;

end.
