{*******************************************************************************
*                                                                              *
*  TksInputList                                                                *
*                                                                              *
*  https://github.com/gmurt/TksInputList                                    *
*                                                                              *
*  Copyright 2020 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License forthe specific language governing permissions and          *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksInputList;

interface

uses System.Classes, FMX.Controls, FMX.InertialMovement, System.Types,
  System.Generics.Collections, FMX.Graphics, System.UITypes, FMX.Layouts, FMX.Types,
  FMX.Objects, FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation, System.Threading,
  FMX.ListBox, Json, FMX.Pickers, System.UIConsts, FMX.TextLayout, FMX.Forms;

  {.$DEFINE DEBUG_BOXES}

const
  C_DEFAULT_SEPERATOR_HEIGHT = 44;

type
  TksInputList = class;
  TksBaseInputListItem = class;
  TksInputListSelectorItem = class;
  TksInputListEditItem = class;
  TksInputListSwitchItem = class;
  TksInputListCheckBoxItem = class;
  TksInputListButtonItem = class;
  TksInputListTrackBarItem = class;
  TksInputListDateTimeSelectorItem = class;

  TksInputAccessoryType = (atNone, atMore, atCheckmark, atDetail);

  TksInputListItemClickEvent = procedure(Sender: TObject; AItem: TksBaseInputListItem; AID: string) of object;
  TksInputListItemLongTapEvent = procedure(Sender: TObject; AItem: TksBaseInputListItem; AID: string) of object;
  TksInputListSelectorItemChangeEvent = procedure(Sender: TObject; AItem: TksInputListSelectorItem; AID, AValue: string) of object;
  TksInputListDateTimeSelectedEvent = procedure(Sender: TObject; AItem: TksInputListDateTimeSelectorItem; AID: string; ADateTime: TDateTime) of object;
  TksInputListEditTextChangeEvent = procedure(Sender: TObject; AItem: TksInputListEditItem; AID, AText: string) of object;
  TksInputListSwitchChangeEvent = procedure(Sender: TObject; AItem: TksInputListSwitchItem; AID: string; AIsChecked: Boolean) of object;
  TksInputListCheckBoxChangeEvent = procedure(Sender: TObject; AItem: TksInputListCheckBoxItem; AID: string; AIsChecked: Boolean) of object;
  TksInputListButtonClickEvent = procedure(Sender: TObject; AItem: TksInputListButtonItem; AID: string) of object;
  TksInputListTrackBarItemEvent = procedure(Sender: TObject; AItem: TksInputListTrackBarItem; AID: string; AValue: single) of object;

  TksBaseInputListItem = class
  private
    FksInputList: TksInputList;
    FEnabled: Boolean;
    FItemID: string;
    FFont: TFont;
    FItemRect: TRectF;
    FImageRect: TRectF;
    FContentRect: TRectF;
    FAccessoryRect: TRectF;
    FAccessory: TksInputAccessoryType;
    FBackground: TAlphaColor;

    FHeight: single;
    FIndex: integer;
    FTitle: string;

    FDetail: string;
    FImage: TBitmap;
    FMouseDown: Boolean;
    FOnChange: TNotifyEvent;
    FSelected: Boolean;
    FShowSelection: Boolean;
    FSelectedColor: TAlphaColor;
    FTagStr: string;
    FTagInt: integer;
    FReadOnly: Boolean;
    FTextColor: TAlphaColor;
    FDetailTextColor: TAlphaColor;
    function GetItemRect: TRectF;
    function GetAccessoryWidth(const AAddPadding: Boolean = False): single;
    procedure SetTitle(const Value: string);
    procedure SetHeight(const Value: Single);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetSelected(const Value: Boolean);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetAccessory(const Value: TksInputAccessoryType);
    procedure SetDetail(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure DrawDisabledRect(ACanvas: TCanvas);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetDetailTextColor(const Value: TAlphaColor);
  protected
    class function GetClassID: string; virtual; abstract;
    function GetHeight: Single; virtual;
    function GetDetailTextColor: TAlphaColor; virtual;
    function GetValue: string; virtual;
    procedure SetValue(const AValue: string); virtual;
    procedure SaveStructure(AJson: TJsonObject); virtual;
    procedure LoadStructure(AJson: TJSONObject); virtual;
    procedure UpdateRects; virtual;
    procedure MouseDown; virtual;
    procedure ItemClick; virtual;
    procedure MouseUp(ATapEvent: Boolean); virtual;
    procedure Changed; virtual;
    procedure Reset; virtual;
    procedure DoCustomDraw(ACanvas: TCanvas); virtual;
    property ItemRect: TRectF read GetItemRect;
    procedure SetReadOnly(const Value: Boolean); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AInputList: TksInputList); virtual;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas); virtual;

    procedure DrawSeparators(ACanvas: TCanvas; ATop, ABottom: Boolean);
    procedure SaveToJson(AJson: TJsonObject; AStructure, AData: Boolean);
    procedure LoadFromJson(AJson: TJsonObject; AStructure, AData: Boolean);
    property Accessory: TksInputAccessoryType read FAccessory write SetAccessory;
    property BackgroundColor: TAlphaColor read FBackground write SetBackgroundColor;
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property DetailTextColor: TAlphaColor read FDetailTextColor write SetDetailTextColor;
    property Image: TBitmap read FImage;
    property Height: Single read GetHeight write SetHeight;
    property Title: string read FTitle write SetTitle;
    property Detail: string read FDetail write SetDetail;
    property ClassID: string read GetClassID;
    property ID: string read FItemID write FItemID;
    property Value: string read GetValue write SetValue;
    property TagStr: string read FTagStr write FTagStr;
    property TagInt: integer read FTagInt write FTagInt;
    property Selected: Boolean read FSelected write SetSelected default False;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default False;
    property SelectedColor: TAlphaColor read FSelectedColor write SetSelectedColor;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

  TksInputListSeperator = class(TksBaseInputListItem)
  private
    FHorzAlign: TTextAlign;
    procedure SetHorzAlign(const Value: TTextAlign);

  protected
    class function GetClassID: string; override;
    function GetValue: string; override;
  public
    constructor Create(AInputList: TksInputList); override;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
    property HorzAlign: TTextAlign read FHorzAlign write SetHorzAlign default TTextAlign.Leading;
  end;

  TksInputListItem = class(TksBaseInputListItem)
  protected
    class function GetClassID: string; override;
    procedure UpdateRects; override;
  public
    procedure DrawToCanvas(ACanvas: TCanvas); override;
    property Accessory;
    property BackgroundColor;
    property ItemRect;
    property Selected;
    property ShowSelection;
  end;

  TksInputListBadgeItem = class(TksInputListItem)
  private
    FBadgeColor: TAlphaColor;
    FBadgeTextColor: TAlphaColor;
    procedure SetBadgeColor(const Value: TAlphaColor);
  protected
    class function GetClassID: string; override;
    function GetDetailTextColor: TAlphaColor; override;
  public
    constructor Create(AInputList: TksInputList); override;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
    property BadgeColor: TAlphaColor read FBadgeColor write SetBadgeColor default claDodgerBlue;
    property BadgeTextColor: TAlphaColor read FBadgeTextColor write FBadgeTextColor default claWhite;
  end;


  TksInputListChatItem = class(TksInputListItem)
  private
    FColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FSender: string;
    FBody: string;
    FDateTime: TDateTime;
    FCached: TBitmap;
    FAlignment: TTextAlign;
    FUse24HourTime: Boolean;
    function IsEmojiOnly: Boolean;
    procedure SetSender(const Value: string);
    procedure SetDateTime(const Value: TDateTime);
    procedure SetBody(const Value: string);
    function CalculateHeight: single;
    procedure SetAlignment(const Value: TTextAlign);
    procedure SetUse24HourTime(const Value: Boolean);
  protected
    class function GetClassID: string; override;
    procedure UpdateRects; override;
    function GetHeight: Single; override;
    procedure MouseDown; override;

  public
    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
    property Color: TAlphaColor read FColor write FColor default claDodgerBlue;
    property TextColor: TAlphaColor read FTextColor write FTextColor default claWhite;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property Sender: string read FSender write SetSender;
    property Body: string read FBody write SetBody;
    property Alignment: TTextAlign read FAlignment write SetAlignment default TTextAlign.Leading;
    property Use24HourTime: Boolean read FUse24HourTime write SetUse24HourTime default True;
  end;



  TksInputListItemWithControl = class(TksInputListItem)
  private
    FControl: TPresentedControl;
    FCached: TBitmap;
    FControlRect: TRectF;
    FFullWidthSelect: Boolean;
  protected
    procedure UpdateRects; override;
    function CreateControl: TPresentedControl; virtual; abstract;
    procedure PaintControl(ACanvas: TCanvas); virtual;
    procedure UpdateControlPosition;
    procedure ClickControl; virtual;
    procedure Changed; override;
    procedure Reset; override;
    procedure ItemClick; override;
  public
    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    procedure ClearCache;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
  end;

  // inherited to allow access to protected methods.
  TksEdit = class(TEdit);

  TksInputListEditItem = class(TksInputListItemWithControl)
  private
    function GetEdit: TksEdit;
    procedure TextChange(Sender: TObject);
  protected
    class function GetClassID: string; override;
    procedure SetReadOnly(const Value: Boolean); override;
    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    function CreateControl: TPresentedControl; override;
    procedure Reset; override;
    procedure ClickControl; override;
    procedure MouseDown; override;
  public
    property Edit: TksEdit read GetEdit;

  end;

  TksInputListSwitchItem = class(TksInputListItemWithControl)
  private
    function GetSwitch: TSwitch;
    procedure SwitchChange(Sender: TObject);
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    function CreateControl: TPresentedControl; override;
    class function GetClassID: string; override;
    procedure Reset; override;

  public
    constructor Create(AInputList: TksInputList); override;
    property Switch: TSwitch read GetSwitch;
  end;

  TksInputListCheckBoxItem = class(TksInputListItemWithControl)
  private
    FRadioGroupID: string;
    function GetCheckBox: TCheckBox;
    procedure CheckBoxChange(Sender: TObject);
    procedure SetRadioGroupID(const Value: string);
  protected
    function CreateControl: TPresentedControl; override;
    class function GetClassID: string; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    procedure ClickControl; override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;
    property CheckBox: TCheckBox read GetCheckBox;
    property RadioGroupID: string read FRadioGroupID write SetRadioGroupID;
  end;

  TksInputListButtonItem = class(TksInputListItemWithControl)
  private
    function GetButton: TButton;
    procedure DoButtonClick(Sender: TObject);
  protected
    function CreateControl: TPresentedControl; override;
    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    class function GetClassID: string; override;
    procedure ClickControl; override;
  public
    property Button: TButton read GetButton;
  end;

  // inherited to allow access to protected methods.
  TksTrackBar = class(TTrackBar);

  TksInputListTrackBarItem = class(TksInputListItemWithControl)
  private
    function GetTrackBar: TksTrackBar;
    procedure TrackBarChange(Sender: TObject);
  protected
    function CreateControl: TPresentedControl; override;
    class function GetClassID: string; override;
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;

    property TrackBar: TksTrackBar read GetTrackBar;
  end;

  TksInputListImageItem = class(TksInputListItem)
  protected
    class function GetClassID: string; override;
    procedure UpdateRects; override;
    procedure DoCustomDraw(ACanvas: TCanvas); override;
  public

    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas); override;
  end;

  TksInputListSelectorItem = class(TksBaseInputListItem)
  private
    FItems: TStrings;
    FCombo: TComboBox;
    FValue: string;
    procedure DoSelectorChanged(Sender: TObject);
    procedure SetItems(const Value: TStrings);
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    procedure MouseUp(ATapEvent: Boolean); override;
    class function GetClassID: string; override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    property Items: TStrings read FItems write SetItems;
    property Value: string read GetValue write SetValue;
  end;

  TksDateTimeSelectorKind = (ksDateSelector, ksTimeSelector);

  TksInputListDateTimeSelectorItem = class(TksBaseInputListItem)
  private
    FPickerService: IFMXPickerService;
    FDateTimePicker: TCustomDateTimePicker;
    FDateTime: TDateTime;
    FKind: TksDateTimeSelectorKind;
    function GetDateTime: TDateTime;
    procedure SetDateTime(const AValue: TDateTime);
    procedure DoSelectDateTime(Sender: TObject; const ADateTime: TDateTime);
    procedure SetKind(const Value: TksDateTimeSelectorKind);
  protected
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    procedure SaveStructure(AJson: TJsonObject); override;
    procedure LoadStructure(AJson: TJSONObject); override;
    procedure MouseUp(ATapEvent: Boolean); override;
    class function GetClassID: string; override;
    procedure Reset; override;
  public
    constructor Create(AInputList: TksInputList); override;
    destructor Destroy; override;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Kind: TksDateTimeSelectorKind read FKind write SetKind;
  end;

  TksInputListItems = class(TObjectList<TksBaseInputListItem>)
  private
    FksInputList: TksInputList;
    function GetItemByID(AID: string): TksBaseInputListItem;
    function GetCheckedCount: integer;
  public
    constructor Create(AForm: TksInputList); virtual;
    procedure ItemChange(Sender: TObject);
    procedure DrawToCanvas(ACanvas: TCanvas; AViewPort: TRectF);
    procedure DeselectAll;
    function AddSeperator(ATitle: string; const AHeight: integer = C_DEFAULT_SEPERATOR_HEIGHT): TksInputListSeperator;
    function AddItem(AID: string; AImg: TBitmap; ATitle: string;
                     const AAccessory: TksInputAccessoryType = atNone): TksInputListItem;
    procedure Insert(Index: Integer; const Value: TksBaseInputListItem); overload;
    function AddEditBoxItem(AID: string; AImg: TBitmap;
                            ATitle: string;
                            AValue: string;
                            APlaceholder: string;
                            const AKeyboard: TVirtualKeyboardType = TVirtualKeyboardType.Default): TksInputListEditItem;
    function AddSwitchItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean): TksInputListSwitchItem;
    function AddCheckBoxItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean): TksInputListCheckBoxItem;
    procedure AddButtonItem(AID: string; AImg: TBitmap; ATitle, AButtonTitle: string);
    procedure AddTrackBar(AID: string; AImg: TBitmap; ATitle: string; APos, AMax: integer);

    function AddImageItem(AID: string; AImg: TBitmap): TksInputListImageItem;
    function AddItemSelector(AID: string; AImg: TBitmap; ATitle, ASelected: string; AItems: array of string): TksInputListSelectorItem overload;
    function AddItemSelector(AID: string; AImg: TBitmap; ATitle, ASelected: string; AItems: TStrings): TksInputListSelectorItem overload;
    function AddDateSelector(AID: string; AImg: TBitmap; ATitle: string; ASelected: TDateTime): TksInputListDateTimeSelectorItem overload;
    function AddTimeSelector(AID: string; AImg: TBitmap; ATitle: string; ASelected: TDateTime): TksInputListDateTimeSelectorItem overload;
    function AddRadioItem(AID: string; AImage: TBitmap; AGroupID, ATitle: string; AChecked: Boolean): TksInputListCheckBoxItem;
    function AddBadgeItem(AID: string; AImage: TBitmap; ATitle, AValue: string; ABadgeColor, ABadgeTextColor: TAlphaColor; const AAccessory: TksInputAccessoryType = atNone): TksInputListBadgeItem;
    function AddChatItem(AID, ASender, AText: string; ADateTime: TDateTime; AAlign: TTextAlign; AColor, ATextColor: TAlphaColor; const AUse24HourTime: Boolean = True): TksInputListChatItem;
    function ItemExists(AID: string): Boolean;

    property CheckedCount: integer read GetCheckedCount;
    property ItemByID[AID: string]: TksBaseInputListItem read GetItemByID;

  end;

  TksInputListCanvas = class(TPaintBox)
  protected
    procedure Paint; override;

  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksInputList = class(TVertScrollBox)
  private
    FBuffer: TForm;
    FPickerService: IFMXPickerService;
    FCanvas: TksInputListCanvas;
    FItems: TksInputListItems;
    FLastScrollPos: single;
    FScrollMonitor: ITask;
    FControlsVisible: Boolean;
    FUpdateCount: integer;
    FLastScrollChange: TDateTime;
    FMouseDownTime: Cardinal;
    FMouseDownPos: TPointF;
    FMousePos: TPointF;
    FMouseDownItem: TksBaseInputListItem;
    FOnSelectorItemSelected: TksInputListSelectorItemChangeEvent;
    FOnDateTimeSelected: TksInputListDateTimeSelectedEvent;
    FOnEditItemTextChange: TksInputListEditTextChangeEvent;
    FOnItemClick: TksInputListItemClickEvent;
    FOnItemLongTap: TksInputListItemLongTapEvent;
    FOnSwitchChange: TksInputListSwitchChangeEvent;
    FOnCheckBoxChange: TksInputListCheckBoxChangeEvent;
    FOnItemButtonClick: TksInputListButtonClickEvent;
    FOnItemTrackBarChange: TksInputListTrackBarItemEvent;
    FItemHeight: single;
    FShowDividers: Boolean;
    FBackgroundColor: TAlphaColor;
    FMouseDown: Boolean;
    procedure UpdateItemRects;
    procedure RedrawItems;
    procedure CreateScrollMonitor;
    procedure ShowOnScreenControls;
    function GetIsScrolling: Boolean;
    procedure HidePickers;
    function GetValue(AName: string): string;
    procedure SetItemHeight(const Value: single);
    function GetAsJson(AStructure, AData: Boolean): string;
    procedure SetValue(AName: string; const Value: string);
    procedure SetShowDividers(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TAlphaColor);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
                                     const ContentSizeChanged: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HideAllControls;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure ClearItems;
    procedure Reset;
    procedure EnableAll;
    procedure DisableAll;
    procedure LoadFromJson(AJsonData: string; AStructure: Boolean; AData: Boolean); overload;
    procedure LoadFromJson(AJson: TJsonObject; AStructure: Boolean; AData: Boolean); overload;
    procedure SaveToJson(AJson: TJsonObject; AStructure: Boolean; AData: Boolean); overload;

    procedure ScrollToTop(const AAnimated: Boolean = False);
    procedure ScrollToBottom(const AAnimated: Boolean = False);
    property IsScrolling: Boolean read GetIsScrolling;
    property Items: TksInputListItems read FItems;
    property Value[AName: string]: string read GetValue write SetValue;
    property AsJson[AStructure, AData: Boolean]: string read GetAsJson;
  published
    property VScrollBar;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default claNull;
    property ShowDividers: Boolean read FShowDividers write SetShowDividers default True;
    property ItemHeight: single read FItemHeight write SetItemHeight;
    property OnItemClick: TksInputListItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemLongTapClick: TksInputListItemLongTapEvent read FOnItemLongTap write FOnItemLongTap;
    property OnSelectorItemSelected: TksInputListSelectorItemChangeEvent read FOnSelectorItemSelected write FOnSelectorItemSelected;
    property OnDateTimeSelected: TksInputListDateTimeSelectedEvent read FOnDateTimeSelected write FOnDateTimeSelected;
    property OnEditItemTextChange: TksInputListEditTextChangeEvent read FOnEditItemTextChange write FOnEditItemTextChange;
    property OnItemSwitchChanged: TksInputListSwitchChangeEvent read FOnSwitchChange write FOnSwitchChange;
    property OnItemCheckBoxChanged: TksInputListCheckBoxChangeEvent read FOnCheckBoxChange write FOnCheckBoxChange;
    property OnItemButtonClick: TksInputListButtonClickEvent read FOnItemButtonClick write FOnItemButtonClick;
    property OnItemTrackBarChange: TksInputListTrackBarItemEvent read FOnItemTrackBarChange write FOnItemTrackBarChange;
  end;

  procedure Register;


implementation

uses SysUtils, FMX.DialogService, DateUtils, FMX.Ani,
  Math, FMX.Styles, FMX.Styles.Objects, System.NetEncoding, FMX.Platform, System.Character;

const
  C_CORNER_RADIUS = 12;
  C_DEFAULT_ITEM_HEIGHT = 44;

  {$IFDEF MSWINDOWS}
  C_RIGHT_MARGIN = 20;
  {$ELSE}
  C_RIGHT_MARGIN = 8;
  {$ENDIF}

type
  TInputListAccessoryImage = class
  private
    FAccessoryType: TksInputAccessoryType;
    FBitmap: TBitmap;
  private
    constructor Create(AType: TksInputAccessoryType; ABmp: TBitmap); virtual;
    destructor Destroy; override;
    property AccessoryType: TksInputAccessoryType read FAccessoryType;
    property Bitmap: TBitmap read FBitmap;
  end;

  TInputListAccessoryImages = class(TObjectList<TInputListAccessoryImage>)
  private
    function GetItemByAccessory(AType: TksInputAccessoryType): TInputListAccessoryImage;
  public
    constructor Create; virtual;
    property ItemByAccessory[AType: TksInputAccessoryType]: TInputListAccessoryImage read GetItemByAccessory;
  end;

var
  //TempForm: TForm;
  AAccessoriesList: TInputListAccessoryImages;
  ATextLayout: TTextLayout;
  AScreenScale: single;

procedure Register;
begin
	RegisterComponents('Pentire FMX', [TksInputList]);
end;



function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  if AScreenScale > 0 then
  begin
    Result := AScreenScale;
    Exit;
  end
  else
  begin
    Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService));
    Result := Service.GetScreenScale;
    {$IFDEF IOS}
    if Result < 2 then
     Result := 2;
     AScreenScale := Result;
    {$ENDIF}
  end;
  {$IFDEF ANDROID}
  AScreenScale := Result;
  {$ENDIF}
end;

function AccessoryToStr(AAcc: TksInputAccessoryType): string;
begin
  Result := '';
  case AAcc of
    atMore: Result := 'more';
    atCheckmark: Result := 'checkmark';
    atDetail: Result := 'detail';
  end;
end;

function StrToAccessory(AAcc: string): TksInputAccessoryType;
begin
  AAcc := Trim(AAcc.ToLower);
  Result := atNone;
  if AAcc = 'more' then Result := atMore;
  if AAcc = 'checkmark' then Result := atCheckmark;
  if AAcc = 'detail' then Result := atDetail;
end;

function CreateListItem(AInputList: TksInputList; AClassID: string): TksBaseInputListItem;
begin
  Result := nil;
  if AClassID = TksInputListSeperator.GetClassID then Result := TksInputListSeperator.Create(AInputList);
  if AClassID = TksInputListItem.GetClassID then Result := TksInputListItem.Create(AInputList);
  if AClassID = TksInputListEditItem.GetClassID then Result := TksInputListEditItem.Create(AInputList);
  if AClassID = TksInputListSwitchItem.GetClassID then Result := TksInputListSwitchItem.Create(AInputList);
  if AClassID = TksInputListCheckBoxItem.GetClassID then Result := TksInputListCheckBoxItem.Create(AInputList);
  if AClassID = TksInputListButtonItem.GetClassID then Result := TksInputListButtonItem.Create(AInputList);
  if AClassID = TksInputListTrackBarItem.GetClassID then Result := TksInputListTrackBarItem.Create(AInputList);
  if AClassID = TksInputListSelectorItem.GetClassID then Result := TksInputListSelectorItem.Create(AInputList);
end;

function BmpToBase64(AImg: TBitmap): string;
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
begin
  AStream := TMemoryStream.Create;
  AEncoded := TStringStream.Create;
  try
    if AImg <> nil then
      AImg.SaveToStream(AStream);
    AStream.Position := 0;
    TNetEncoding.Base64.Encode(AStream, AEncoded);
    Result := AEncoded.DataString;
  finally
    AStream.Free;
    AEncoded.Free;
  end;
end;

procedure Base64ToBmp(AData: string; AImg: TBitmap);
var
  AStream: TMemoryStream;
  AEncoded: TStringStream;
begin
  AEncoded := TStringStream.Create(AData);
  AStream := TMemoryStream.Create;
  try
    AEncoded.Position := 0;
    TNetEncoding.Base64.Decode(AEncoded, AStream);
    AStream.Position := 0;
    AImg.LoadFromStream(AStream);
  finally
    AStream.Free;
    AEncoded.Free;
  end;
end;

function TryGetBoolFromString(AValue: string): Boolean;
var
  AStrings: TStrings;
begin
  AValue := AValue.ToLower;
  AStrings := TStringList.Create;
  try
    AStrings.CommaText := 'y,yes,t,true,1,on,checked';
    Result := AStrings.IndexOf(AValue) > -1;
  finally
    AStrings.Free;
  end;
end;

function GetAccessoryFromResource(AStyleName: array of string; const AState: string = ''): TBitmap;
var
  AStyleObj: TStyleObject;
  AImgRect: TBounds;
  r: TRectF;
  ABitmapLink: TBitmapLinks;
  AImageMap: TBitmap;
  I: integer;
  AScale: single;
  ICount: integer;
  AMap: TBitmap;
begin
  AMap := TBitmap.Create;
  Result := TBitmap.Create;
  AScale := GetScreenScale;
  AStyleObj := TStyleObject(TStyleManager.ActiveStyle(nil));

  for ICount := Low(AStyleName) to High(AStyleName) do
    AStyleObj := TStyleObject(AStyleObj.FindStyleResource(AStyleName[ICount]));

  if AStyleObj <> nil then
  begin
    if AMap.IsEmpty then
    begin
      for i := 0 to (AStyleObj as TStyleObject).Source.MultiResBitmap.Count-1 do
      begin
        AScale := (AStyleObj as TStyleObject).Source.MultiResBitmap[i].Scale;
        if Round(AScale) <= AScale then
        begin
          AScale := Round(AScale);
          Break;
        end;
      end;
      AImageMap := ((AStyleObj as TStyleObject).Source.MultiResBitmap.Bitmaps[AScale]);
      AMap.SetSize(Round(AImageMap.Width), Round(AImageMap.Height));
      AMap.Clear(claNull);

      AMap.Canvas.BeginScene;
      try
        AMap.Canvas.DrawBitmap(AImageMap,
                                    RectF(0, 0, AImageMap.Width, AImageMap.Height),
                                    RectF(0, 0, AMap.Width, AMap.Height),
                                    1,
                                    True);
      finally
        AMap.Canvas.EndScene;
      end;
    end;

    ABitmapLink := nil;
    if AStyleObj = nil then
      Exit;
    if (AStyleObj.ClassType = TCheckStyleObject) then
    begin
      if AState = 'checked' then
        ABitmapLink := TCheckStyleObject(AStyleObj).ActiveLink
      else
        ABitmapLink := TCheckStyleObject(AStyleObj).SourceLink
    end;

    if ABitmapLink = nil then
      ABitmapLink := AStyleObj.SourceLink;

    AImgRect := ABitmapLink.LinkByScale(AScale, True).SourceRect;


    Result.SetSize(Round(AImgRect.Width), Round(AImgRect.Height));
    Result.Clear(claNull);
    Result.Canvas.BeginScene;

    r := AImgRect.Rect;

    Result.Canvas.DrawBitmap(AMap,
                             r,
                             RectF(0, 0, Result.Width, Result.Height),
                             1,
                             True);
    Result.Canvas.EndScene;
  end;
  AMap.Free;
end;


{ TksInputList }

procedure TksInputList.BeginUpdate;
begin
  //inherited;
  Inc(FUpdateCount);
end;

procedure TksInputList.ClearItems;
begin
  BeginUpdate;
  try
    FItems.Clear;
  finally
    EndUpdate;
  end;
end;

constructor TksInputList.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TForm.CreateNew(nil);

  FPickerService := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService);

  FControlsVisible := False;
  FItemHeight := C_DEFAULT_ITEM_HEIGHT;
  FLastScrollPos := 0;
  FLastScrollChange := Now;
  FItems := TksInputListItems.Create(Self);
  FCanvas := TksInputListCanvas.Create(Self);
  FCanvas.Align := TAlignLayout.Top;
  FShowDividers := True;
  FCanvas.HitTest := False;
  FCanvas.Stored := False;
  AddObject(FCanvas);
  UpdateItemRects;
  FMouseDown := False;
  CreateScrollMonitor;
  AniCalculations.BoundsAnimation := True;
end;

procedure TksInputList.CreateScrollMonitor;
begin
  FScrollMonitor := TTask.Create (procedure ()
  begin
    while not Application.Terminated do
    begin
      sleep (100);
      if (FItems.Count > 0) and (MilliSecondsBetween(FLastScrollPos, Now) > 300) then
      begin
        if (FLastScrollPos = VScrollBarValue) and (FControlsVisible = False) then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowOnScreenControls;
            end);
        end

      end;
      FLastScrollPos := VScrollBarValue;

    end;
  end);
  FScrollMonitor.Start;
end;

destructor TksInputList.Destroy;
var
  AItem: TksBaseInputListItem;
  c: TPresentedControl;
begin
  for AItem in FItems do
  begin
    if AItem is TksInputListItemWithControl then
    begin
      c := (AItem as TksInputListItemWithControl).FControl;
      c.parent := nil;
    end;
  end;


  FItems.Free;
  FCanvas.Free;
  FBuffer.Free;
  inherited;
end;

procedure TksInputList.DisableAll;
var
  AItem: TksBaseInputListItem;
begin
  BeginUpdate;
  for AItem in FItems do
    AItem.Enabled := False;
  EndUpdate;
end;

procedure TksInputList.EnableAll;
var
  AItem: TksBaseInputListItem;
begin
  BeginUpdate;
  for AItem in FItems do
    AItem.Enabled := True;
  EndUpdate;
end;

procedure TksInputList.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    UpdateItemRects;
    {$IFNDEF MSWINDOWS}
    RedrawItems;
    {$ENDIF}
    //HideAllControls;
    HidePickers;
    ShowOnScreenControls;
    InvalidateRect(ClipRect);
  end;
end;

function TksInputList.GetAsJson(AStructure, AData: Boolean): string;
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.Create;
  try
    SaveToJson(AJson, AStructure, AData);
    Result := AJson.ToJSON;
  finally
    AJson.Free;
  end;
end;

function TksInputList.GetIsScrolling: Boolean;
begin
  Result := ((FMousePos.Y) < (FMouseDownPos.Y - 4)) or ((FMousePos.Y) > (FMouseDownPos.Y + 4));
end;

function TksInputList.GetValue(AName: string): string;
var
  AItem: TksBaseInputListItem;
begin
  Result := '';
  AItem := FItems.ItemByID[AName];
  if AItem <> nil then
  begin
    Result := AItem.Value;
  end;
end;

procedure TksInputList.HideAllControls;
var
  AItem: TksBaseInputListItem;
  c: TFMXObject;
begin
  inherited;
  if (FUpdateCount > 0) or (FControlsVisible = False) then
    Exit;

  for AItem in FItems do
  begin
    if AItem is TksInputListItemWithControl then
    begin
      c := (AItem as TksInputListItemWithControl).FControl;
      if c.Parent <> FBuffer then
      begin
        FBuffer.AddObject(c);
        (AItem as TksInputListItemWithControl).FControl.ApplyStyleLookup;
        //FBuffer.RemoveObject(c);
      end;
    end;
  end;
  FControlsVisible := False;
end;

procedure TksInputList.HidePickers;
begin
  if FPickerService <> nil then
    FPickerService.CloseAllPickers;
end;

procedure TksInputList.LoadFromJson(AJsonData: string; AStructure,
  AData: Boolean);
var
  AJson: TJSONObject;
begin
  AJson := TJSONObject.ParseJSONValue(AJsonData) as TJSONObject;
  try
    LoadFromJson(AJson, AStructure, AData);
  finally
    AJson.Free;
  end;
end;

procedure TksInputList.LoadFromJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
var
  AObj: TJSONObject;
  AArray: TJSONArray;
  ICount: integer;
  AItem: TksBaseInputListItem;
begin
  BeginUpdate;
  try
    if AStructure then
      ClearItems;
    AArray := (AJson.Values['items'] as TJSONArray);
    begin
      for ICount := 0 to AArray.Count-1 do
      begin
        AObj := AArray.Items[ICount] as TJSONObject;
        if AStructure then
        begin
          AItem := CreateListItem(Self, AObj.Values['class_id'].Value);
          FItems.Add(AItem);
        end
        else
          AItem := FItems.ItemByID[AObj.Values['id'].Value];
        if AItem <> nil then
          AItem.LoadFromJson(AObj, AStructure, AData);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TksInputList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  AItem: TksBaseInputListItem;
  ATask: ITask;
  ATapEvent: Boolean;
begin
  inherited;
  FMouseDown := True;
  HidePickers;
  y := y + VScrollBarValue;

  Root.SetFocused(nil);

  FMouseDownPos := PointF(X, Y);
  FMousePos := FMouseDownPos;
  FMouseDownItem := nil;

  for AItem in FItems do
  begin

    if PtInRect(AItem.ItemRect, PointF(x, y)) then
    begin
      if AItem.Enabled then
      begin
        FMouseDownItem := AItem;
        FMouseDownTime := MilliSecondOfTheDay(Now);
        aTask := TTask.Create (procedure ()
        begin
          Sleep(100);

          ATapEvent := ((FMousePos.Y) > (FMouseDownPos.Y - 10)) and ((FMousePos.Y) < (FMouseDownPos.Y + 10));
          TThread.Synchronize(nil,procedure
            var
              AItem: TksBaseInputListItem;
            begin
              if FMouseDown then
              begin
                for AItem in FItems do
                begin
                  if AItem.FMouseDown then
                    AItem.MouseUp(False);
                end;
                FMouseDownItem.MouseDown;

              end;
              Application.ProcessMessages;
            end
          );


        {aTask := TTask.Create (procedure ()
        begin
          Sleep(100);

          ATapEvent := ((FMousePos.Y) > (FMouseDownPos.Y - 10)) and ((FMousePos.Y) < (FMouseDownPos.Y + 10));
          TThread.Synchronize(nil,procedure
            begin
              if FMouseDown then
                FMouseDownItem.MouseDown;
            end
          );    }



          (*if ATapEvent then
          begin
            if FMouseDown then
            begin

            TThread.Synchronize(nil,procedure
                        begin
                          if FMouseDown then
                          begin
                            FMouseDownItem.MouseDown;
                          end
                          else
                          begin
                            FMouseDownItem.MouseDown;
                            {$IFDEF IOS}
                            Application.ProcessMessages;
                            Sleep(50);
                            {$ENDIF}
                            FMouseDownItem.Selected := False;
                          end;
                        end);
            Sleep(300);
            if FMouseDown then
            begin

              // long tap event...
              FLongTap := True;
              TThread.Synchronize(nil,procedure
                          begin
                            if Assigned(OnItemLongTapClick) then
                              OnItemLongTapClick(Self, FMouseDownItem, FMouseDownItem.ID);
                          end);
            end;
          end;
                 *)
        end);
        aTask.Start;
      end;

    end;
  end;
end;

procedure TksInputList.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  y := y + VScrollBarValue;
  FMousePos := PointF(X, Y);
end;

procedure TksInputList.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  ATapEvent: Boolean;
  ADelay: cardinal;
  AQuckTap: Boolean;
  AItem: TksBaseInputListItem;
  ALongTap: Boolean;
begin
  inherited;
  FMouseDown := False;
  y := y + VScrollBarValue;
  ADelay := MilliSecondOfTheDay(Now) - FMouseDownTime;
  AQuckTap := ADelay < 200;
  ALongTap := ADelay > 500;
  //form11.memo1.lines.add(FMouseDownPos.Y.ToString+'  '+y.ToString);

  ATapEvent := ((Y) > (FMouseDownPos.Y - 5)) and ((Y) < (FMouseDownPos.Y + 5));
  if FMouseDownItem <> nil then
  begin
    if FMouseDownItem.Enabled = False then
      Exit;

    //ADelay := (MilliSecondOfTheDay(Now) - FMouseDownTime);
    if ATapEvent then
    begin
      if AQuckTap then
      begin
        for AItem in FItems do
        begin
          if AItem.FMouseDown then
            AItem.MouseUp(False);
        end;

        FMouseDownItem.MouseDown;
        Application.ProcessMessages;
        Sleep(50);
      end;
      FMouseDownItem.MouseUp(ATapEvent);
      Application.ProcessMessages;

      if ALongTap then
      begin
        if Assigned(FOnItemLongTap) then
          FOnItemLongTap(Self, FMouseDownItem, FMouseDownItem.ID);
      end
      else
      begin
        FMouseDownItem.ItemClick;
        if Assigned(OnItemClick) then
          OnItemClick(Self, FMouseDownItem, FMouseDownItem.ID);
      end;
    end
    else
      FMouseDownItem.MouseUp(ATapEvent);
  end;
end;

procedure TksInputList.Paint;
begin
  if BackgroundColor <> claNull then
    Canvas.ClearRect(ClipRect, BackgroundColor);
  inherited;
end;

procedure TksInputList.UpdateItemRects;
var
  ICount: integer;
  AItem: TksBaseInputListItem;
  ATop: single;
  AHeight: single;
begin
  ATop := 0;
  AHeight := 0;
  for ICount := 0 to FItems.Count-1 do
  begin
    AItem := FItems[ICount];
    AItem.FItemRect := RectF(0, ATop, Width, ATop + AItem.Height);
    AItem.FIndex := ICount;
    ATop := AItem.FItemRect.Bottom;
    AHeight := AHeight + AItem.FItemRect.Height;
  end;
  FCanvas.Height := AHeight;
end;

procedure TksInputList.ViewportPositionChange(const OldViewportPosition,
  NewViewportPosition: TPointF; const ContentSizeChanged: boolean);
begin
  inherited;
  FLastScrollChange := Now;
  HideAllControls;
  HidePickers;
end;

procedure TksInputList.RedrawItems;
var
  r: TRectF;
begin
  r := ClipRect;
  OffsetRect(r, 0, VScrollBarValue);
  FItems.DrawToCanvas(FCanvas.Canvas, r);
end;

procedure TksInputList.Reset;
var
  AItem: TksBaseInputListItem;
begin
  BeginUpdate;
  try
    for AItem in FItems do
    begin
      AItem.Reset;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TksInputList.Resize;
begin
  inherited;
  UpdateItemRects;
  ShowOnScreenControls;
end;

procedure TksInputList.SaveToJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
var
  AArray: TJSONArray;
  AObj: TJSONObject;
  AItem: TksBaseInputListItem;
begin
  AArray := TJSONArray.Create;
  AJson.AddPair('items', AArray);
  for AItem in FItems do
  begin
    if (AStructure) or (not (AItem is TksInputListSeperator)) then
    begin
      AObj := TJSONObject.Create;
      AItem.SaveToJson(AObj, AStructure, AData);
      AArray.Add(AObj);
    end;
  end;
end;

procedure TksInputList.ScrollToBottom(const AAnimated: Boolean = False);
var
  ATime: Single;
begin
  {ATask := TTask.Create (procedure ()
  begin
    TThread.Synchronize(nil,procedure
    begin    }
      AniCalculations.UpdatePosImmediately(True);
      case AAnimated of
        True: ATime := 0.4;
        False: ATime := 0;
      end;

      if VScrollBar <> nil then
      begin
        //VScrollBar.Max
        case AAnimated of
          False: VScrollBar.Value := VScrollBar.Max;
          True: TAnimator.AnimateFloat(Self, 'VScrollBar.Value', {FCanvas.Height-Height} VScrollBar.Max, ATime, TAnimationType.InOut, TInterpolationType.Quadratic);
        end;

      end;
   { end);
  end);
  ATask.Start;   }
end;

procedure TksInputList.ScrollToTop(const AAnimated: Boolean = False);
var
  ATime: Single;
begin
  case AAnimated of
    True: ATime := 1;
    False: ATime := 0;
  end;
  if VScrollBar <> nil then
    TAnimator.AnimateFloat(Self, 'VScrollBar.Value', 0, ATime, TAnimationType.InOut, TInterpolationType.Quadratic);
end;

procedure TksInputList.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor <> Value then
  begin

    FBackgroundColor := Value;
    Repaint;
  end;
end;

procedure TksInputList.SetItemHeight(const Value: single);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    UpdateItemRects;
    ShowOnScreenControls;
  end;
end;

procedure TksInputList.SetShowDividers(const Value: Boolean);
begin
  if FShowDividers <> Value then
  begin
    FShowDividers := Value;
    RedrawItems;
  end;
end;

procedure TksInputList.SetValue(AName: string; const Value: string);
var
  AItem: TksBaseInputListItem;
begin;
  AItem := FItems.ItemByID[AName];
  if AItem <> nil then
  begin
    AItem.Value := Value;
  end;
end;

procedure TksInputList.ShowOnScreenControls;
var
  AItem: TksBaseInputListItem;
  ACtrlItem: TksInputListItemWithControl;
  r: TRectF;
begin

  if FUpdateCount > 0 then
    Exit;
  r := ContentRect;
  OffsetRect(r, 0, VScrollBarValue);

  for AItem in FItems do
  begin
    if (AItem is TksInputListItemWithControl) then
    begin
      ACtrlItem := (AItem as TksInputListItemWithControl);
      if ACtrlItem.Enabled then
      begin
        if IntersectRect(r, ACtrlItem.FItemRect) then
        begin
          if ACtrlItem.FControl.Parent = FBuffer then
          begin
            ACtrlItem.UpdateControlPosition;
            ACtrlItem.FControl.Visible := True;
            ACtrlItem.ClearCache;
            Self.AddObject(ACtrlItem.FControl);
          end;
        end;
      end;
    end;
  end;
  FControlsVisible := True;

end;

{ TksBaseInputListItem }

procedure TksBaseInputListItem.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


constructor TksBaseInputListItem.Create(AInputList: TksInputList);
begin
  inherited Create;
  FksInputList := AInputList;
  FImage := TBitmap.Create;
  FFont := TFont.Create;
  FHeight := FksInputList.ItemHeight;
  FItemID := '';
  FBackground := claWhite;
  FTextColor := claBlack;
  FDetailTextColor := claBlack;
  FSelected := False;
  FShowSelection := False;
  FReadOnly := False;
  FEnabled := True;
  FSelectedColor := $FFEAEAEA;
end;

destructor TksBaseInputListItem.Destroy;
begin
  FImage.Free;
  FFont.Free;
  inherited;
end;

procedure TksBaseInputListItem.DoCustomDraw(ACanvas: TCanvas);
begin
  //
end;


procedure TksBaseInputListItem.DrawDisabledRect(ACanvas: TCanvas);
begin
  ACanvas.Fill.Color := claWhite;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.FillRect(FItemRect, 0, 0, AllCorners, 0.5);
end;

procedure TksBaseInputListItem.DrawSeparators(ACanvas: TCanvas; ATop, ABottom: Boolean);
var
  r: TRectF;
begin
  ACanvas.Stroke.Color := claGray;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := 0.5;
  r := FItemRect;

  InflateRect(r, 0, (ACanvas.Stroke.Thickness / GetScreenScale()));

  if ATop then ACanvas.DrawLine(r.TopLeft, PointF(r.Right, r.Top), 1);
  if ABottom then  ACanvas.DrawLine(PointF(r.Left, r.Bottom), r.BottomRight, 1);
end;

procedure TksBaseInputListItem.DrawToCanvas(ACanvas: TCanvas);
var
  AState: TCanvasSaveState;
  AAccRect: TRectF;
  AAcc: TBitmap;
  r: TRectF;
begin
  UpdateRects;
  AState := ACanvas.SaveState;
  try
    ACanvas.IntersectClipRect(FItemRect);
    ACanvas.Fill.Color := FBackground;
    ACanvas.Font.Assign(FFont);

    if ((FSelected) and (FShowSelection)) and (not FReadOnly) and (FEnabled) then
      ACanvas.Fill.Color := FSelectedColor;

    r := FItemRect;
    InflateRect(r, 0, 0.5);

    ACanvas.FillRect(r, 0, 0, AllCorners, 1);

    {$IFDEF DEBUG_BOXES}
    ACanvas.Stroke.Color := claRed;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Thickness := GetScreenScale;
    ACanvas.DrawRect(FContentRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);
    ACanvas.Stroke.Color := claGreen;

    ACanvas.DrawRect(FContentRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);

    ACanvas.Stroke.Color := claBlue;
    ACanvas.DrawRect(FAccessoryRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);

    ACanvas.Stroke.Color := claPink;
    ACanvas.DrawRect(FImageRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);
    {$ENDIF}

    if (FAccessory <> atNone) and (FReadOnly = False) then
    begin
      AAcc := AAccessoriesList.ItemByAccessory[FAccessory].Bitmap;
      if AAcc <> nil then
      begin
        AAccRect := RectF(0, 0, AAcc.Width, AAcc.Height);

        ACanvas.DrawBitmap(AAcc,
                           AAccRect,
                           RectF(0, 0, AAccRect.Width/GetScreenScale, AAccRect.Height/GetScreenScale).PlaceInto(FAccessoryRect),
                           1,
                           True);

        {$IFDEF DEBUG_BOXES}
        ACanvas.Stroke.Color := claBlack;
        ACanvas.Stroke.Kind := TBrushKind.Solid;
        ACanvas.Stroke.Thickness := 1;
        ACanvas.DrawRect(FAccessoryRect, 0, 0, AllCorners, 1);
        {$ENDIF}
      end;
    end;

    if FImage.IsEmpty = False then
    begin
      FImageRect.Inflate(-4, -4);
      ACanvas.DrawBitmap(FImage, FImage.BoundsF, FImage.BoundsF.PlaceInto(FImageRect),1, True);
    end;

    ACanvas.Fill.Color := FTextColor;
    if Trim(FTitle) <> '' then
    begin
      ACanvas.FillText(FContentRect, FTitle, False, 1, [], TTextAlign.Leading, TTextAlign.Center);
    end;
    if Trim(FDetail) <> '' then
    begin
      ACanvas.Fill.Color := GetDetailTextColor;
      ACanvas.FillText(FContentRect, FDetail, False, 1, [], TTextAlign.Trailing, TTextAlign.Center);
    end;
    DoCustomDraw(ACanvas);
  finally
    ACanvas.RestoreState(AState);
  end;
end;

function TksBaseInputListItem.GetAccessoryWidth(const AAddPadding: Boolean = False): single;
begin
  Result := 0;
  if FAccessory <> atNone then
  begin
    //Result := AAccessoriesList.ItemByAccessory[FAccessory].Bitmap.Width;
    //{$IFDEF ANDROID}
    Result := 20;
    //{$ENDIF}
    if AAddPadding then
      Result := Result + 4;
  end;
end;

function TksBaseInputListItem.GetDetailTextColor: TAlphaColor;
begin
  Result := FDetailTextColor;
end;

function TksBaseInputListItem.GetHeight: Single;
begin
  Result := FHeight;
end;

function TksBaseInputListItem.GetItemRect: TRectF;
begin
  Result := FItemRect;
end;

function TksBaseInputListItem.GetValue: string;
begin
  Result := '';
end;

procedure TksBaseInputListItem.ItemClick;
begin
  //
end;

procedure TksBaseInputListItem.LoadFromJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
begin
  if AStructure then  LoadStructure(AJson);
  if AData then Value := AJson.Values['value'].Value;
  
end;

procedure TksBaseInputListItem.LoadStructure(AJson: TJSONObject);
begin
  if AJson.Values['image'] <> nil then
    Base64ToBmp(AJson.Values['image'].Value, FImage);
  FItemID := AJson.Values['id'].Value;
  FAccessory := StrToAccessory(AJson.Values['acc'].Value);
  FBackground := StringToAlphaColor(AJson.Values['background'].Value);
  FHeight := StrToFloat(AJson.Values['height'].Value);
  FIndex := StrToInt(AJson.Values['index'].Value);
  FTitle := AJson.Values['title'].Value;
  FDetail := AJson.Values['detail'].Value;

  FShowSelection := StrToBool(AJson.Values['show_selection'].Value);
end;

procedure TksBaseInputListItem.MouseDown;
begin
  if not FEnabled then
    Exit;
  if FksInputList.IsScrolling then
    Exit;
  FMouseDown := True;
  if FShowSelection then
    Selected := True;
end;

procedure TksBaseInputListItem.MouseUp(ATapEvent: Boolean);
begin
  if FEnabled = False then
    Exit;
  if FMouseDown then
  begin

    FMouseDown := False;
    if (FSelected) then
      Selected := False;
  end;
end;

procedure TksBaseInputListItem.Reset;
begin
  //
end;



procedure TksBaseInputListItem.SaveStructure(AJson: TJsonObject);
begin
  AJson.AddPair('class_id', GetClassID);
  if not FImage.IsEmpty then
    AJson.AddPair('image', BmpToBase64(FImage));
  AJson.AddPair('acc', AccessoryToStr(FAccessory));
  AJson.AddPair('background', AlphaColorToString(FBackground));
  AJson.AddPair('height', FloatToStr(FHeight));
  AJson.AddPair('index', IntToStr(FIndex));
  AJson.AddPair('title', FTitle);
  AJson.AddPair('detail', FDetail);
  AJson.AddPair('show_selection', BoolToStr(FShowSelection, True));
end;

procedure TksBaseInputListItem.SaveToJson(AJson: TJsonObject; AStructure,
  AData: Boolean);
begin
  AJson.AddPair('id', FItemID);
  if AStructure then
    SaveStructure(AJson);
  if AData then
    AJson.AddPair('value', Value);
end;

procedure TksBaseInputListItem.SetAccessory(const Value: TksInputAccessoryType);
begin
  if FAccessory <> Value then
  begin
    FAccessory := Value;
    if FAccessory = atMore then
      FShowSelection := True;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetBackgroundColor(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetDetail(const Value: string);
begin
  if FDetail <> Value then
  begin
    FDetail := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetDetailTextColor(const Value: TAlphaColor);
begin
  if FDetailTextColor <> Value then
  begin
    FDetailTextColor := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetHeight(const Value: Single);
begin
  FHeight := Value;
  Changed;
end;

procedure TksBaseInputListItem.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    if Value then
      FShowSelection := False;
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetSelectedColor(const Value: TAlphaColor);
begin
  if FSelectedColor <> Value then
  begin
    FSelectedColor := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetShowSelection(const Value: Boolean);
begin
  FShowSelection := Value;
end;

procedure TksBaseInputListItem.SetTextColor(const Value: TAlphaColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TksBaseInputListItem.SetTitle(const Value: string);
begin
  FTitle := Value;
  Changed;
end;

procedure TksBaseInputListItem.SetValue(const AValue: string);
begin
  // overridden in descendant classes.
end;

procedure TksBaseInputListItem.UpdateRects;
var
  AAccessoryWidth: single;
begin
  FAccessoryRect := FAccessoryRect.Empty;
  FImageRect := FImageRect.Empty;

  FContentRect := FItemRect;
  FContentRect.Left := FContentRect.Left + 10;
  FContentRect.Right := FContentRect.Right - C_RIGHT_MARGIN;

  // add image rect...
  if not FImage.IsEmpty then
  begin
    FImageRect := FContentRect;
    FImageRect.Right := FContentRect.Left+36;
    FContentRect.Left := FImageRect.Right+8;
  end;

  if (FAccessory <> atNone) and (not ((FAccessory = atMore) and (FReadOnly))) then
  begin
    // add accessory rect
    AAccessoryWidth := GetAccessoryWidth(False);
    FAccessoryRect := FContentRect;
    FAccessoryRect.Left := FContentRect.Right-AAccessoryWidth;
    FContentRect.Right := FAccessoryRect.Left;
  end
  {$IFNDEF MSWINDOWS}
  else
    FContentRect.Right := FContentRect.Right - 6;
  {$ENDIF}
end;

{ TksInputListItems }

function TksInputListItems.AddBadgeItem(AID: string; AImage: TBitmap; ATitle, AValue: string;
  ABadgeColor, ABadgeTextColor: TAlphaColor; const AAccessory: TksInputAccessoryType = atNone): TksInputListBadgeItem;
begin
  Result := TksInputListBadgeItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.FImage.Assign(AImage);
  Result.Title := ATitle;
  Result.Detail := AValue;
  Result.BadgeColor := ABadgeColor;
  Result.BadgeTextColor := ABadgeTextColor;
  Result.Accessory := AAccessory;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

procedure TksInputListItems.AddButtonItem(AID: string; AImg: TBitmap; ATitle,
  AButtonTitle: string);
var
  AItem: TksInputListButtonItem;
begin
  AItem := TksInputListButtonItem.Create(FksInputList);
  AItem.FItemID := AID;
  AItem.FImage.Assign(AImg);
  AItem.Title := ATitle;
  AItem.Button.Text := AButtonTitle;
  AItem.Button.DisableFocusEffect := True;
  AItem.Button.CanFocus := False;
  AItem.OnChange := ItemChange;
  Add(AItem);
  ItemChange(Self);
end;

function TksInputListItems.AddChatItem(AID, ASender, AText: string; ADateTime: TDateTime; AAlign: TTextAlign;
  AColor, ATextColor: TAlphaColor; const AUse24HourTime: Boolean = True): TksInputListChatItem;
var
  AStr: string;
  ATone: integer;
begin
  Result := TksInputListChatItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.Color := AColor;

  AStr := AText;

  for ATone := 57339 to 57344 do
    if Pos(Chr(ATone), AStr) > 0 then System.Delete(AStr, Pos(Chr(ATone), AStr)-1, 2); //StringReplace(s, Chr(57339), '', [rfReplaceAll]);


  if Pos(Chr(8205), AStr) > 0 then System.Delete(AStr, Pos(Chr(8205), AStr), 3); //StringReplace(s, Chr(57339), '', [rfReplaceAll]);



  AStr := StringReplace(AStr, '‍♂️', '', [rfReplaceAll]);
  AStr := StringReplace(AStr, '♀️', '', [rfReplaceAll]);

  AStr := Trim(AStr);
  Result.TextColor := ATextColor;
  Result.Body := AStr;
  Result.DateTime := ADateTime;
  Result.Sender := ASender;
  Result.Alignment := AAlign;
  Result.Use24HourTime := AUse24HourTime;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddCheckBoxItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean): TksInputListCheckBoxItem;
begin
  Result := TksInputListCheckBoxItem.Create(FksInputList);
  Result.FImage.Assign(AImg);
  Result.FItemID := AID;
  Result.Title := ATitle;
  Result.CheckBox.IsChecked := AState;
  Result.CheckBox.OnChange := Result.CheckBoxChange;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddEditBoxItem(AID: string;
                                          AImg: TBitmap;
                                          ATitle: string;
                                          AValue: string;
                                          APlaceholder: string;
                                          const AKeyboard: TVirtualKeyboardType = TVirtualKeyboardType.Default): TksInputListEditItem;
begin
  Result := TksInputListEditItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.Edit.KeyboardType := AKeyboard;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.Edit.TextPrompt := APlaceholder;
  Result.Edit.Text := AValue;
  Result.OnChange := ItemChange;
  Result.Edit.OnChangeTracking := Result.TextChange;
  Result.Edit.ApplyStyleLookup;

  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddSeperator(ATitle: string; const AHeight: integer = C_DEFAULT_SEPERATOR_HEIGHT): TksInputListSeperator;
begin
  Result := TksInputListSeperator.Create(FksInputList);
  Result.Height := AHeight;
  Result.FItemID := GuidToString(TGUID.NewGuid);
  Result.Title := ATitle;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddSwitchItem(AID: string; AImg: TBitmap; ATitle: string; AState: Boolean): TksInputListSwitchItem;
begin
  Result := TksInputListSwitchItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.Switch.IsChecked := AState;
  Result.Switch.OnSwitch := Result.SwitchChange;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

procedure TksInputListItems.AddTrackBar(AID: string; AImg: TBitmap; ATitle: string; APos,
  AMax: integer);
var
  AItem: TksInputListTrackBarItem;
begin
  AItem := TksInputListTrackBarItem.Create(FksInputList);
  AItem.FItemID := AID;
  AItem.FImage.Assign(AImg);
  AItem.Title := ATitle;
  AItem.TrackBar.Max := AMax;
  AItem.TrackBar.Value := AMax;
  AItem.OnChange := ItemChange;
  AItem.TrackBar.OnTracking := AItem.TrackBarChange;

  Add(AItem);
  ItemChange(Self);
end;

function TksInputListItems.AddImageItem(AID: string; AImg: TBitmap): TksInputListImageItem;
begin
  Result := TksInputListImageItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.FImage.Assign(AImg);

  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddItem(AID: string; AImg: TBitmap; ATitle: string;
  const AAccessory: TksInputAccessoryType = atNone): TksInputListItem;
begin


  Result := TksInputListItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  if AAccessory <> atNone then
    Result.ShowSelection := True;
  Result.Accessory := AAccessory;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;



function TksInputListItems.AddItemSelector(AID: string; AImg: TBitmap; ATitle,
  ASelected: string; AItems: TStrings): TksInputListSelectorItem;
begin
  Result := TksInputListSelectorItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.Items.Assign(AItems);
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.Accessory := atMore;
  Result.Value := ASelected;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddDateSelector(AID: string; AImg: TBitmap; ATitle: string; ASelected: TDateTime): TksInputListDateTimeSelectorItem overload;
begin
  Result := TksInputListDateTimeSelectorItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.DateTime := ASelected;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.Accessory := atMore;
  Result.OnChange := ItemChange;
  Result.Kind := ksDateSelector;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddTimeSelector(AID: string; AImg: TBitmap; ATitle: string; ASelected: TDateTime): TksInputListDateTimeSelectorItem overload;
begin
  Result := TksInputListDateTimeSelectorItem.Create(FksInputList);
  Result.FItemID := AID;
  Result.Kind := ksTimeSelector;

  Result.DateTime := ASelected;
  Result.FImage.Assign(AImg);
  Result.Title := ATitle;
  Result.Accessory := atMore;
  Result.OnChange := ItemChange;
  Add(Result);
  ItemChange(Self);
end;

function TksInputListItems.AddRadioItem(AID: string; AImage: TBitmap; AGroupID, ATitle: string; AChecked: Boolean): TksInputListCheckBoxItem;
begin
  Result := AddCheckBoxItem(AID, AImage, ATitle, AChecked);
  Result.RadioGroupID := AGroupID;
end;

function TksInputListItems.AddItemSelector(AID: string; AImg: TBitmap; ATitle,
  ASelected: string; AItems: array of string): TksInputListSelectorItem;
var
  AStrings: TStrings;
  s: string;
begin
  AStrings := TStringList.Create;
  try
    for s in AItems do
      AStrings.Add(s);
    Result := AddItemSelector(AId, AImg, ATitle, ASelected, AStrings);
  finally
    AStrings.Free;
  end;
end;

constructor TksInputListItems.Create(AForm: TksInputList);
begin
  inherited Create(True);
  FksInputList := AForm;
end;

procedure TksInputListItems.DeselectAll;
var
  AItem: TksBaseInputListItem;
begin
  for AItem in Self do
    AItem.Selected := False;
end;

procedure TksInputListItems.DrawToCanvas(ACanvas: TCanvas; AViewPort: TRectF);
var
  AItem: TksBaseInputListItem;
  ICount: integer;
begin
  for AItem in Self do
  begin
    if IntersectRect(AViewPort, AItem.FItemRect) then
      AItem.DrawToCanvas(ACanvas);
    if AItem.Enabled = False then
      AItem.DrawDisabledRect(ACanvas);
  end;

  if FksInputList.ShowDividers then
  begin
    for ICount := 0 to Count-1 do
    begin
      AItem := Items[ICount];
      if IntersectRect(AViewPort, AItem.FItemRect) then
        //if (not ((AItem is TksInputListSeperator) and (ICount = Count-1))) and
        if (not ((AItem is TksInputListSeperator) and (ICount = 0))) then
          AItem.DrawSeparators(ACanvas,
                               True,
                               (ICount = Count-1) and (not(AItem is TksInputListSeperator)));
    end;
  end;

end;

function TksInputListItems.GetCheckedCount: integer;
var
  AItem: TksBaseInputListItem;
begin
  Result := 0;
  for AItem in Self do
  begin
    if (AItem is TksInputListCheckBoxItem) then
      if (AItem as TksInputListCheckBoxItem).CheckBox.IsChecked then
      Result := Result +1;
  end;
end;

function TksInputListItems.GetItemByID(AID: string): TksBaseInputListItem;
var
  AItem: TksBaseInputListItem;
begin
  Result := nil;
  for AItem in Self do
  begin
    if UpperCase(AItem.ID) = UpperCase(AID)
     then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

procedure TksInputListItems.Insert(Index: Integer; const Value: TksBaseInputListItem);
begin
  Value.OnChange := ItemChange;
  inherited Insert(Index, Value);
  ItemChange(Self);
end;

procedure TksInputListItems.ItemChange(Sender: TObject);
begin
  if FksInputList.FUpdateCount > 0 then
    Exit;
  FksInputList.UpdateItemRects;
  FksInputList.InvalidateRect(FksInputList.ClipRect);
  FksInputList.ShowOnScreenControls;
end;

function TksInputListItems.ItemExists(AID: string): Boolean;
begin
  Result := ItemByID[AID] <> nil;
end;

{ TksInputListCanvas }

procedure TksInputListCanvas.Paint;
begin
  inherited;
  if (Owner as TksInputList).BackgroundColor <> claNull then
    Canvas.Clear((Owner as TksInputList).BackgroundColor);
  (Owner as TksInputList).RedrawItems;
end;

{ TksInputListEditItem }


procedure TksInputListEditItem.ClickControl;
begin
  inherited ClickControl;
end;


function TksInputListEditItem.CreateControl: TPresentedControl;
begin
  Result := TksEdit.Create(nil);
  (Result as TksEdit).ApplyStyle;
  (Result as TksEdit).Width := FksInputList.Width / 2;
  (Result as TksEdit).TextSettings.HorzAlign := TTextAlign.Trailing;
  (Result as TksEdit).CanFocus := True;
  (Result as TksEdit).DisableFocusEffect := False;
end;

class function TksInputListEditItem.GetClassID: string;
begin
  Result := '{099048A9-EC3B-4931-89DF-61C938B3F571}';
end;

function TksInputListEditItem.GetEdit: TksEdit;
begin
  Result := (FControl as TksEdit);
end;

function TksInputListEditItem.GetValue: string;
begin
  Result := Edit.Text;
end;

procedure TksInputListEditItem.MouseDown;
begin
  inherited;

end;

procedure TksInputListEditItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;
  Edit.KeyboardType := TVirtualKeyboardType(StrToIntDef(AJson.Values['keyboard'].Value, 0));
end;

procedure TksInputListEditItem.Reset;
begin
  inherited;
  Edit.Text := '';
end;

procedure TksInputListEditItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('keyboard', IntToStr(Ord(Edit.KeyboardType)));

end;

procedure TksInputListEditItem.SetReadOnly(const Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    //case Value of
    //  True: Edit.StyleLookup := '';
    //  False: Edit.StyleLookup := 'clearingeditstyle';
    //end;
    Edit.CanFocus := not ReadOnly;
    Edit.HitTest := not ReadOnly;
    Edit.ReadOnly := ReadOnly;
    Changed;
  end;
end;

procedure TksInputListEditItem.SetValue(const AValue: string);
begin
  Edit.Text := AValue;
end;

procedure TksInputListEditItem.TextChange(Sender: TObject);
begin
  if Assigned(FksInputList.OnEditItemTextChange) then
    FksInputList.OnEditItemTextChange(FksInputList, Self, ID, Value);
end;

{ TksInputListItemWithControl }

procedure TksInputListItemWithControl.Changed;
begin
  inherited;
  UpdateRects;
  UpdateControlPosition;
end;

procedure TksInputListItemWithControl.ClearCache;
begin
  FCached.SetSize(0,0);
end;

procedure TksInputListItemWithControl.ClickControl;
begin
  //FksInputList.HidePickers;
end;

constructor TksInputListItemWithControl.Create(AInputList: TksInputList);
begin
  inherited;
  FControl := CreateControl;
  FControl.Visible := True;
  //FControl.Parent := TempForm;
  FControl.ApplyStyleLookup;
  FCached := TBitmap.Create;
  FFullWidthSelect := False;
end;


destructor TksInputListItemWithControl.Destroy;
begin
  FControl.Free;
  FCached.Free;
  inherited;
end;

procedure TksInputListItemWithControl.DrawToCanvas(ACanvas: TCanvas);
var
  AScreenScale: single;
begin
  inherited;
  {$IFDEF DEBUG_BOXES}
  ACanvas.Stroke.Color := claBlack;
  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := GetScreenScale;
  ACanvas.DrawRect(FControl.BoundsRect, C_CORNER_RADIUS, C_CORNER_RADIUS, AllCorners, 1);
  {$ENDIF}

  if FControl.Parent <> FksInputList.FBuffer then
    Exit;

  AScreenScale := GetScreenScale;

  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Stroke.Thickness := 0.5;

  if FCached.IsEmpty then
  begin
    FCached.BitmapScale := AScreenScale;
    FCached.SetSize(Round(FControl.Width*AScreenScale), Round(FControl.Height*AScreenScale));
    FCached.Canvas.BeginScene;
    FCached.Clear(claNull);
    PaintControl(FCached.Canvas);
    FCached.Canvas.EndScene;
  end;
  ACanvas.DrawBitmap(FCached, Rect(0, 0, FCached.Width, FCached.Height), FControl.BoundsRect.PlaceInto(FControlRect, THorzRectAlign.Right), 1);
end;

procedure TksInputListItemWithControl.ItemClick;
begin
  if (FFullWidthSelect) and (not ReadOnly) and (FEnabled) then
    ClickControl;
end;

procedure TksInputListItemWithControl.PaintControl(ACanvas: TCanvas);
begin
  FControl.PaintTo(ACanvas, RectF(0, 0, FControl.Width, FControl.Height));
end;


procedure TksInputListItemWithControl.Reset;
begin
  inherited;
  FCached.SetSize(0, 0);
end;

procedure TksInputListItemWithControl.UpdateControlPosition;
begin
  UpdateRects;
  FControl.BoundsRect := FControl.BoundsRect.PlaceInto(FControlRect, THorzRectAlign.Right);
end;

procedure TksInputListItemWithControl.UpdateRects;
begin
  inherited;
  FControlRect := FContentRect;
  FControlRect.Left := FControlRect.Right - FControl.Width;
  FContentRect.Right := FControlRect.Left;
end;

{ TksInputListSwitchItem }

constructor TksInputListSwitchItem.Create(AInputList: TksInputList);
begin
  inherited;

end;

function TksInputListSwitchItem.CreateControl: TPresentedControl;
begin
  Result := TSwitch.Create(nil);
end;

class function TksInputListSwitchItem.GetClassID: string;
begin
  Result := '{C9533F62-6097-4AB2-B353-C54F83B29EEF}';
end;

function TksInputListSwitchItem.GetSwitch: TSwitch;
begin
  Result := FControl as TSwitch;
end;

function TksInputListSwitchItem.GetValue: string;
begin
  Result := BoolToStr(Switch.IsChecked, True);
end;

procedure TksInputListSwitchItem.Reset;
begin
  inherited;
  Switch.IsChecked := False;
end;

procedure TksInputListSwitchItem.SetValue(const AValue: string);
begin
  inherited;
  Switch.IsChecked := TryGetBoolFromString(AValue);
end;

procedure TksInputListSwitchItem.SwitchChange(Sender: TObject);
var
  ATask: ITask;
begin
  ATask := TTask.Create (procedure ()
   begin
     TThread.Synchronize(nil,procedure
      begin
        if Assigned(FksInputList.OnItemSwitchChanged) then
        begin
          FksInputList.OnItemSwitchChanged(FksInputList, Self, ID, Switch.IsChecked);
        end;
      end);
   end);
  ATask.Start;
end;

{ TksInputListCheckBoxItem }

procedure TksInputListCheckBoxItem.CheckBoxChange(Sender: TObject);
begin
  if Assigned(FksInputList.OnItemCheckBoxChanged) then
  begin
    FksInputList.OnItemCheckBoxChanged(FksInputList, Self, ID, CheckBox.IsChecked);
  end;
end;

procedure TksInputListCheckBoxItem.ClickControl;
var
  AItem: TksBaseInputListItem;
  cb: TksInputListCheckBoxItem;
begin
  inherited;
  if FRadioGroupID <> '' then
  begin
    for AItem in FksInputList.Items do
    begin
      if AItem is TksInputListCheckBoxItem then
      begin
        cb := (AItem as TksInputListCheckBoxItem);
        cb.CheckBox.IsChecked := False;
      end;
    end;
  end;
  if (CheckBox.IsChecked) and (FRadioGroupID <> '') then
    Exit;
  CheckBox.IsChecked := not CheckBox.IsChecked;

end;


constructor TksInputListCheckBoxItem.Create(AInputList: TksInputList);
begin
  inherited;
  FShowSelection := True;
  FFullWidthSelect := True;
end;

function TksInputListCheckBoxItem.CreateControl: TPresentedControl;
begin
  Result := TCheckBox.Create(nil);
end;

function TksInputListCheckBoxItem.GetCheckBox: TCheckBox;
begin
  Result := (FControl as TCheckBox);
  (Result as TCheckBox).Width := 24;
  (Result as TCheckBox).Height := 24;
end;

class function TksInputListCheckBoxItem.GetClassID: string;
begin
  Result := '{07EC5B74-F220-45DD-BDC0-D4D686058C7C}';
end;

function TksInputListCheckBoxItem.GetValue: string;
begin
  Result := BoolToStr(CheckBox.IsChecked, True);
end;

procedure TksInputListCheckBoxItem.Reset;
begin
  inherited;
  CheckBox.IsChecked := False;
end;

procedure TksInputListCheckBoxItem.SetRadioGroupID(const Value: string);
begin
  if FRadioGroupID <> Value then
  begin
    FRadioGroupID := Value;
  end;
end;

procedure TksInputListCheckBoxItem.SetValue(const AValue: string);
begin
  inherited;
  CheckBox.IsChecked := TryGetBoolFromString(AValue);
end;

{ TksInputListButtonItem }

procedure TksInputListButtonItem.ClickControl;
begin
  inherited;
  (FControl as TButton).IsPressed := True;
end;



function TksInputListButtonItem.CreateControl: TPresentedControl;
begin
  Result := TButton.Create(nil);
  (Result as TButton).StyleLookup := 'listitembutton';
  (Result as TButton).Height := 32;
  (Result as TButton).OnClick := DoButtonClick;
end;

procedure TksInputListButtonItem.DoButtonClick(Sender: TObject);
begin
  if Assigned(FksInputList.OnItemButtonClick) then
    FksInputList.OnItemButtonClick(FksInputList, Self, ID);
end;

function TksInputListButtonItem.GetButton: TButton;
begin
  Result := (FControl as TButton);
end;

class function TksInputListButtonItem.GetClassID: string;
begin
  Result := '{D7E87C25-E018-41F0-B85D-AA6B690C36CB}';
end;

procedure TksInputListButtonItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;
  Button.Text := AJson.Values['button_text'].Value;
  Button.Width := StrToFloat(AJson.Values['button_width'].Value);
end;

procedure TksInputListButtonItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('button_text', Button.Text);
  AJson.AddPair('button_width', FloatToStr(Button.Width));
end;

{ TksInputListTrackBarItem }

constructor TksInputListTrackBarItem.Create(AInputList: TksInputList);
begin
  inherited;
  TrackBar.Width := 200;
end;

function TksInputListTrackBarItem.CreateControl: TPresentedControl;
begin
  Result := TksTrackBar.Create(nil);
  (Result as TksTrackBar).ApplyStyle;
end;

class function TksInputListTrackBarItem.GetClassID: string;
begin
  Result := '{CBBF6D98-CCBD-4FCF-9AF4-6DE99E9E2362}'
end;

function TksInputListTrackBarItem.GetTrackBar: TksTrackBar;
begin
  Result := (FControl as TksTrackBar);
end;

function TksInputListTrackBarItem.GetValue: string;
begin
  Result := FloatToStr(TrackBar.Value);
end;

procedure TksInputListTrackBarItem.PaintControl(ACanvas: TCanvas);
var
  AThumbRect: TRectF;
begin
  inherited;
  AThumbRect := TrackBar.GetThumbRect;
  if TrackBar.Thumb <> nil then
  begin
    if TrackBar.Thumb.StyleState <> TStyleState.Applied then
      TrackBar.Thumb.ApplyStyleLookup;

    TrackBar.Thumb.PaintTo(ACanvas, AThumbRect);
  end;
end;

procedure TksInputListTrackBarItem.Reset;
begin
  inherited;
  TrackBar.Value := 0;
end;

procedure TksInputListTrackBarItem.SetValue(const AValue: string);
begin
  inherited;
  TrackBar.Value := StrToFloatDef(AValue, 0);
end;

procedure TksInputListTrackBarItem.TrackBarChange(Sender: TObject);
begin
  if Assigned(FksInputList.OnItemTrackBarChange) then
    FksInputList.OnItemTrackBarChange(FksInputList, Self, ID, StrToFloatDef(Value, 0));
end;

{ TksInputListSelectorItem }

constructor TksInputListSelectorItem.Create(AInputList: TksInputList);
begin
  inherited;
  

  FCombo := TComboBox.Create(nil);
  //FksInputList.FBuffer.AddObject(FCombo);
  FItems := TStringList.Create;
  FShowSelection := True;
  FCombo.OnChange := DoSelectorChanged;

end;

destructor TksInputListSelectorItem.Destroy;
begin
  FItems.Free;
  FCombo.Free;
  inherited;
end;

procedure TksInputListSelectorItem.DoSelectorChanged(Sender: TObject);
var
  ATask: ITask;
begin
  if FCombo.ItemIndex > -1 then
    Value := FCombo.Items[FCombo.ItemIndex];
  aTask := TTask.Create (procedure ()
   begin
      Sleep(200);
      // Copy files here
      TThread.Synchronize(nil, procedure
  begin

      if Assigned(FksInputList.OnSelectorItemSelected) then
        FksInputList.OnSelectorItemSelected(FksInputList, Self, ID, Value);
    end);
   end);
 aTask.Start;
end;

class function TksInputListSelectorItem.GetClassID: string;
begin
  Result := '{D5BB3373-99AB-4BB4-A478-96EAAC0AD091}';
end;

function TksInputListSelectorItem.GetValue: string;
begin
  Result := FValue;
end;

procedure TksInputListSelectorItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;
{  FType := ksItemSelector;
  if AJson.FindValue('type') <> nil then
  begin
    if AJson.Values['type'].Value = 'date' then FType := ksDateSelector;
    if AJson.Values['type'].Value = 'time' then FType := ksTimeSelector;

  end;  }
  FItems.CommaText := AJson.Values['items'].Value;
end;

procedure TksInputListSelectorItem.MouseUp(ATapEvent: Boolean);
begin
  if (FMouseDown) and (ATapEvent) then
  begin
    FCombo.OnChange := nil;
    FCombo.Items.Assign(FItems);
    FCombo.ItemIndex := FCombo.Items.IndexOf(FValue);
    FCombo.DropDown;
    FCombo.OnChange := DoSelectorChanged;
  end;
  inherited;
end;

procedure TksInputListSelectorItem.Reset;
begin
  inherited;
  Value := '';
end;

procedure TksInputListSelectorItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;
  AJson.AddPair('items', FItems.CommaText);
end;

procedure TksInputListSelectorItem.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TksInputListSelectorItem.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FDetail := Value;
    FValue := Value;
    Changed;
  end;
end;

{ TInputListAccessoryImages }

constructor TInputListAccessoryImages.Create;
begin
  inherited Create(True);
  Add(TInputListAccessoryImage.Create(atMore, GetAccessoryFromResource(['listviewstyle','accessorymore'])));
  Add(TInputListAccessoryImage.Create(atCheckmark, GetAccessoryFromResource(['listviewstyle','accessorycheckmark'])));
  Add(TInputListAccessoryImage.Create(atDetail, GetAccessoryFromResource(['listviewstyle','accessorydetail'])));

end;

function TInputListAccessoryImages.GetItemByAccessory(AType: TksInputAccessoryType): TInputListAccessoryImage;
var
  AItem: TInputListAccessoryImage;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.AccessoryType = AType then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

{ TInputListAccessoryImage }

constructor TInputListAccessoryImage.Create(AType: TksInputAccessoryType; ABmp: TBitmap);
begin
  inherited Create;
  FAccessoryType := AType;
  FBitmap := ABmp;
end;

destructor TInputListAccessoryImage.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

{ TksInputListSeperator }


constructor TksInputListSeperator.Create(AInputList: TksInputList);
begin
  inherited;
  FHeight := C_DEFAULT_SEPERATOR_HEIGHT;
  FFont.Size := 11;
  FHorzAlign := TTextAlign.Leading;
end;

procedure TksInputListSeperator.DrawToCanvas(ACanvas: TCanvas);
var
  AText: string;
begin
  AText := FTitle;
  FTitle := '';
  FBackground := claNull;
  inherited DrawToCanvas(ACanvas);
  FTitle := AText;
  FBackground := claNull;
  ACanvas.Fill.Color := claDimgray;
  ACanvas.Font.Size := 12;
  FContentRect.Inflate(0, -4);
  ACanvas.FillText(FContentRect, FTitle, False, 1, [], FHorzAlign, TTextAlign.Trailing);
  FContentRect.Inflate(0, 4);
end;

class function TksInputListSeperator.GetClassID: string;
begin
  Result := '{7DAC9C9A-3222-418B-A4B1-2EB33EC99466}';
end;

function TksInputListSeperator.GetValue: string;
begin
  Result := '';
end;

procedure TksInputListSeperator.SetHorzAlign(const Value: TTextAlign);
begin
  if FHorzAlign <> Value then
  begin
    FHorzAlign := Value;
    Changed;
  end;
end;

{ TksInputListItem }

procedure TksInputListItem.DrawToCanvas(ACanvas: TCanvas);
begin
  inherited DrawToCanvas(ACanvas);

end;

class function TksInputListItem.GetClassID: string;
begin
  Result := '{4457C117-E5E0-41F3-9EC9-71BBB30C198D}';
end;

procedure TksInputListItem.UpdateRects;
begin
  inherited UpdateRects;
  //FImageRect := RectF(0, 0, FksInputList.Width, Height);
end;

{ TksInputListImageItem }

constructor TksInputListImageItem.Create(AInputList: TksInputList);
begin
  inherited;
  FImage := TBitmap.Create;
end;

destructor TksInputListImageItem.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TksInputListImageItem.DoCustomDraw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.DrawBitmap(FImage, FImage.BoundsF, FItemRect, 1, True);
end;

procedure TksInputListImageItem.DrawToCanvas(ACanvas: TCanvas);
begin
  inherited DrawToCanvas(ACanvas);

end;

class function TksInputListImageItem.GetClassID: string;
begin
  Result := '{E5E6CA88-3228-46BD-8A66-41E0EB674115}';
end;

procedure TksInputListImageItem.UpdateRects;
begin
  inherited UpdateRects;
  FImageRect.Left := 0;
  FImageRect.Width := FksInputList.Width;
end;

{ TksInputListDateTimeSelectorItem }

constructor TksInputListDateTimeSelectorItem.Create(AInputList: TksInputList);
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    FDateTimePicker := FPickerService.CreateDateTimePicker;
    //FDatePicker.ShowMode := TDatePickerShowMode.Date;
    //FTimePicker := FPickerService.CreateDateTimePicker;
    //FTimePicker.ShowMode := TDatePickerShowMode.Time;
  end;
end;

destructor TksInputListDateTimeSelectorItem.Destroy;
begin

  inherited;
end;

procedure TksInputListDateTimeSelectorItem.DoSelectDateTime(Sender: TObject; const ADateTime: TDateTime);
begin
  DateTime := ADateTime;
  if Assigned(FksInputList.OnDateTimeSelected) then
    FksInputList.OnDateTimeSelected(FksInputList, Self, FItemID, ADateTime);
end;

class function TksInputListDateTimeSelectorItem.GetClassID: string;
begin
  Result := '{E4F2A9C8-FDBC-4E99-B434-66B444F5A3B3}';
end;

function TksInputListDateTimeSelectorItem.GetDateTime: TDateTime;
begin
  Result := FDateTime;
end;


function TksInputListDateTimeSelectorItem.GetValue: string;
begin
  Result := DateToISO8601(FDateTime);
end;

procedure TksInputListDateTimeSelectorItem.LoadStructure(AJson: TJSONObject);
begin
  inherited;

end;

procedure TksInputListDateTimeSelectorItem.MouseUp(ATapEvent: Boolean);
begin
  if ((FMouseDown) and (ATapEvent)) and (not ReadOnly) and (FEnabled) then
  begin
    case FKind of
      ksDateSelector: FDateTimePicker.ShowMode := TDatePickerShowMode.Date;
      ksTimeSelector: FDateTimePicker.ShowMode := TDatePickerShowMode.Time;
    end;
    FDateTimePicker.Date := FDateTime;
    FDateTimePicker.OnDateChanged := DoSelectDateTime;
    FDateTimePicker.Show;
  end;
  inherited;
end;

procedure TksInputListDateTimeSelectorItem.Reset;
begin
  inherited;
  DateTime := 0;
end;

procedure TksInputListDateTimeSelectorItem.SaveStructure(AJson: TJsonObject);
begin
  inherited;

end;

procedure TksInputListDateTimeSelectorItem.SetDateTime(const AValue: TDateTime);
begin
  FDateTime := AValue;

  case FKind of
    ksDateSelector: Detail := FormatDateTime('ddd, d mmmm, yyyy', FDateTime);
    ksTimeSelector: Detail := FormatDateTime('hh:nn', FDateTime);
  end;

end;


procedure TksInputListDateTimeSelectorItem.SetKind(const Value: TksDateTimeSelectorKind);
begin
  FKind := Value;
  SetDateTime(FDateTime);
end;

procedure TksInputListDateTimeSelectorItem.SetValue(const Value: string);
begin
  try
    FDateTime :=  ISO8601ToDate(Value)
  except
    // invalid date/time?
  end;
end;

{ TksInputListBadgeItem }

constructor TksInputListBadgeItem.Create(AInputList: TksInputList);
begin
  inherited;
  FBadgeColor := claDodgerblue;
  FBadgeTextColor := claWhite;
end;

procedure TksInputListBadgeItem.DrawToCanvas(ACanvas: TCanvas);
var
  r: TRectF;
  ADetail: string;
  ACenter: TPointF;
begin
  ADetail := FDetail;
  FDetail := '';
  inherited DrawToCanvas(ACanvas);
  FDetail := ADetail;
  if Trim(FDetail) = '' then
    Exit;
  ACanvas.Fill.Color := FBadgeColor;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Font.Size := 13;
  r := FContentRect;
  r.Left := r.Right - (ACanvas.TextWidth(FDetail)+14);
  ACenter := r.CenterPoint;
  r.Top := ACenter.Y-13;
  r.Bottom := ACenter.Y+13;
  ACanvas.FillRect(r, 6, 6, AllCorners, 1);

  ACanvas.Fill.Color := FBadgeTextColor;
  ACanvas.FillText(r, FDetail, False, 1, [], TTextAlign.Center, TTextAlign.Center);
end;

class function TksInputListBadgeItem.GetClassID: string;
begin
  Result := '{5CE55631-0B72-4BDA-8805-D2EDF5E639FF}';
end;

function TksInputListBadgeItem.GetDetailTextColor: TAlphaColor;
begin
  Result := FBadgeTextColor;
end;

procedure TksInputListBadgeItem.SetBadgeColor(const Value: TAlphaColor);
begin
  if FBadgeColor <> Value then
  begin
    FBadgeColor := Value;
    Changed;
  end;
end;

{ TksInputListChatItem }

constructor TksInputListChatItem.Create(AInputList: TksInputList);
begin
  inherited;
  FColor := claDodgerblue;
  FTextColor := claWhite;
  FCached := TBitmap.Create;
  FCached.BitmapScale := GetScreenScale;
  FUse24HourTime := True;
end;

function TksInputListChatItem.CalculateHeight: single;
var
  AEmojiOnly: Boolean;
begin
  AEmojiOnly := IsEmojiOnly;

  ATextLayout.Font.Size := 16;
  if AEmojiOnly then
    ATextLayout.Font.Size := 36;
  ATextLayout.BeginUpdate;
  ATextLayout.Text := FBody;
  ATextLayout.WordWrap := True;

  ATextLayout.Color := FTextColor;
  ATextLayout.HorizontalAlign := TTextAlign.Leading;

  ATextLayout.Padding.Rect := RectF(0, 0, 0, 0);
  ATextLayout.Trimming := TTextTrimming.None;
  ATextLayout.TopLeft := PointF(0,0);
  ATextLayout.MaxSize := PointF(FksInputList.Width * 0.7, MaxSingle);
  ATextLayout.EndUpdate;

  //Result := ATextLayout.TextRect.Height+20;
  //if FSender <> '' then
  if AEmojiOnly then
    Result := ATextLayout.TextRect.Height+40
  else

  Result := ATextLayout.TextRect.Height+40;

end;

destructor TksInputListChatItem.Destroy;
begin
  FCached.Free;
  inherited;
end;

procedure TksInputListChatItem.DrawToCanvas(ACanvas: TCanvas);
var
  r: TRectF;
  AEmojiOnly: Boolean;
  AStr: string;
  APadding: single;
begin
  inherited;

  APadding := 8;

  if FCached.IsEmpty then
  begin
    AEmojiOnly := IsEmojiOnly;
    ATextLayout.Font.Size := 16;
    if AEmojiOnly then
      ATextLayout.Font.Size := 36;
    ATextLayout.BeginUpdate;

    AStr := FBody;




    ATextLayout.Text := Trim(AStr);
    ATextLayout.WordWrap := True;

    ATextLayout.Color := FTextColor;
    ATextLayout.HorizontalAlign := TTextAlign.Leading;
    ATextLayout.Padding.Rect := RectF(0, 0, 0, 0);
    ATextLayout.Trimming := TTextTrimming.None;

    //ATextLayout.TopLeft := PointF((GetScreenScale * 4), (GetScreenScale * 4));
    ATextLayout.TopLeft := PointF(APadding, APadding);

    //if AEmojiOnly then
    //  ATextLayout.TopLeft := PointF(8, -4);

    ATextLayout.MaxSize := PointF(FksInputList.Width * 0.7, MaxSingle);
    ATextLayout.EndUpdate;


    APadding := (8);
    FCached.SetSize(Round(GetScreenScale*(ATextLayout.Width+(APadding*2))), Round(GetScreenScale*((ATextLayout.Height+(APadding*2)))));
    //case AEmojiOnly of
    //  True:  FCached.SetSize(Round(GetScreenScale*(ATextLayout.Width+APadding)), Round(GetScreenScale*((ATextLayout.Height))));
    //  False: FCached.SetSize(Round(GetScreenScale*(ATextLayout.Width+APadding)), Round(GetScreenScale*((ATextLayout.Height+APadding))));
    //end;

    FCached.Canvas.BeginScene(nil);
    try
      FCached.Clear(claNull);
      if AEmojiOnly = False then
      begin
        FCached.Canvas.Fill.Kind := TBrushKind.Solid;
        FCached.Canvas.Fill.Color := FColor;
        FCached.Canvas.FillRect(RectF(0, 0, Round(FCached.Width/GetScreenScale), Round(FCached.Height/GetScreenScale)), 8, 8, AllCorners, 1);
      end;
      FCached.Canvas.Fill.Color := FColor;
      ATextLayout.RenderLayout(FCached.Canvas);

    finally
      FCached.Canvas.EndScene;
    end;
  end;

  AStr := Trim(FSender);
  if AStr <> '' then
    AStr := AStr + ' - ';

  if FUse24HourTime then
    AStr := AStr + FormatDateTime('hh:nn', FDateTime)
  else
    AStr := AStr + FormatDateTime('h:nn am/pm', FDateTime);
  ACanvas.Fill.Color := claSilver;
  ACanvas.Font.Size := 12;
  r := FContentRect;
  r.Inflate(-4, -6);

  ACanvas.FillText(r, AStr, False, 1, [], FAlignment, TTextAlign.Leading);


  r := FCached.BoundsF;
  r.Width := r.Width/GetScreenScale;
  r.Height := r.Height/GetScreenScale;

  case FAlignment of
    TTextAlign.Leading: OffsetRect(r, (4), FContentRect.Top+(24));
    TTextAlign.Center: OffsetRect(r, (4), FContentRect.Top+(24));
    TTextAlign.Trailing: OffsetRect(r, FContentRect.Right-r.Width, FContentRect.Top+(24));
  end;
 

  ACanvas.DrawBitmap(FCached, FCached.BoundsF, r, 1, True);
end;

class function TksInputListChatItem.GetClassID: string;
begin
  Result := '{FD931C67-7C01-4C78-B3D4-EF66A94C2593}';
end;

function TksInputListChatItem.GetHeight: Single;
begin
  Result := CalculateHeight;
end;

function TksInputListChatItem.IsEmojiOnly: Boolean;
var
  c: Char;
begin
  Result := True;
  for c in FBody do
  begin
    if Ord(c) < 5000 then
    begin
      Result := False;
      Exit;
    end;
    {if (c.IsLetterOrDigit = True) or
       (c.IsPunctuation = True) or
       (c.IsSeparator = True) or
       (c.IsSymbol = True) or
       (c.IsWhiteSpace = True) or
       (c.IsControl) then
    begin
      Result := False;
      Exit;
    end;}
  end;
end;

procedure TksInputListChatItem.MouseDown;
begin
  inherited;
end;

procedure TksInputListChatItem.SetAlignment(const Value: TTextAlign);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TksInputListChatItem.SetBody(const Value: string);
begin
  if FBody <> Value then
  begin
    FBody := Value;
    Changed;
  end;
end;

procedure TksInputListChatItem.SetDateTime(const Value: TDateTime);
begin
  if CompareDateTime(FDateTime, Value) <> 0 then
  begin
    FDateTime := Value;
    Changed;
  end;
end;

procedure TksInputListChatItem.SetSender(const Value: string);
begin
  IF FSender <> Value then
  begin
    FSender := Value;
    Changed;
  end;
end;

procedure TksInputListChatItem.SetUse24HourTime(const Value: Boolean);
begin
  if FUse24HourTime <> Value then
  begin
    FUse24HourTime := Value;
    Changed;
  end;
end;

procedure TksInputListChatItem.UpdateRects;
begin
  //Height := CalculateHeight;
  inherited UpdateRects;
end;

initialization

  //TempForm := TForm.CreateNew(nil);

  AAccessoriesList := TInputListAccessoryImages.Create;
  ATextLayout :=  TTextLayoutManager.DefaultTextLayout.Create;
  AScreenScale := 0;


finalization

  //TempForm.Free;
  AAccessoriesList.Free;
  ATextLayout.Free;

end.



