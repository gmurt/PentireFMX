{*******************************************************************************
*                                                                              *
*  PentireFMX                                                                  *
*                                                                              *
*  https://github.com/gmurt/PentireFMX                                         *
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

unit ksVirtualListView;

interface


uses System.Classes, System.Types, FMX.Graphics, System.UITypes,
  System.Generics.Collections, FMX.Types, FMX.InertialMovement, System.UIConsts,
  FMX.StdCtrls, FMX.Controls, FMX.Platform, FMX.Objects, FMX.Edit,
  FMX.TextLayout, System.RTTI, ksAccessories;

const
  C_VLIST_ITEM_DEFAULT_HEIGHT = 44;
  C_VLIST_HEADER_DEFAULT_HEIGHT = 24;

  C_VLIST_DEFAULT_SELECTED_COLOR = $FFE9E9E9;

  C_VLIST_CACHE_COUNT = 100;
  C_LONG_TAP_DURATION = 400;

{$IFDEF MSWINDOWS}
  C_VLIST_SCROLLBAR_WIDTH = 16;
{$ELSE}
  C_VLIST_SCROLLBAR_WIDTH = 8;
{$ENDIF}
  C_ACCESSORY_WIDTH = 12;

type

  TksVListItem = class;
  TksVListItemList = class;
  TksVirtualListView = class;
  TksVListActionButton = class;
  TksVListActionButtons = class;

  TksSelectionType = (ksSingleSelect, ksMultiSelect);
  TksVListCheckBoxAlign = (ksCbLeftAlign, ksCbRightAlign);
  TksVListActionButtonAlign = (ksAbLeftAlign, ksAbRightAlign);
  TksVListSwipeDirection = (ksSwipeFromLeft, ksSwipeFromRight);
  TksVListItemPurpose = (None, Header, Seperator);
  TksVListItemState = (Normal, Deleting, Deleted, Sliding);
  TksVListItemSelectorType = (ksSelectorNone, ksSelectorEdit, ksSelectorPicker, ksSelectorDate, ksSelectorTime, ksSelectorDateTime);
  TksImageShape = (ksImageRect, ksImageCircle);

  TksVListItemClickEvent = procedure(Sender: TObject; AItem: TksVListItem) of object;
  TksVListItemLongTapEvent = procedure(Sender: TObject; AItem: TksVListItem) of object;
  TksVListItemSwipeEvent = procedure(Sender: TObject; ARow: TksVListItem; ASwipeDirection: TksVListSwipeDirection; AButtons: TksVListActionButtons)  of object;
  TksVListDeletingItemEvent = procedure(Sender: TObject; AItem: TksVListItem; var ACanDelete: Boolean) of object;
  TksItemActionButtonClickEvent = procedure(Sender: TObject; ARow: TksVListItem; AButton: TksVListActionButton) of object;
  TksItemEditInputEvent = procedure(Sender: TObject; ARow: TksVListItem; AText: string) of object;

  TksItemBeforeSelectPickerItemEvent = procedure(Sender: TObject; ARow: TksVListItem; var AText: string) of object;
  TksItemSelectPickerItemEvent = procedure(Sender: TObject; ARow: TksVListItem; AText: string) of object;

  TksItemDateSelectedEvent = procedure(Sender: TObject; ARow: TksVListItem; ADate: TDateTime) of object;
  TksItemTimeSelectedEvent = procedure(Sender: TObject; ARow: TksVListItem; ATime: TDateTime) of object;
  TksItemDateTimeSelectedEvent = procedure(Sender: TObject; ARow: TksVListItem; ADateTime: TDateTime) of object;
  TksItemGetPickerItemsEvent = procedure(Sender: TObject; ARow: TksVListItem; var ASelected: string; AItems: TSTrings) of object;

  TksItemSwitchClicked = procedure(Sender: TObject; AItem: TksVListItem; ASwitchID: string; AChecked: Boolean) of object;
  TksItemSegmentButtonClicked = procedure(Sender: TObject; AItem: TksVListItem; ASegID: string; AItemIndex: integer) of object;

  TksVirtualListViewAppearence = class(TPersistent)
  private
    [weak]FListView: TksVirtualListView;
    FBackground: TAlphaColor;
    FItemBackground: TAlphaColor;
    FSeparatorColor: TAlphaColor;
    FHeaderColor: TAlphaColor;
    FHeaderFontColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    FSelectedFontColor: TAlphaColor;
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetItemBackground(const Value: TAlphaColor);
    procedure SetSeparatorBackground(const Value: TAlphaColor);
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    procedure SetHeaderFontColor(const Value: TAlphaColor);
  public
    constructor Create(AListView: TksVirtualListView);
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Background: TAlphaColor read FBackground write SetBackground default claWhite;
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default claNull;
    property HeaderFontColor: TAlphaColor read FHeaderFontColor write SetHeaderFontColor default claNull;
    property SeparatorColor: TAlphaColor read FSeparatorColor write SetSeparatorBackground default claDarkgray;
    property ItemBackground: TAlphaColor read FItemBackground write SetItemBackground default claWhite;
    property SelectedColor: TAlphaColor read FSelectedColor write SetSelectedColor default C_VLIST_DEFAULT_SELECTED_COLOR;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write SetSelectedFontColor default claNull;
  end;

  TksVListActionButton = class
  strict private
    FWidth: integer;
    FIcon: TBitmap;
    FTextColor: TAlphaColor;
    FColor: TAlphaColor;
    FText: string;
    FIsDeleteButton: Boolean;
    FAccessory: TksAccessoryType;
    FTagObject: TObject;
  private
    FButtonRect: TRectF;
    procedure SetAccessory(const Value: TksAccessoryType);
  private
    procedure SetTextColor(const Value: TAlphaColor);
  public
    constructor Create(AIsDelete: Boolean);
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
    property Accessory: TksAccessoryType read FAccessory write SetAccessory;
    property Text: string read FText write FText;

    property TextColor: TAlphaColor read FTextColor write SetTextColor default claWhite;
    property Color: TAlphaColor read FColor write FColor;
    property Width: integer read FWidth write FWidth default 80;
    property IsDeleteButton: Boolean read FIsDeleteButton write FIsDeleteButton;
    property TagObject: TObject read FTagObject write FTagObject;
  end;

  TksVListActionButtons = class(TObjectList<TksVListActionButton>)
  strict private
    FAlignment: TksVListActionButtonAlign;
  private
    function GetTotalWidth: integer;
  public
    constructor Create(AOwner: TksVListItem);
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
    function AddButton(AText: string; AColor, ATextColor: TAlphaColor;
      const AIcon: TksAccessoryType = atNone; const AWidth: integer = 60): TksVListActionButton;
    function ButtonAtXY(x, y: single): TksVListActionButton;
    property Alignment: TksVListActionButtonAlign read FAlignment
      write FAlignment;
    property TotalWidth: integer read GetTotalWidth;
  end;

  TksVListCheckBoxOptions = class(TPersistent)
  private
    [weak] FOwner: TksVirtualListView;
    FVisible: Boolean;
    FMode: TksSelectionType;
    FAlignment: TksVListCheckBoxAlign;
    FMakeBold: Boolean;
    procedure SetAlignment(const Value: TksVListCheckBoxAlign);
    procedure SetMode(const Value: TksSelectionType);
    procedure SetVisible(const Value: Boolean);
    procedure Changed;
    procedure SetMakeBold(const Value: Boolean);
  public
    constructor Create(AOwner: TksVirtualListView); virtual;
  published
    property Visible: Boolean read FVisible write SetVisible default False;
    property Mode: TksSelectionType read FMode write SetMode
      default ksSingleSelect;
    property Alignment: TksVListCheckBoxAlign read FAlignment write SetAlignment
      default ksCbRightAlign;
    property MakeBold: Boolean read FMakeBold write SetMakeBold default False;
  end;

  TksVListSelectionOptions = class(TPersistent)
  private
    [weak]
    FOwner: TksVirtualListView;
    FKeepSelection: Boolean;
    FSelectionType: TksSelectionType;
    procedure SetKeepSelection(const Value: Boolean);
    procedure SetSelectionType(const Value: TksSelectionType);
    procedure Changed;
  public
    constructor Create(AOwner: TksVirtualListView); virtual;
  published
    property KeepSelection: Boolean read FKeepSelection write SetKeepSelection
      default False;
    property SelectionType: TksSelectionType read FSelectionType
      write SetSelectionType default ksSingleSelect;
  end;

  TksVListItemBaseObject = class(TPersistent)
  private
    [weak]FOwner: TksVListItem;
    FID: string;
    FVertAlign: TVerticalAlignment;
    FHorzAlign: TAlignment;
    FLeft: single;
    FTop: single;
    FWidth: single;
    FHeight: single;
    FVisible: Boolean;
    FUsePercentForXPos: Boolean;
    FObjectRect: TRectF;
    FOnChange: TNotifyEvent;
    procedure SetVertAlign(const Value: TVerticalAlignment);
    procedure SetHeight(const Value: single);
    procedure SetLeft(const Value: single);
    procedure SetTop(const Value: single);
    procedure SetWidth(const Value: single);
    procedure SetHorzAlign(const Value: TAlignment);
    procedure SetVisible(const Value: Boolean);
    procedure SetUsePercentForXPos(const Value: Boolean);
    function GetListview: TksVirtualListView;
  protected
    function CalcObjectRect(AItemRect: TRectF): TRectF; virtual;
    procedure Changed; virtual;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); virtual;
    procedure Clicked(x, y: single); virtual;
  public
    constructor Create(AItem: TksVListItem); virtual;
    procedure ClearCache; virtual;
    procedure SetSize(AWidth, AHeight: single);
    property Left: single read FLeft write SetLeft;
    property Top: single read FTop write SetTop;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property HorzAlign: TAlignment read FHorzAlign write SetHorzAlign;
    property VertAlign: TVerticalAlignment read FVertAlign write SetVertAlign default TVerticalAlignment.taVerticalCenter;
    property UsePercentForXPos: Boolean read FUsePercentForXPos write SetUsePercentForXPos default False;
    property ID: string read FID write FID;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property ListView: TksVirtualListView read GetListview;
  end;

  TksVListItemTextObject = class(TksVListItemBaseObject)
  private
    FAutoSize: Boolean;
    {$IFDEF IOS}
    //FCached: TBitmap;
    {$ENDIF}
    //FCachedSize: TRectF;
    FTextSize: TPointF;
    //FTextRect: TRectF;
    FTextLayout: TTextLayout;
    FTextSettings: TTextSettings;
    FText: string;
    FMaxWidth: integer;
    FActualTextWidth: single;
    FPasswordField: Boolean;
    procedure SetText(const Value: string);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetMaxWidth(const Value: integer);
    procedure SetPasswordField(const Value: Boolean);
    procedure TextSettingsChanged(Sender: TObject);
  protected
    procedure Changed; override;
    function ActualTextWidth: single;
    function CalculateTextWidth(AText: string; AFont: TFont; AWordWrap: Boolean;
      const AMaxWidth: single = 0; const APadding: single = 0): single;
    function CalculateSize: TSizeF;
    procedure BeforeRenderText(ACanvas: TCanvas; ARect: TRectF); virtual;
  public
    constructor Create(AItem: TksVListItem); override;
    destructor Destroy; override;
    procedure ClearCache; override;
    function CalculateWidth: single;
    function MatchesFilter(AFilter: string): Boolean;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    property Text: string read FText write SetText;
    property TextSettings: TTextSettings read FTextSettings;
    property Font: TFont read GetFont write SetFont;
    property MaxWidth: integer read FMaxWidth write SetMaxWidth default 0;
    property PasswordField: Boolean read FPasswordField write SetPasswordField;
  end;

  TksVListItemLineObject = class(TksVListItemBaseObject)
  private
    FStroke: TStrokeBrush;
  public
    constructor Create(AItem: TksVListItem); override;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    property Stroke: TStrokeBrush read FStroke;
  end;


  TksVListItemShapeObject = class(TksVListItemBaseObject)
  private
    FStroke: TStrokeBrush;
    FFill: TBrush;
    FCornerRadius: single;
    //FCanvas: TBitmap;
  public
    constructor Create(AItem: TksVListItem); override;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    property CornerRadius: single read FCornerRadius write FCornerRadius;
    property Fill: TBrush read FFill;
    property Stroke: TStrokeBrush read FStroke;
  end;

  TksVListItemSegmentButtons = class(TksVListItemShapeObject)
  private
    FCaptions: TStrings;
    FItemIndex: integer;
    FColor: TAlphaColor;
    FHideInactiveText: Boolean;
    procedure SetItemIndex(const Value: integer);
    procedure SetColor(const Value: TAlphaColor);
  public
    constructor Create(AItem: TksVListItem); override;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    procedure Clicked(x, y: single); override;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Color: TAlphaColor read FColor write SetColor default claDodgerBlue;
    property HideInactiveText: Boolean read FHideInactiveText write FHideInactiveText default False;
  end;

  TksVListItemBubbleObject = class(TksVListItemTextObject)
  private
    FColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FSender: string;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetTextColor(const Value: TAlphaColor);
  public
    constructor Create(AItem: TksVListItem); override;
    property Color: TAlphaColor read FColor write SetColor;
    property TextColor: TAlphaColor read FTextColor write SetTextColor;
    property Sender: string read FSender write FSender;


  end;

  TksVListItemImageObject = class(TksVListItemBaseObject)
  private
    FBitmap: TBitmap;
    FRenderImage: TBitmap;
    FBackground: TAlphaColor;
    FOpacity: single;
    FImageShape: TksImageShape;
    FBadge: integer;
    function GetIsEmpty: Boolean;
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetOpacity(const Value: single);
    procedure SetImageShape(const Value: TksImageShape);
    procedure SetBadge(const Value: integer);
  protected
    procedure SetBitmap(const Value: TBitmap); virtual;
  public
    constructor Create(AItem: TksVListItem); override;
    destructor Destroy; override;
    procedure SetOpaqueColor(AColor: TAlphaColor); virtual;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    procedure SetProperties(ABitmap: TBitmap; AOpaqueColor, ABackgroundColor: TAlphaColor);
    property IsEmpty: Boolean read GetIsEmpty;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Background: TAlphaColor read FBackground write SetBackground;
    property Opacity: single read FOpacity write SetOpacity;
    property ImageShape: TksImageShape read FImageShape write SetImageShape;
    property Badge: integer read FBadge write SetBadge default 0;

  end;

  TksVListItemSwitchObject = class(TksVListItemBaseObject)
  private
    FChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  protected
    procedure Clicked(x, y: single); override;
  public
    constructor Create(AItem: TksVListItem); override;
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    procedure Toggle;
    property Checked: Boolean read FChecked write SetChecked default False;
  end;

  TksVListItemProgressBarObject = class(TksVListItemShapeObject)
  private
    FBackground: TAlphaColor;
    FMax: integer;
    FValue: integer;
    procedure SetMax(const Value: integer);
    procedure SetValue(const Value: integer);
  public
    procedure DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF); override;
    property Max: integer read FMax write SetMax;
    property Value: integer read FValue write SetValue;
  end;


  TksVListItemAccessoryObject = class(TksVListItemImageObject)
  private
    FAccessoryType: TksAccessoryType;
    FColor: TAlphaColor;
    procedure RedrawAccessory;
    procedure SetAccessoryType(const Value: TksAccessoryType);
    procedure SetColor(const Value: TAlphaColor);
  protected
    procedure Changed; override;
  public
    constructor Create(AItem: TksVListItem); override;
    procedure SetOpaqueColor(AColor: TAlphaColor); override;
    property AccessoryType: TksAccessoryType read FAccessoryType write SetAccessoryType default atNone;
    property Color:  TAlphaColor read FColor write SetColor default claNull;
  end;

  TksVListObjectList = class(TObjectList<TksVListItemBaseObject>)
  public
    function ObjectByID(AID: string): TksVListItemBaseObject;
    function ObjectAtPos(AItem: TksVListItem; x, y: single): TksVListItemBaseObject;
    procedure ClearCache;
  end;

  TksVListItem = class // (TFmxObject)
  private
    [weak]FOwner: TksVListItemList;
    // FCache4dRow: TBitmap;
    FData: TDictionary<string, TValue>;
    FBackground: TAlphaColor;
    FCanSelect: Boolean;
    FChecked: Boolean;
    FHeight: integer;
    FSelected: Boolean;
    FItemRect: TRectF;
    FImage: TksVListItemImageObject;
    FQuantity: TksVListItemTextObject;
    FTitle: TksVListItemTextObject;
    FSubTitle: TksVListItemTextObject;
    FDetail: TksVListItemTextObject;
    FAccessory: TksVListItemAccessoryObject;
    FActionButtons: TksVListActionButtons;
    FOffset: integer;
    FChanged: Boolean;
    FUpdateCount: integer;
    FAbsoluteIndex: integer;
    FIndex: integer;
    FSwipeCalc: TAniCalculations;
    FDeleteCalc: TAniCalculations;
    FTagInt: integer;
    FPurpose: TksVListItemPurpose;
    FState: TksVListItemState;
    FIconSize: integer;
    // events
    FOnClick: TNotifyEvent;
    FObjects: TksVListObjectList;
    FTagStr: string;
    FTagObject:Tobject;
    FCheckBoxVisible: Boolean;
    FOnEditInput: TksItemEditInputEvent;
    FBeforeSelectPickerItem: TksItemBeforeSelectPickerItemEvent;
    FOnSelectPickerItem: TksItemSelectPickerItemEvent;
    FSelectedDate: TDateTime;
    FSelectedDateTime: TDateTime;
    FSelectedTime: TDateTime;
    FSelectedItem: string;
    FPickerItems: TStrings;
    FDisableLeftMargin: Boolean;
    FDefaultPickerItem: string;
    FUseActionSheet: Boolean;
    FEditFieldKeyboardType: TVirtualKeyboardType;
    FSelectorType: TksVListItemSelectorType;
    FOnDateSelected: TksItemDateSelectedEvent;
    FOnDateTimeSelected: TksItemDateTimeSelectedEvent;
    FOnTimeSelected: TksItemTimeSelectedEvent;
    procedure Changed;
    procedure SetAccessory(const Value: TksVListItemAccessoryObject);
    procedure SetDetail(const Value: TksVListItemTextObject);
    procedure SetImage(const Value: TksVListItemImageObject);
    procedure SetTitle(const Value: TksVListItemTextObject);
    procedure SetQuantity(const Value: TksVListItemTextObject);
    procedure SetHeight(const Value: integer);
    procedure UpdateStandardObjectPositions;
    procedure SetSelected(const Value: Boolean);
    procedure SelectItem(ADeselectAfter: integer);
    procedure SetChecked(const Value: Boolean);
    procedure SetSubTitle(const Value: TksVListItemTextObject);
    procedure SetBackground(const Value: TAlphaColor);
    procedure DeleteItem;
    procedure SlideOut(ADirection: TksVListSwipeDirection);
    procedure SlideIn;
    procedure SwipeCalcChange(Sender: TObject);
    procedure DeleteCalcChange(Sender: TObject);
    function CreateAniCalc(AOnChange: TNotifyEvent): TAniCalculations;
    procedure DoClicked(var AHandled: Boolean);
    procedure SetPurpose(const Value: TksVListItemPurpose);
    procedure SetOffset(const Value: integer);
    procedure SetCanSelect(const Value: Boolean);
    procedure SetIconSize(const Value: integer);
    procedure DoItemPickerChanged(Sender: TObject; AItem: string; AValueIndex: Integer);
    procedure DoDatePickerChanged(Sender: TObject; ADate: TDateTime);
    procedure DoTimePickerChanged(Sender: TObject; ATime: TDateTime);
    {$IFDEF IOS}
    procedure DoDateTimePickerChanged(Sender: TObject; ADateTime: TDateTime);
    {$ENDIF}

    procedure ShowEditInput;
    procedure ShowDatePicker(ASelected: TDateTime);
    {$IFDEF IOS}
    procedure ShowDateTimePicker(ASelected: TDateTime);
    {$ENDIF}
    procedure ShowTimePicker(ASelected: TDateTime);
    procedure ShowPicker;
    function GetItemData(const AIndex: string): TValue;

    procedure SetItemData(const AIndex: string; const Value: TValue);
    function GetHasData(const AIndex: string): Boolean;
    procedure SetSelectedDate(const Value: TDateTime);
    procedure SetSelectedTime(const Value: TDateTime);
    procedure SetSelectorType(const Value: TksVListItemSelectorType);
    procedure SetDisableLeftMargin(const Value: Boolean);  protected
    function MatchesFilter(AFilter: string): Boolean;
  public
    constructor Create(Owner: TksVListItemList); virtual;
    destructor Destroy; override;
    function IsItemVisible(AViewPort: TRectF): Boolean;
    function AddText(x, y: single; AText: string): TksVListItemTextObject; overload;
    function AddText(x, y: single; AText: string; AFontColor: TAlphaColor; AFontSize: integer): TksVListItemTextObject; overload;
    function AddText(x, y, AWidth: single; AText: string): TksVListItemTextObject; overload;
    function AddDetailText(y: single; AText: string): TksVListItemTextObject; overload;
    function AddImage(x, y, AWidth, AHeight: single; ABitmap: TBitmap): TksVListItemImageObject;
    function AddSwitch(x, y: single; AChecked: Boolean; const AID: string = ''): TksVListItemSwitchObject;
    function DrawRect(x, y, AWidth, AHeight, ACornerRadius: single; AStroke, AFill: TAlphaColor): TksVListItemShapeObject;
    function DrawLine(x, y, x2, y2: single;  AStroke: TAlphaColor): TksVListItemLineObject;
    function AddChatBubble(AText, ASender: string; ALeftAlign: Boolean): TksVListItemBubbleObject;
    procedure AddProgressBar(x, y, w, h: single; AValue, AMax: integer; AFill, ABackground, ABorder: TAlphaColor);

    function AddSegmentButtons(ATitles: array of string; AWidth: Integer; const AHeight: integer = 30): TksVListItemSegmentButtons;
    procedure ClearCache;

    procedure SetItemFont(AName: string; ASize: integer; AColor: TAlphaColor; AStyle: TFontStyles);
    procedure DrawToCanvas(ACanvas: TCanvas; AScrollPos: single; ADrawToCache: Boolean);
    property AbsoluteIndex: integer read FAbsoluteIndex;
    property Index: integer read FIndex;
    property Background: TAlphaColor read FBackground write SetBackground default claNull;
    property CanSelect: Boolean read FCanSelect write SetCanSelect default True;
    property Checked: Boolean read FChecked write SetChecked default False;
    property DisableLeftMargin: Boolean read FDisableLeftMargin write SetDisableLeftMargin default False;
    property Height: integer read FHeight write SetHeight;
    property Image: TksVListItemImageObject read FImage write SetImage;
    property Title: TksVListItemTextObject read FTitle write SetTitle;
    property Quantity: TksVListItemTextObject read FQuantity write SetQuantity;
    property SubTitle: TksVListItemTextObject read FSubTitle write SetSubTitle;
    property Detail: TksVListItemTextObject read FDetail write SetDetail;
    property Accessory: TksVListItemAccessoryObject read FAccessory write SetAccessory;
    property ItemRect: TRectF read FItemRect;
    property Selected: Boolean read FSelected write SetSelected;
    property Purpose: TksVListItemPurpose read FPurpose write SetPurpose
      default None;
    property TagInt: integer read FTagInt write FTagInt default 0;
    property TagStr: string read FTagStr write FTagStr;
    property TagObject: Tobject read FTagObject write FTagObject;
    property HasData[const AIndex: string]: Boolean read GetHasData;
    property Data[const AIndex: string]: TValue read GetItemData write SetItemData;

    property State: TksVListItemState read FState write FState default Normal;
    property Offset: integer read FOffset write SetOffset default 0;
    property IconSize: integer read FIconSize write SetIconSize default 28;
    property SelectedDate: TDateTime read FSelectedDate write SetSelectedDate;
    property SelectedTime: TDateTime read FSelectedTime write SetSelectedTime;
    property PickerItems: TStrings read FPickerItems;
    // events...
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property CheckBoxVisible: Boolean read FCheckBoxVisible write FCheckBoxVisible default True;
    property SelectorType: TksVListItemSelectorType read FSelectorType write SetSelectorType default ksSelectorNone;
    property EditFieldKeyboardType: TVirtualKeyboardType read FEditFieldKeyboardType write FEditFieldKeyboardType default TVirtualKeyboardType.Alphabet;
    property OnEditInput: TksItemEditInputEvent read FOnEditInput write FOnEditInput;
    property BeforeSelectPickerItem: TksItemBeforeSelectPickerItemEvent read FBeforeSelectPickerItem write FBeforeSelectPickerItem;
    property OnSelectPickerItem: TksItemSelectPickerItemEvent read FOnSelectPickerItem write FOnSelectPickerItem;
    property OnDateSelected: TksItemDateSelectedEvent read FOnDateSelected write FOnDateSelected;
    property OnTimeSelected: TksItemTimeSelectedEvent read FOnTimeSelected write FOnTimeSelected;
    property OnDateTimeSelected: TksItemDateTimeSelectedEvent read FOnDateTimeSelected write FOnDateTimeSelected;
    property Objects: TksVListObjectList read FObjects;
  end;

  TksVListItemList = class(tobjectlist<tksvlistitem>)
  private
    [weak]FOwner: TksVirtualListView;
    procedure UpdateItemRects;
    procedure Changed(AUpdateScrollLimits: Boolean);
    function GetCheckedCount: integer;
    function GetFirstChecked: TksVListItem;
  public
    constructor Create(AOwner: TksVirtualListView); virtual;
    function Add: TksVListItem; overload;
    function Add(ATitle, ASubTitle, ADetail: string; const AAccessory: TksAccessoryType = atNone): TksVListItem; overload;
    function Add(ATitle, ASubTitle, ADetail,AQuantity: string; const AAccessory: TksAccessoryType = atNone): TksVListItem; overload;
    function Add(ATitle, ASubTitle, ADetail: string; AImage: TBitmap; const AAccessory: TksAccessoryType = atNone): TksVListItem; overload;
    function AddPickerSelector(ATitle, ASubTitle, ADetail: string; AImage: TBitmap; ATagStr: string; AItems: array of string; const ADefaultItem: string = ''; const AUseActionSheet: Boolean = False): TksVListItem; overload;
    function AddPickerSelector(ATitle, ASubTitle, ADetail: string; AImage: TBitmap; ATagStr: string; AItems: TStrings; const ADefaultItem: string = ''; const AUseActionSheet: Boolean = False): TksVListItem; overload;
    function AddPickerSelector(ATitle, ASubTitle, ADetail: string; AImage: TBitmap; ATagStr: string; const AUseActionSheet: Boolean = False): TksVListItem; overload;
    function AddDateSelector(ATitle, ASubTitle: string; ASelected: TDateTime; AImage: TBitmap; ATagStr: string): TksVListItem;
    function AddDateTimeSelector(ATitle, ASubTitle: string; ASelected: TDateTime; AImage: TBitmap; ATagStr: string): TksVListItem;
    function AddTimeSelector(ATitle, ASubTitle: string; ASelected: TDateTime; AImage: TBitmap; ATagStr: string): TksVListItem;
    function AddInputSelector(ATitle, ASubTitle, ADetail, ATagStr: string): TksVListItem;
    function AddHeader(AText: string): TksVListItem;
    function InsertHeader(AIndex: integer;AText: string): TksVListItem;
    function AddSeperator(const AText: string = ''): TksVListItem;
    function AddChatBubble(AText, ASender: string; AColor, ATextColor: TAlphaColor; ALeftAlign: Boolean): TksVListItem;
    function Insert(AIndex: integer; ATitle, ASubTitle, ADetail,AQuantity: string; const AAccessory: TksAccessoryType = atNone): TksVListItem;
    function ItemAtPos(x, y: single): TksVListItem;
    function ItemByTagStr(ATagStr: string): TksVListItem;
    procedure Clear;
    procedure Delete(AIndex: integer; AAnimate: Boolean); overload;
    procedure Delete(AItem: TksVListItem; const AAnimate: Boolean = False); overload;
    procedure ClearCache;
    procedure SetCheckedByTagStr(ATagStr: string; AChecked: Boolean);
    property CheckedCount: integer read GetCheckedCount;
    property FirstChecked: TksVListItem read GetFirstChecked;
  end;

  TksVListPullToRefreshOptions = class(TPersistent)
  private
    FPullText: string;
    FReleaseText: string;
    FEnabled: Boolean;
  public
    constructor Create; virtual;
  published
    property PullText: string read FPullText write FPullText;
    property ReleaseText: string read FReleaseText write FReleaseText;
    property Enabled: Boolean read FEnabled write FEnabled default False;
  end;

  TksVListDeleteButton = class(TPersistent)
  private
    FEnabled: Boolean;
    FText: string;
    FColor: TAlphaColor;
    FTextColor: TAlphaColor;
    FWidth: integer;
    FShowImage: Boolean;
  public
    constructor Create; virtual;
  published
    property Color: TAlphaColor read FColor write FColor default claRed;
    property TextColor: TAlphaColor read FTextColor write FTextColor default claWhite;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Text: string read FText write FText;
    property ShowImage: Boolean read FShowImage write FShowImage default True;
    property Width: integer read FWidth write FWidth default 60;
  end;

  TksNoItemsText = class(TPersistent)
  private
    [weak]FOwner: TksVirtualListView;
    FEnabled: Boolean;
    FTextColor: TAlphaColor;
    FFont: TFont;
    FText: string;
    procedure Changed;
    procedure SetEnabled(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);

  public
    constructor Create(AListView: TksVirtualListView); virtual;
    destructor Destroy; override;
    procedure DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Text: string read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property TextColor: TAlphaColor read FTextColor write FTextColor default claSilver;
  end;

  TksScrollBar = class(TScrollBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TksAniCalc = class(TAniCalculations)
  public
    procedure UpdatePosImmediately;
  end;


  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksVirtualListView = class(TControl)
  private
    FAniCalc: TksAniCalc;
    FAppearence: TksVirtualListViewAppearence;
    FCheckBoxOptions: TksVListCheckBoxOptions;
    FItems: TksVListItemList;
    FScrollPos: integer;
    FScrollBar: TksScrollBar;
    FOnScroll: TNotifyEvent;
    FTotalItemHeight: single;
    FMouseDownPos: TPointF;
    FMaxScrollPos: integer;
    FUpdateCount: integer;
    FMouseDownTime: TDateTime;
    FSelectionOptions: TksVListSelectionOptions;
    FPendingRefresh: Boolean;
    FOnPullRefresh: TNotifyEvent;
    FOnReleaseIsFiring: Boolean;
    FOnReleaseText: TNotifyEvent;
    FTimerService: IFMXTimerService;
    FLongTapTimer: TFmxHandle;
    FMousePt: TPointF;
    FMouseDownItem: TksVListItem;
    FPullToRefresh: TksVListPullToRefreshOptions;
    FDeleteButton: TksVListDeleteButton;
    FItemHeight: integer;
    FItemIndex: integer;
    FRefreshing: Boolean;

    // events..FOnItemSwitchClick.
    FOnItemSwitchClick: TksItemSwitchClicked;
    FOnItemSegmentButtonClick: TksItemSegmentButtonClicked;
    FOnItemClick: TksVListItemClickEvent;
    FOnItemLongTap: TksVListItemLongTapEvent;
    FOnItemSwipe: TksVListItemSwipeEvent;
    FOnItemDeleted: TNotifyEvent;
    FCanDeleteItem: TksVListDeletingItemEvent;
    FHeaderHeight: integer;
    FNoItemsText: TksNoItemsText;
    FFocusedControl: TControl;
    FOnActionButtonClick: TksItemActionButtonClickEvent;
    FOnItemEditInputEvent: TksItemEditInputEvent;
    FOnItemDateSelectedEvent: TksItemDateSelectedEvent;
    FOnItemTimeSelectedEvent: TksItemTimeSelectedEvent;
    FOnItemDateTimeSelectedEvent: TksItemDateTimeSelectedEvent;
    FBeforeItemPickerSelectedEvent: TksItemBeforeSelectPickerItemEvent;
    FOnItemPickerSelectedEvent: TksItemSelectPickerItemEvent;
    FOnGetPickerItemsEvent: TksItemGetPickerItemsEvent;
    FScrollingDisabled: Boolean;
    FShowScrollBar: Boolean;

    procedure SetScrollPos(const Value: integer);
    procedure AniCalcChange(Sender: TObject);
    procedure AniCalcStart(Sender: TObject);
    procedure AniCalcStop(Sender: TObject);
    function GetViewport: TRectF;
    procedure SetCheckBoxOptions(const Value: TksVListCheckBoxOptions);
    procedure ScrollBarChanged(Sender: TObject);
    function CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
    procedure DoItemLongTap(AItem: TksVListItem);
    procedure LongTapTimerProc;
    procedure KillTimer(var ATimer: TFmxHandle);
    procedure DoItemClicked(AItem: TksVListItem; ACallClickEvent: Boolean);
    procedure ResetItemOffsets(AIgnore: TksVListItem);
    procedure SetItemHeight(const Value: integer);
    procedure SetHeaderHeight(const Value: integer);
    procedure SetAppearence(const Value: TksVirtualListViewAppearence);
    procedure SetItemIndex(const Value: integer);
    procedure SetNoItemsText(const Value: TksNoItemsText);
    function GetIsEmpty: Boolean;
    function GetTopItem: TksVListItem;
    procedure HideKeyboard;
  protected
    procedure Paint; override;
    procedure DrawPullToRefresh;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: single); override;
    procedure MouseMove(Shift: TShiftState; x, y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      x, y: single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer;
      var Handled: Boolean); override;
    procedure DoMouseLeave; override;
    procedure DoItemDeleted;
    procedure FocusControl(AItem: TksVListItem; AControl: TControl);
    procedure UnfocusControl;
    procedure DoItemEditInput(Sender: TObject; ARow: TksVListItem; AText: string);
    procedure DoItemDateSelected(Sender: TObject; ARow: TksVListItem; ADate: TDateTime);
    procedure DoItemTimeSelected(Sender: TObject; ARow: TksVListItem; ATime: TDateTime);
    procedure DoItemDateTimeSelected(Sender: TObject; ARow: TksVListItem; ADateTime: TDateTime);
    procedure Loaded; override;
    procedure DoBeforeItemPickerSelected(Sender: TObject; ARow: TksVListItem; var AText: string);
    procedure DoItemPickerSelected(Sender: TObject; ARow: TksVListItem; AText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateAniCalc5(AUpdateLimits: Boolean);
    procedure DoCleanupDeletedItems;
    procedure DoItemSwiped(AItem: TksVListItem;
      ASwipeDirection: TksVListSwipeDirection);
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure ClearItems;
    procedure Invalidate;
    procedure DeselectAll;
    procedure ScrollTo(const Value: integer);
    procedure ScrollToBottom(AAnimated: Boolean);
    procedure ScrollToFirstChecked;
    procedure ScrollToItem(AItem: TksVListItem);
    procedure SetItemImage(AImageID: string; ABmp: TBitmap);

    procedure UpdateScrollLimmits;
    procedure CheckAll;
    procedure UncheckAll;
    procedure SwipeItem(AItem: TksVListItem; ASwipeDirection: TksVListSwipeDirection);
    procedure SelectItem(AItem: TksVListItem);
    property Items: TksVListItemList read FItems;
    property ScrollPos: integer read FScrollPos write SetScrollPos;
    property Viewport: TRectF read GetViewport;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property IsEmpty: Boolean read GetIsEmpty;
    property TopItem: TksVListItem read GetTopItem;
    property TotalItemHeight: single read FTotalItemHeight;
    property ScrollingDisabled: Boolean read FScrollingDisabled write FScrollingDisabled default False;
    property ShowScrollBar: Boolean read FShowScrollBar write FShowScrollBar default True;
  published
    property Align;
    property Appearence: TksVirtualListViewAppearence read FAppearence write SetAppearence;
    property CheckBoxes: TksVListCheckBoxOptions read FCheckBoxOptions write SetCheckBoxOptions;
    property DeleteButton: TksVListDeleteButton read FDeleteButton write FDeleteButton;
    property Height;

    property ItemHeight: integer read FItemHeight write SetItemHeight default C_VLIST_ITEM_DEFAULT_HEIGHT;
    property HeaderHeight: integer read FHeaderHeight write SetHeaderHeight default C_VLIST_HEADER_DEFAULT_HEIGHT;
    property NoItemsText: TksNoItemsText read FNoItemsText write SetNoItemsText;
    property Opacity;
    property Position;
    property Padding;
    property Margins;
    property PullToRefresh: TksVListPullToRefreshOptions read FPullToRefresh write FPullToRefresh;
    property SelectionOptions: TksVListSelectionOptions read FSelectionOptions write FSelectionOptions;
    property Size;
    property Width;
    property Visible;

    property Ani: TksAniCalc read FAniCalc;

    property CanDeleteItem: TksVListDeletingItemEvent read FCanDeleteItem write FCanDeleteItem;
    property OnItemLongTap: TksVListItemLongTapEvent read FOnItemLongTap write FOnItemLongTap;
    property OnItemClick: TksVListItemClickEvent read FOnItemClick write FOnItemClick;
    property OnItemSwitchClick: TksItemSwitchClicked read FOnItemSwitchClick write FOnItemSwitchClick;
    property OnItemSegmentButtonClick: TksItemSegmentButtonClicked read FOnItemSegmentButtonClick write FOnItemSegmentButtonClick;
    property OnItemSwipe: TksVListItemSwipeEvent read FOnItemSwipe write FOnItemSwipe;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnPullRefresh: TNotifyEvent read FOnPullRefresh write FOnPullRefresh;
    property OnReleaseText: TNotifyEvent read FOnReleaseText write FOnReleaseText;
    property OnItemDeleted: TNotifyEvent read FOnItemDeleted write FOnItemDeleted;
    property OnActionButtonClick: TksItemActionButtonClickEvent read FOnActionButtonClick write FOnActionButtonClick;
    property OnItemEditInput: TksItemEditInputEvent read FOnItemEditInputEvent write FOnItemEditInputEvent;
    property OnItemDateSelected: TksItemDateSelectedEvent read FOnItemDateSelectedEvent write FOnItemDateSelectedEvent;
    property OnItemTimeSelected: TksItemTimeSelectedEvent read FOnItemTimeSelectedEvent write FOnItemTimeSelectedEvent;
    property OnItemDateTimeSelected: TksItemDateTimeSelectedEvent read FOnItemDateTimeSelectedEvent write FOnItemDateTimeSelectedEvent;
    property OnItemPickerSelected: TksItemSelectPickerItemEvent read FOnItemPickerSelectedEvent write FOnItemPickerSelectedEvent;
    property BeforeItemPickerSelected: TksItemBeforeSelectPickerItemEvent read FBeforeItemPickerSelectedEvent write FBeforeItemPickerSelectedEvent;
    property OnGetPickerItems: TksItemGetPickerItemsEvent read FOnGetPickerItemsEvent write FOnGetPickerItemsEvent;
  end;

  // {$R *.dcr}

procedure Register;

implementation

uses SysUtils, Math, System.Math.Vectors, ksPickers, FMX.VirtualKeyboard,
  DateUtils, FMX.Forms, FMX.Ani, FMX.Utils,
  FMX.DialogService, System.Net.HttpClient, System.Threading;

var
  AScreenScale: single;
  ATextLayout: TTextLayout;
  ASwitchSize: TSizeF;
  ASwitchBmp: array[False..True] of TBitmap;


procedure Register;
begin
  RegisterComponents('Pentire FMX', [TksVirtualListView]);
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

procedure ReplaceOpaqueColor(ABmp: TBitmap; Color : TAlphaColor);
var
  AMap: TBitmapData;
  PixelColor: TAlphaColor;
  PixelWhiteColor: TAlphaColor;
  C: PAlphaColorRec;
begin
  TThread.Synchronize(TThread.CurrentThread,procedure
                    var
                    x,y: Integer;
                    begin
                      if (Assigned(ABmp)) then
  begin
    if ABmp.Map(TMapAccess.ReadWrite, AMap) then
    try
      AlphaColorToPixel(Color   , @PixelColor, AMap.PixelFormat);
      AlphaColorToPixel(claWhite, @PixelWhiteColor, AMap.PixelFormat);
      for y := 0 to ABmp.Height - 1 do
      begin
        for x := 0 to ABmp.Width - 1 do
        begin
          C := @PAlphaColorArray(AMap.Data)[y * (AMap.Pitch div 4) + x];
          if (C^.Color<>claWhite) and (C^.A>0) then
            C^.Color := PremultiplyAlpha(MakeColor(PixelColor, C^.A / $FF));
        end;
      end;
    finally
      ABmp.Unmap(AMap);
    end;
  end;
  end);
end;


function CalculateTextHeight(AText: string; AFont: TFont; AWordWrap: Boolean; ATrimming: TTextTrimming;
  const AWidth: single = 0; const APadding: single = 0): single;
var
  APoint: TPointF;
begin
  Result := 0;
  if AText = '' then
    Exit;
  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  APoint.x := MaxSingle;
  if AWidth > 0 then
    APoint.x := AWidth;
  APoint.y := MaxSingle;
  ATextLayout.Font.Assign(AFont);
  ATextLayout.Trimming := ATrimming;
  ATextLayout.MaxSize := APoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Padding.Rect := RectF(APadding, APadding, APadding, APadding);
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.VerticalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
  Result := ATextLayout.Height+(1/GetScreenScale);
end;

function GetColorOrDefault(AColor, ADefaultIfNull: TAlphaColor): TAlphaColor;
begin
  Result := AColor;
  if Result = claNull then
    Result := ADefaultIfNull;
end;

procedure InitializeSwitch(const AForce: Boolean = false); var ASwitch: TSwitch; i: Boolean;
begin
  if not assigned(application.MainForm) then exit;
  if (not AForce) and (not ASwitchSize.IsZero) then exit;

  ASwitch := TSwitch.Create(nil);
  try
    ASwitch.Parent := application.MainForm;
    ASwitch.Visible := False;
    ASwitch.ApplyStyleLookup;
    for i := Low(ASwitchBmp) to High(ASwitchBmp) do
    begin
      ASwitch.IsChecked := i;

      ASwitchSize.Width := ASwitch.Width;
      ASwitchSize.Height := ASwitch.Height;

      ASwitchBmp[i].SetSize(Round(ASwitch.Width*GetScreenScale), Round(ASwitch.Height*GetScreenScale));
      ASwitchBmp[i].Clear(0);
      if ASwitchBmp[i].Canvas.BeginScene(nil) then
      begin
        ASwitch.PaintTo(ASwitchBmp[i].Canvas, RectF(0, 0, ASwitch.Width,
          ASwitch.Height), nil);
        ASwitchBmp[i].Canvas.EndScene;
      end;
    end;
  finally
    ASwitch.DisposeOf;
  end;
end;

function SwitchWidth: single;
begin
  InitializeSwitch;
  Result := ASwitchSize.Width;
end;

function SwitchHeight: single;
begin
  InitializeSwitch;
  Result := ASwitchSize.Height;
end;

procedure SwitchImage(ACanvas: TCanvas; ARect: TRectF; AChecked: Boolean);
var
  ASaveState: TCanvasSaveState;
begin
  InitializeSwitch;
  ASaveState := ACanvas.SaveState;
  try
    ACanvas.DrawBitmap(ASwitchBmp[AChecked], RectF(0, 0, ASwitchBmp[AChecked].Width, ASwitchBmp[AChecked].Height), ARect, 1);
  finally
    ACanvas.RestoreState(ASaveState);
  end;
end;




{ TksVListItem }

function TksVListItem.AddText(x, y: single; AText: string): TksVListItemTextObject;
begin
  Result := AddText(x, y, 0, AText);
end;

function TksVListItem.AddText(x, y, AWidth: single; AText: string): TksVListItemTextObject;
begin
  Result := TksVListItemTextObject.Create(Self);
  Result.FLeft := x;
  Result.FTop := y;
  Result.FWidth := AWidth;
  Result.FAutoSize := AWidth = 0;
  Result.Text := AText;
  FObjects.Add(Result);
end;

function TksVListItem.AddDetailText(y: single;
  AText: string): TksVListItemTextObject;
begin
  Result := AddText(0, y, AText);
  Result.HorzAlign := TAlignment.taRightJustify;
  Result.TextSettings.HorzAlign := TTextAlign.Trailing;
  {$IFDEF IOS}
  Result.TextSettings.FontColor := claDodgerblue;
  {$ELSE}
  Result.TextSettings.FontColor := claGray;
  {$ENDIF}
  Result.Font.Size := 14;
end;

function TksVListItem.AddImage(x, y, AWidth, AHeight: single; ABitmap: TBitmap): TksVListItemImageObject;
begin
  Result := TksVListItemImageObject.Create(Self);
  Result.FLeft := x;
  Result.FTop := y;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Bitmap := ABitmap;
  FObjects.Add(Result);
end;

procedure TksVListItem.AddProgressBar(x, y, w, h: single; AValue, AMax: integer; AFill, ABackground,
  ABorder: TAlphaColor);
var
  ABar: TksVListItemProgressBarObject;
begin
  ABar := TksVListItemProgressBarObject.Create(Self);
  ABar.FLeft := x;
  ABar.FTop := y;
  ABar.Width := w;
  ABar.Height := h;
  ABar.Fill.Color := AFill;
  ABar.Stroke.Color := ABorder;
  ABar.FBackground := ABackground;
  ABar.Value := AValue;
  ABar.Max := AMax;
  FObjects.Add(ABar);
end;

function TksVListItem.AddSegmentButtons(ATitles: array of string; AWidth: Integer; const AHeight: integer = 30): TksVListItemSegmentButtons;
var
  AStr: string;
begin
  Self.CanSelect := False;
  Result := TksVListItemSegmentButtons.Create(Self);
  Result.HideInactiveText := True;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.CornerRadius := 0;
  Result.HorzAlign := TAlignment.taRightJustify;
  Result.FStroke.Color := claBlack;
  Result.FFill.Color := claDodgerblue;
  for AStr in ATitles do
    Result.FCaptions.Add(AStr);
  FObjects.Add(Result);
end;

function TksVListItem.AddSwitch(x, y: single; AChecked: Boolean;
  const AID: string = ''): TksVListItemSwitchObject;
begin
  Result := TksVListItemSwitchObject.Create(Self);
  Result.HorzAlign := TAlignment.taRightJustify;
  Result.FLeft := x;
  Result.FTop := y;
  Result.ID := AID;
  Result.FChecked := AChecked;
  FCanSelect := False;
  FObjects.Add(Result);
end;

function TksVListItem.AddText(x, y: single; AText: string;
  AFontColor: TAlphaColor; AFontSize: integer): TksVListItemTextObject;
begin
  Result := AddText(x, y, AText);
  Result.TextSettings.FontColor := AFontColor;
  Result.TextSettings.Font.Size := AFontSize;
end;

procedure TksVListItem.ClearCache;
begin
  FTitle.ClearCache;
  FQuantity.ClearCache;
  FSubTitle.ClearCache;
  FDetail.ClearCache;
  FChanged := True;
end;

procedure TksVListItem.Changed;
begin
  if FState <> Normal then
    Exit;
  FChanged := True;
  FOwner.Changed(False);
end;

constructor TksVListItem.Create(Owner: TksVListItemList);
begin
  inherited Create; // (nil);
  FActionButtons := TksVListActionButtons.Create(Self);
  FObjects := TksVListObjectList.Create(True);
  FPickerItems := TStringList.Create;
  FUseActionSheet := False;
  FCheckBoxVisible := True;
  FSelectorType := ksSelectorNone;
  FDisableLeftMargin := False;
  FOffset := 0;
  FUpdateCount := 0;
  FAbsoluteIndex := 0;
  FIndex := 0;
  FTagInt := 0;
  FTagStr := '';
  FTagObject := nil;
  FIconSize := 28;
  FOwner := Owner;
  FChecked := False;
  FState := Normal;
  FCanSelect := True;
  FBackground := claNull;
  FSelectedDate := Now;
  FSelectedDateTime := Now;
  FEditFieldKeyboardType := TVirtualKeyboardType.Alphabet;
  //BeginUpdate;
  try
    FHeight := FOwner.FOwner.FItemHeight;

    FImage := TksVListItemImageObject.Create(Self);
    FImage.Width := 32;
    FImage.Height := 32;
    FImage.VertAlign := TVerticalAlignment.taVerticalCenter;

    FTitle := TksVListItemTextObject.Create(Self);
    FTitle.VertAlign := TVerticalAlignment.taVerticalCenter;
    FTitle.Font.Size := 14;

    FQuantity := TksVListItemTextObject.Create(Self);
    FQuantity.VertAlign := TVerticalAlignment.taVerticalCenter;


    FSubTitle := TksVListItemTextObject.Create(Self);
    FSubTitle.VertAlign := TVerticalAlignment.taVerticalCenter;
    FSubTitle.TextSettings.FontColor := claDimgray;
    FSubTitle.Font.Size := 14;

    FDetail := TksVListItemTextObject.Create(Self);
    FDetail.VertAlign := TVerticalAlignment.taVerticalCenter;
    FDetail.HorzAlign := TAlignment.taRightJustify;

    FDetail.TextSettings.HorzAlign := TTextAlign.Trailing;
    {$IFDEF IOS}
    FDetail.TextSettings.FontColor := claDodgerblue;
    {$ELSE}
    FDetail.TextSettings.FontColor := claGray;
    {$ENDIF}

    FDetail.Font.Size := 14;


    FAccessory := TksVListItemAccessoryObject.Create(Self);
    FAccessory.VertAlign := TVerticalAlignment.taVerticalCenter;
    FAccessory.HorzAlign := TAlignment.taRightJustify;
  finally
    //EndUpdate;
  end;
  FChanged := True;
end;

function TksVListItem.CreateAniCalc(AOnChange: TNotifyEvent): TAniCalculations;
begin
  Result := TAniCalculations.Create(nil);
  Result.ViewportPositionF := PointF(FOffset, 0);
  Result.Animation := True;
  Result.Averaging := True;
  Result.Interval := 8;
  Result.OnChanged := AOnChange;
end;

procedure TksVListItem.DeleteItem;
var
  Targets: array of TAniCalculations.TTarget;
begin
  FState := Deleting;
  FreeAndNil(FDeleteCalc);
  FDeleteCalc := CreateAniCalc(DeleteCalcChange);
  FDeleteCalc.ViewportPositionF := PointF(0, FHeight);
  SetLength(Targets, 1);
  Targets[0].TargetType := TAniCalculations.TTargetType.Other;
  Targets[0].Point := TPointD.Create(0, 0);
  FDeleteCalc.SetTargets(Targets);
end;

destructor TksVListItem.Destroy;
begin
  FreeAndNil(FSwipeCalc);
  FreeAndNil(FObjects);
  FreeAndNil(FTitle);
  FreeAndNil(FQuantity);
  FreeAndNil(FSubTitle);
  FreeAndNil(FDetail);
  FreeAndNil(FImage);
  FreeAndNil(FAccessory);
  FreeAndNil(FActionButtons);
  FreeAndNil(FPickerItems);
  FreeAndNil(FData);
  inherited;
end;

procedure TksVListItem.DoClicked(var AHandled: Boolean);
begin
  PickerService.HidePickers;

  if Assigned(FOnClick) then
    FOnClick(Self);
  AHandled := False;
  case FSelectorType of
    ksSelectorEdit: ShowEditInput;
    ksSelectorDate: ShowDatePicker(FSelectedDate);
    ksSelectorTime: ShowTimePicker(FSelectedTime);
    {$IFDEF IOS}
    ksSelectorDateTime: ShowDateTimePicker(FSelectedDateTime);
    {$ENDIF}
    ksSelectorPicker: ShowPicker;
  else
    Exit;
  end;
  AHandled := True;
end;

procedure TksVListItem.DoDatePickerChanged(Sender: TObject; ADate: TDateTime);
begin
  FSelectedDate := ADate;
  FDetail.Text := FormatDateTime('ddd, dd mmm, yyyy', ADate);
  FDetail.ClearCache;
  if Assigned(FOnDateSelected) then
    FOnDateSelected(FOwner.FOwner, Self, ADate);
end;

{$IFDEF IOS}
procedure TksVListItem.DoDateTimePickerChanged(Sender: TObject;
  ADateTime: TDateTime);
begin
  FSelectedDateTime := ADateTime;
  FDetail.Text := FormatDateTime('ddd, dd mmm, yyyy - hh:nn', ADateTime);
  FDetail.ClearCache;
  if Assigned(FOnDateTimeSelected) then
    FOnDateTimeSelected(FOwner.FOwner, Self, ADateTime);
end;
{$ENDIF}


procedure TksVListItem.DoTimePickerChanged(Sender: TObject; ATime: TDateTime);
begin
  FSelectedTime := ATime;
  FDetail.Text := FormatDateTime('hh:nn', ATime);
  FDetail.ClearCache;
  if Assigned(FOnTimeSelected) then
    FOnTimeSelected(FOwner.FOwner, Self, ATime);
end;

procedure TksVListItem.DoItemPickerChanged(Sender: TObject;
  AItem: string; AValueIndex: Integer);
var
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread (
    procedure
    begin
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          if Assigned(FBeforeSelectPickerItem) then
            FBeforeSelectPickerItem(FOwner.FOwner, Self, AItem);

          FSelectedItem := AItem;
          FDetail.Text := AItem;
          FDetail.ClearCache;

          if Assigned(FOnSelectPickerItem) then
            FOnSelectPickerItem(FOwner.FOwner, Self, AItem);

        end
      );
    end
  );
  Thread.start;
end;

procedure TksVListItem.SelectItem(ADeselectAfter: integer);
var
  Thread: TThread;
begin
  if (FPurpose <> None) or (FCanSelect = False) then
    Exit;

  FOwner.FOwner.ItemIndex := FIndex;
  if ADeselectAfter > 0 then
  begin
    Thread := TThread.CreateAnonymousThread (
      procedure
      begin
        Sleep(ADeselectAfter);
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
             Selected := False;
          end
        );
      end
    );
    Thread.start;
  end;
end;

procedure TksVListItem.SetAccessory(const Value: TksVListItemAccessoryObject);
begin
  FAccessory.Assign(Value);
  Changed;
end;

procedure TksVListItem.SetBackground(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    //Changed;
  end;
end;

procedure TksVListItem.SetCanSelect(const Value: Boolean);
begin
  if FCanSelect <> Value then
  begin
    FCanSelect := Value;
    Changed;
  end;
end;

procedure TksVListItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if FOwner.FOwner.CheckBoxes.MakeBold then
    begin
      case Value of
        True: FTitle.Font.Style := [TFontStyle.fsBold];
        False: FTitle.Font.Style := [];
      end;
    end;
    Changed;
  end;
end;

procedure TksVListItem.SetDetail(const Value: TksVListItemTextObject);
begin
  FDetail.Assign(Value);
  Changed;
end;

procedure TksVListItem.SetDisableLeftMargin(const Value: Boolean);
begin
  if FDisableLeftMargin <> Value then
  begin
    FDisableLeftMargin := Value;
    Changed;
  end
end;

procedure TksVListItem.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
    if (FHeight = 0) and (FState = Deleting) then
      FState := Deleted;

  end;
end;

procedure TksVListItem.SetIconSize(const Value: integer);
begin
  if FIconSize <> Value then
  begin
    FIconSize := Value;
    Changed;
  end;
end;

procedure TksVListItem.SetImage(const Value: TksVListItemImageObject);
begin
  FImage.Assign(Value);
  Changed;
end;

procedure TksVListItem.SetItemData(const AIndex: string; const Value: TValue);
begin
  if FData = nil then
    FData := TDictionary<string, TValue>.Create;
  FData.AddOrSetValue(AIndex, Value);
end;

procedure TksVListItem.SetItemFont(AName: string; ASize: integer;
  AColor: TAlphaColor; AStyle: TFontStyles);

  procedure SetFont(AText: TksVListItemTextObject);
  begin
    //
    if AName <> '' then AText.Font.Family := AName;
    if ASize > -1 then AText.Font.Size := ASize;
    if AColor <> claNull then AText.TextSettings.FontColor := AColor;
    AText.Font.Style := AStyle;

  end;
var
  AObj: TksVListItemBaseObject;
  AText: TksVListItemTextObject;
begin
  for AObj in FObjects do
  begin
    if AObj is TksVListItemTextObject then
    begin
      AText := AObj as TksVListItemTextObject;
      SetFont(AText);
    end;
  end;
  SetFont(FTitle);
  SetFont(FQuantity);
  SetFont(FDetail);
  SetFont(FSubTitle);
end;

procedure TksVListItem.SetOffset(const Value: integer);
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    if (FOffset = FActionButtons.TotalWidth) or (FOffset = 0) then
      FState := Normal;

  end;
end;

procedure TksVListItem.SetPurpose(const Value: TksVListItemPurpose);
begin
  if FPurpose <> Value then
  begin
    FPurpose := Value;
    case FPurpose of
      Header    : FHeight := FOwner.FOwner.HeaderHeight;
      None      : FHeight := FOwner.FOwner.ItemHeight;
      Seperator : FHeight := Round((FOwner.FOwner.ItemHeight * 0.8));
    end;
    Changed;
  end;
end;

procedure TksVListItem.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    if FCanSelect = False then
      FSelected := False
    else
      FSelected := Value;
    FObjects.ClearCache;
    FTitle.ClearCache;
    FQuantity.ClearCache;
    FSubTitle.ClearCache;
    FDetail.ClearCache;
    FAccessory.ClearCache;
    Changed;
  end;
end;

procedure TksVListItem.SetSelectedDate(const Value: TDateTime);
begin
  if FSelectedDate <> Value then
  begin
    FSelectedDate := Value;
    if FSelectedDate = -1 then
      FDetail.Text := ''
    else
      FDetail.Text := FormatDateTime('ddd, dd mmm, yyyy', FSelectedDate);
    FDetail.ClearCache;
    Changed;
  end;
end;

procedure TksVListItem.SetSelectedTime(const Value: TDateTime);
begin
  if FSelectedTime <> Value then
  begin
    FSelectedTime := Value;
    if FSelectedTime = -1 then
      FDetail.Text := ''
    else
      FDetail.Text := FormatDateTime('hh:nn', FSelectedTime);
    FDetail.ClearCache;
    Changed;
  end;
end;

procedure TksVListItem.SetSelectorType(const Value: TksVListItemSelectorType);
begin
  if FSelectorType <> Value then
  begin
    FSelectorType := Value;
    if Value = ksSelectorNone then
      FAccessory.AccessoryType := atNone;
  end;
end;

procedure TksVListItem.SetSubTitle(const Value: TksVListItemTextObject);
begin
  FSubTitle.Assign(Value);
  Changed;
end;

procedure TksVListItem.SetTitle(const Value: TksVListItemTextObject);
begin
  FTitle.Assign(Value);
  Changed;
end;

procedure TksVListItem.SetQuantity(const Value: TksVListItemTextObject);
begin
  FQuantity.Assign(Value);
  Changed;
end;

procedure TksVListItem.ShowEditInput;

var
  AStr: string;
begin
  PickerService.HidePickers;
  TDialogService.InputQuery(FTitle.Text, [''], [FDetail.Text],
  procedure(const AResult: TModalResult; const AValues: array of string)
  begin
    if AResult = mrOk then
    begin
      FDetail.Text := AValues[0];
      AStr := AValues[0];
      FDetail.ClearCache;
      Changed;

      {$IFDEF ANDROID}
      if Assigned(FOnEditInput) then
        FOnEditInput(Self, Self, AStr);
      {$ELSE}
      TThread.CreateAnonymousThread(
        procedure
        begin
          Application.processMessages;
          Sleep(100);
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              if Assigned(FOnEditInput) then
                FOnEditInput(Self, Self, AStr);
            end
          );
        end
      ).Start;
      {$ENDIF}
    end;
  end);
end;

procedure TksVListItem.ShowPicker;
var
  AItems: TStrings;
  ASelected: string;
  AIndex: integer;
begin
  PickerService.HidePickers;

  AItems := TStringList.Create;
  try
    ASelected := '';
    AItems.Assign(FPickerItems);

    if Assigned(FOwner.FOwner.OnGetPickerItems) then
      FOwner.FOwner.OnGetPickerItems(FOwner.FOwner, Self, ASelected, AItems);

    if ASelected = '' then
      ASelected := FDetail.Text;
    if AItems.IndexOf(ASelected) > -1 then
      AIndex := AItems.IndexOf(ASelected)
    else
      AIndex := 0;

    if FPickerItems.Count > 0 then
    begin
      if (FDefaultPickerItem <> '') and (FDetail.Text = '') then
      begin
        if (FPickerItems.IndexOf(FDefaultPickerItem) > -1) then
          AIndex := FPickerItems.IndexOf(FDefaultPickerItem);
      end;
    end;

    if FUseActionSheet then
      PickerService.ShowActionSheet(AItems, '', DoItemPickerChanged, nil)
    else
      PickerService.ShowItemPicker(FOwner.FOwner.LocalToScreen(FOwner.FOwner.FMouseDownPos), AItems, '',  AIndex, DoItemPickerChanged);
  finally
    FreeAndNil(AItems);
  end;
end;

procedure TksVListItem.ShowDatePicker(ASelected: TDateTime);
begin
  PickerService.HidePickers;
  PickerService.ShowDatePicker(Screen.MousePos, '', ASelected, DoDatePickerChanged, nil);
end;

{$IFDEF IOS}
procedure TksVListItem.ShowDateTimePicker(ASelected: TDateTime);
begin
  PickerService.HidePickers;
  PickerService.ShowDateTimePicker('', ASelected, DoDateTimePickerChanged);
end;
{$ENDIF}

procedure TksVListItem.ShowTimePicker(ASelected: TDateTime);
begin
  PickerService.HidePickers;
  PickerService.ShowTimePicker('', ASelected, DoTimePickerChanged);
end;

procedure TksVListItem.SlideOut(ADirection: TksVListSwipeDirection);
var
  Targets: array of TAniCalculations.TTarget;

begin
  FState := Sliding;
  FSelected := False;
  FreeAndNil(FSwipeCalc);
  FSwipeCalc := CreateAniCalc(SwipeCalcChange);
  SetLength(Targets, 1);
  Targets[0].TargetType := TAniCalculations.TTargetType.Other;
  if ADirection = ksSwipeFromLeft then
    Targets[0].Point := TPointD.Create(Min(FOffset + FActionButtons.TotalWidth,
      FActionButtons.TotalWidth), 0)
  else
    Targets[0].Point := TPointD.Create(Max(FOffset - FActionButtons.TotalWidth,
      0 - FActionButtons.TotalWidth), 0);
  FSwipeCalc.SetTargets(Targets);
end;

procedure TksVListItem.SlideIn;
var
  Targets: array of TAniCalculations.TTarget;
begin
  FState := Sliding;
  FreeAndNil(FSwipeCalc);
  FSwipeCalc := CreateAniCalc(SwipeCalcChange);
  SetLength(Targets, 1);
  Targets[0].TargetType := TAniCalculations.TTargetType.Other;
  Targets[0].Point := TPointD.Create(0, 0);
  FSwipeCalc.SetTargets(Targets);
end;

procedure TksVListItem.SwipeCalcChange(Sender: TObject);
begin
  Offset := Round(FSwipeCalc.ViewportPosition.x);
  FOwner.FOwner.Invalidate;
end;

procedure TksVListItem.DeleteCalcChange(Sender: TObject);
begin
  if FOwner <> nil then
  begin
    Height := Round(FDeleteCalc.ViewportPosition.y);
    FOwner.UpdateItemRects;
    FOwner.FOwner.Invalidate;
  end;
end;

procedure TksVListItem.UpdateStandardObjectPositions;
begin
  Inc(FUpdateCount);
  try
    FQuantity.FLeft := 0;
    fQuantity.FWidth:=0;
    FQuantity.FTop := 0;
    if (FQuantity.Visible) and (FQuantity.Text <> '') then
     fQuantity.FWidth:=10;
    FTitle.FLeft := fQuantity.FWidth+1;

    FTitle.FTop := 0;
    FTitle.FVertAlign := TVerticalAlignment.taVerticalCenter;


    FQuantity.FVertAlign := TVerticalAlignment.taVerticalCenter;

    if (FSubTitle.Visible) and (FSubTitle.Text <> '') then
    begin
      FTitle.FTop := (0 - (16 / 2)) - GetScreenScale;
      FQuantity.FTop:=FTitle.FTop;
      FSubTitle.FLeft := fQuantity.FWidth+1;
      FSubTitle.FTop := 0;
      if FTitle.Visible then
        FSubTitle.FTop := ((16 / 2)) + GetScreenScale;
      FSubTitle.FVertAlign := TVerticalAlignment.taVerticalCenter;
    end;
    FDetail.FLeft := 0;
    FDetail.FTop := 0;
    FDetail.FVertAlign := TVerticalAlignment.taVerticalCenter;
    FAccessory.FLeft := 0;
    FAccessory.FTop := 0;
    FAccessory.FVertAlign := TVerticalAlignment.taVerticalCenter;

    if FPurpose = Header then
    begin
      FTitle.VertAlign := TVerticalAlignment.taVerticalCenter;
      FTitle.Top := 0;
    end;
  finally
    Dec(FUpdateCount);
  end;
end;

function TksVListItem.AddChatBubble(AText, ASender: string; ALeftAlign: Boolean): TksVListItemBubbleObject;
begin
  Result := TksVListItemBubbleObject.Create(Self);
  Result.FLeft := 6;
  Result.FTop := 12;
  Result.TextSettings.WordWrap := True;
  Result.MaxWidth := Round(FOwner.FOwner.Width * 0.6);
  Result.Width := 0;
  Result.TextSettings.Trimming := TTextTrimming.None;
  Result.Text := AText;
  Result.Sender := ASender;
  Result.VertAlign := TVerticalAlignment.taAlignTop;
  if not ALeftAlign then
  begin
    Result.HorzAlign := TAlignment.taRightJustify;
  end;

  FObjects.Add(Result);
  Height := Round(Result.CalculateSize.Height)+40;
end;

function TksVListItem.DrawRect(x, y, AWidth, AHeight, ACornerRadius: single; AStroke, AFill: TAlphaColor): TksVListItemShapeObject;
begin
  Result := TksVListItemShapeObject.Create(Self);
  Result.FLeft := x;
  Result.FTop := y;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.CornerRadius := ACornerRadius;
  Result.FStroke.Color := AStroke;
  Result.FFill.Color := AFill;
  FObjects.Add(Result);
end;

function TksVListItem.DrawLine(x, y, x2, y2: single; AStroke: TAlphaColor): TksVListItemLineObject;
begin
  Result := TksVListItemLineObject.Create(Self);
  Result.FLeft := x;
  Result.FTop := y;
  Result.Width := x2-x;
  Result.Height := y2-y;
  Result.FStroke.Color := AStroke;
  FObjects.Add(Result);
end;


procedure TksVListItem.DrawToCanvas(ACanvas: TCanvas; AScrollPos: single;
  ADrawToCache: Boolean);

  function GetCheckBoxImage(AChecked: Boolean): TksAccessoryType;
  begin
    Result := TksAccessoryType.atCheckBox;
    if AChecked then
      Result := TksAccessoryType.atCheckBoxChecked;
  end;

var
  ARect: TRectF;
  AInternalRect: TRectF;
  ACheckBoxRect: TRectF;
  ACheckBoxes: TksVListCheckBoxOptions;
  r: TRectF;
  AButtonRect: TRectF;
  ICount: integer;
  AScrollOffset: single;
begin
  if FChanged then
  begin
    UpdateStandardObjectPositions;
    FChanged := False;
  end;

  ARect := FItemRect;

  AScrollOffset := 0 - AScrollPos;

  OffsetRect(ARect, FOffset, AScrollOffset);
  if Purpose = TksVListItemPurpose.Header then
  begin
    if ARect.Top < 0 then
      OffsetRect(ARect, 0, 0-ARect.Top);
  end;
  r := ARect;

  if ADrawToCache then
    OffsetRect(ARect, 0 - ARect.Left, 0 - ARect.Top);

  AInternalRect := ARect;
  if not FDisableLeftMargin then
    AInternalRect.Left := AInternalRect.Left + 8;
  AInternalRect.Right := (AInternalRect.Right - Round(C_VLIST_SCROLLBAR_WIDTH / GetScreenScale));

  ACanvas.Stroke.Kind := TBrushKind.Solid;
  ACanvas.Fill.Kind := TBrushKind.Solid;

  if FOffset <> 0 then
  begin
    AButtonRect := FItemRect;
    OffsetRect(AButtonRect, 0, 0 - AScrollPos);
    if FOffset < 0 then
      AButtonRect.Left := AButtonRect.Right + FOffset
    else
      AButtonRect.Right := AButtonRect.Left + FOffset;
    FActionButtons.DrawToCanvas(ACanvas, AButtonRect);
  end;

  if FPurpose <> Seperator then
  begin
    ACanvas.Fill.Color := GetColorOrDefault(FOwner.FOwner.Appearence.Background, claWhite);

    if FPurpose = TksVListItemPurpose.Header then
      ACanvas.Fill.Color := GetColorOrDefault(FOwner.FOwner.Appearence.HeaderColor, $FFEAEAEA)
    else
    begin
      if FBackground <> claNull then
        ACanvas.Fill.Color := FBackground;
    end;



    if (FSelected) and (FOwner.FOwner.Appearence.SelectedColor <> claNull) then
    begin
      ACanvas.Fill.Color := FOwner.FOwner.Appearence.SelectedColor;
    end;

    ACanvas.Stroke.Color :=  ACanvas.Fill.Color;
    ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);

    ACanvas.DrawRect(ARect, 0, 0, AllCorners, 1);
  end;

  if (FPurpose = TksVListItemPurpose.None) and (FCheckBoxVisible) then
  begin
    ACheckBoxes := FOwner.FOwner.FCheckBoxOptions;
    if ACheckBoxes.Visible then
    begin
      ACheckBoxRect := AInternalRect;
      if ACheckBoxes.Alignment = ksCbLeftAlign then
      begin
        ACheckBoxRect.Right := ACheckBoxRect.Left + 28;
        AInternalRect.Left := AInternalRect.Left + 28;
      end
      else
      begin
        ACheckBoxRect.Left := ACheckBoxRect.Right - 28;
        AInternalRect.Right := AInternalRect.Right - 28;
      end;

      AAccessories.DrawAccessory(ACanvas, ACheckBoxRect, GetCheckBoxImage(FChecked), claDimgray, claNull);
    end;
  end;

  AInternalRect.Left := AInternalRect.Left + 4;
  AInternalRect.Right := AInternalRect.Right - 4;

  if FImage.IsEmpty = False then
  begin
    FImage.DrawToCanvas(ACanvas, AInternalRect);
    AInternalRect.Left := AInternalRect.Left + FImage.Width + 8;
  end;

  if (FAccessory.Visible) and (FPurpose = TksVListItemPurpose.None) then
  begin
    if Accessory.AccessoryType = atCheckBoxChecked then
      Accessory.Bitmap.SaveToFile('C:\Users\Graham\Desktop\acc.bmp');

    Accessory.DrawToCanvas(ACanvas, AInternalRect);
    if FAccessory <> nil then
      AInternalRect.Right := (AInternalRect.Right - (FAccessory.Width+(2*GetScreenScale)));
  end;

  if (FPurpose = TksVListItemPurpose.Header) then // Giacomo
    FTitle.TextSettings.FontColor :=FOwner.FOwner.Appearence.HeaderFontColor;

  Title.DrawToCanvas(ACanvas, AInternalRect);
  Quantity.DrawToCanvas(ACanvas, AInternalRect);
  SubTitle.DrawToCanvas(ACanvas, AInternalRect);
  FDetail.DrawToCanvas(ACanvas, AInternalRect);

  for ICount := 0 to FObjects.Count-1 do
    FObjects[ICount].DrawToCanvas(ACanvas, AInternalRect);

  ACanvas.Stroke.Thickness := 1 / GetScreenScale;
  ACanvas.Stroke.Color := FOwner.FOwner.Appearence.SeparatorColor;
  if ACanvas.Stroke.Color = claNull then
    ACanvas.Stroke.Color := claDarkgray;

  if (FIndex > 0) and (FOwner[FIndex-1].Purpose = Seperator) and (Self.Purpose <> Seperator) then
    ACanvas.DrawLine(PointF(0, ARect.Top-(ACanvas.Stroke.Thickness/2)), PointF(ARect.Width, ARect.Top-(ACanvas.Stroke.Thickness/2)), 1);

  if (FPurpose <> Seperator) and (FIndex < FOwner.Count-1) then
  begin
    if FOwner.Items[FIndex+1].Purpose <> Seperator then
      ACanvas.DrawLine(PointF(0, ARect.Bottom-(ACanvas.Stroke.Thickness/2)), PointF(ARect.Width, ARect.Bottom-(ACanvas.Stroke.Thickness/2)), 1)
    else
      ACanvas.DrawLine(PointF(0, ARect.Bottom), PointF(ARect.Width, ARect.Bottom), 1);
  end;


  if (FPurpose <> Seperator) and (FIndex = FOwner.Count-1) then
    ACanvas.DrawLine(PointF(0, ARect.Bottom), PointF(ARect.Width, ARect.Bottom), 1);

end;

function TksVListItem.GetHasData(const AIndex: string): Boolean;
begin
  Result := (FData <> nil) and FData.ContainsKey(AIndex);
end;

function TksVListItem.GetItemData(const AIndex: string): TValue;
begin
  Result := nil;
  if (FData <> nil) and not FData.TryGetValue(AIndex, Result) then
    Result := TValue.Empty;
end;

function TksVListItem.IsItemVisible(AViewPort: TRectF): Boolean;
var
  r: TRectF;
begin
  Result := IntersectRectF(r, FItemRect, AViewPort);
end;

function TksVListItem.MatchesFilter(AFilter: string): Boolean;
var
  AObj: TksVListItemBaseObject;
begin
  Result := Trim(AFilter) = '';
  if Result then
    Exit;

  Result := (FTitle.MatchesFilter(AFilter)) or
            (FSubTitle.MatchesFilter(AFilter)) or
            (FDetail.MatchesFilter(AFilter));

  if not Result then
  begin
    for AObj in FObjects do
    begin
      if (AObj is TksVListItemTextObject) then
      begin
        if (AObj as TksVListItemTextObject).MatchesFilter(AFilter) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TksVirtualListView.CheckAll;
var
  AItem: TksVListItem;
begin
  for AItem in FItems do
  begin
    if AItem.CheckBoxVisible then
      AItem.Checked := True;
  end;
end;

procedure TksVirtualListView.ClearItems;
begin
  FItems.Clear;
  FAniCalc.UpdatePosImmediately;
end;

constructor TksVirtualListView.Create(AOwner: TComponent);
begin
  inherited;
  TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);

  FAppearence := TksVirtualListViewAppearence.Create(Self);
  FNoItemsText := TksNoItemsText.Create(Self);

  FItemIndex := -1;
  FScrollPos := 0;
  FUpdateCount := 0;
  FScrollingDisabled := False;
  FItemHeight := C_VLIST_ITEM_DEFAULT_HEIGHT;
  FHeaderHeight := C_VLIST_HEADER_DEFAULT_HEIGHT;
  FCheckBoxOptions := TksVListCheckBoxOptions.Create(Self);
  FSelectionOptions := TksVListSelectionOptions.Create(Self);
  FItems := TksVListItemList.Create(Self);
  FScrollBar := TksScrollBar.Create(Self);
  FScrollBar.Stored := False;
  FShowScrollBar := True;

  FPullToRefresh := TksVListPullToRefreshOptions.Create;
  FDeleteButton := TksVListDeleteButton.Create;

  FScrollBar.Width := 0;
  FPendingRefresh := False;

  CreateAniCalc5(False);

  FScrollBar.Orientation := TOrientation.Vertical;
  FScrollBar.OnChange := ScrollBarChanged;
  AddObject(FScrollBar);

  HitTest := True;
  FRefreshing := False;
  if (csDesigning in ComponentState) then
  begin
    Items.Add('Title 1', 'sub title', 'detail');
    Items.Add('Title 2', 'sub title', 'detail');
    Items.Add('Title 3', 'sub title', 'detail');
  end;
  FOnReleaseIsFiring := false;
end;

procedure TksVirtualListView.CreateAniCalc5(AUpdateLimits: Boolean);
begin
  FreeAndNil(FAniCalc);
  FAniCalc := TksAniCalc.Create(nil);
  FAniCalc.OnChanged := AniCalcChange;
  FAniCalc.ViewportPositionF := PointF(0, FScrollPos);
  FAniCalc.UpdatePosImmediately;
  FAniCalc.Animation := True;
  FAniCalc.Averaging := True;
  FAniCalc.Interval := 8;
  FAniCalc.BoundsAnimation := True;
  FAniCalc.TouchTracking := [ttVertical];
  FAniCalc.OnChanged := AniCalcChange;
  FAniCalc.OnStart := AniCalcStart;
  FAniCalc.OnStop := AniCalcStop;
  if AUpdateLimits then
    UpdateScrollLimmits;
end;

destructor TksVirtualListView.Destroy;
begin
  FreeAndNil(FAniCalc);
  FreeAndNil(FItems);
  FreeAndNil(FCheckBoxOptions);
  FreeAndNil(FSelectionOptions);
  FreeAndNil(FPullToRefresh);
  FreeAndNil(FDeleteButton);
{$IFDEF NEXTGEN}
  FScrollBar.DisposeOf;
{$ELSE}
  FScrollBar.Free;
{$ENDIF}
  FreeAndNil(FNoItemsText);
  FreeAndNil(FAppearence);
  inherited;
end;

procedure TksVirtualListView.DoCleanupDeletedItems;
var
  ICount: integer;
begin
  for ICount := Items.Count - 1 downto 0 do
  begin
    if Items[ICount].State = Deleted then
      Items.Delete(ICount, False);
  end;
end;

procedure TksVirtualListView.DoItemClicked(AItem: TksVListItem;
  ACallClickEvent: Boolean);
var
  AHandled: Boolean;
begin
  if AItem = nil then
    Exit;

  if (AItem.Purpose = TksVListItemPurpose.Header) or (AItem.Purpose = TksVListItemPurpose.Seperator) then
    Exit;

  if FCheckBoxOptions.Visible then
  begin
    if AItem.CheckBoxVisible then
    begin
      if FCheckBoxOptions.FMode = ksSingleSelect then
        UncheckAll;
      AItem.Checked := not AItem.Checked;
    end;
  end;

  SelectItem(AItem);
  Invalidate;

  Application.ProcessMessages;

  //FMouseDownItem := nil;
  AItem.DoClicked(AHandled);

  if AHandled = False then
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        Sleep(100);
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
              if Assigned(FOnItemClick) then
                FOnItemClick(Self, AItem);
          end
        );
      end
    ).Start;
  end;
end;

procedure TksVirtualListView.DoItemDateSelected(Sender: TObject;
  ARow: TksVListItem; ADate: TDateTime);
begin
  if Assigned(FOnItemDateSelectedEvent) then
    FOnItemDateSelectedEvent(Self, ARow, ADate);
  {$IFDEF ANDROID}
  Application.ProcessMessages;
  Repaint;
  {$ENDIF}
end;

procedure TksVirtualListView.DoItemDateTimeSelected(Sender: TObject;
  ARow: TksVListItem; ADateTime: TDateTime);
begin
  if Assigned(FOnItemDateTimeSelectedEvent) then
    FOnItemDateTimeSelectedEvent(Self, ARow, ADateTime);
  {$IFDEF ANDROID}
  Application.ProcessMessages;
  Repaint;
  {$ENDIF}
end;

procedure TksVirtualListView.DoItemDeleted;
begin
  if Assigned(FOnItemDeleted) then
    FOnItemDeleted(Self);
end;

procedure TksVirtualListView.DoItemEditInput(Sender: TObject;
  ARow: TksVListItem; AText: string);
begin
  if Assigned(FOnItemEditInputEvent) then
    FOnItemEditInputEvent(Self, ARow, AText);
end;

procedure TksVirtualListView.DoItemSwiped(AItem: TksVListItem;
  ASwipeDirection: TksVListSwipeDirection);
var
  ADeleteIcon: TksAccessoryType;
  ADeleteBtn: TksVListActionButton;
begin
  if AItem.Purpose <> None then
    Exit;

  if AItem.FOffset <> 0 then
  begin
    AItem.SlideIn;
    Exit;
  end;

  FAniCalc.UpdatePosImmediately;
  AItem.FActionButtons.Clear;
  if Assigned(FOnItemSwipe) then
    FOnItemSwipe(Self, AItem, ASwipeDirection, AItem.FActionButtons);
  if ASwipeDirection = ksSwipeFromRight then
  begin
    if FDeleteButton.Enabled then
    begin
      ADeleteIcon := atNone;
      if FDeleteButton.ShowImage then
        ADeleteIcon := atTrash;
      ADeleteBtn := AItem.FActionButtons.AddButton(FDeleteButton.FText,
        FDeleteButton.Color, FDeleteButton.TextColor, ADeleteIcon, FDeleteButton.Width);
      ADeleteBtn.IsDeleteButton := True;

    end;
  end;
  if AItem.FActionButtons.Count = 0 then
    Exit;
  AItem.SlideOut(ASwipeDirection);
end;

procedure TksVirtualListView.DoItemTimeSelected(Sender: TObject;
  ARow: TksVListItem; ATime: TDateTime);
begin
  if Assigned(FOnItemTimeSelectedEvent) then
    FOnItemTimeSelectedEvent(Self, ARow, ATime);
  {$IFDEF ANDROID}
  Application.ProcessMessages;
  Repaint;
  {$ENDIF}
end;

procedure TksVirtualListView.DoMouseLeave;
begin
  if (FAniCalc <> nil) then
    FAniCalc.MouseLeave;
  inherited DoMouseLeave;
end;

procedure TksVirtualListView.DrawPullToRefresh;
var
  ARefreshArea: TRectF;
  AText: string;
  AState: TCanvasSaveState;
begin
  try
    if (ScrollPos < 0) and (FPullToRefresh.Enabled) then
    begin
      Canvas.Stroke.Color := claGainsboro;
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.DrawLine(PointF(0, 0 - FScrollPos), PointF(Width, 0 - FScrollPos), 1);

      ARefreshArea := RectF(0, 0, Width, 0 - ScrollPos);
      AState := Canvas.SaveState;
      try
        Canvas.IntersectClipRect(ARefreshArea);
        Canvas.Fill.Color := claDimgray;
        Canvas.Font.Size := 14;
        Canvas.Fill.Kind := TBrushKind.Solid;

        AText := FPullToRefresh.PullText;
        if FPendingRefresh then
        begin
          AText := FPullToRefresh.ReleaseText;
          if Assigned(FOnReleaseText) and (not FOnReleaseIsFiring) then
          begin
            FOnReleaseText(self);
            FOnReleaseIsFiring := true;
          end;
        end
        else
        begin
          FOnReleaseIsFiring := false;
        end;
        Canvas.FillText(RectF(0, 0, Width, 50), AText, False, 1, [], TTextAlign.Center, TTextAlign.Center);
      finally
        Canvas.RestoreState(AState);
      end;
    end;
  except
    //
  end;
end;

procedure TksVirtualListView.DoItemLongTap(AItem: TksVListItem);
begin
  if Assigned(FOnItemLongTap) then
    FOnItemLongTap(Self, AItem);
end;



procedure TksVirtualListView.DoBeforeItemPickerSelected(Sender: TObject;
  ARow: TksVListItem; var AText: string);
begin
  if Assigned(FBeforeItemPickerSelectedEvent) then
    FBeforeItemPickerSelectedEvent(Self, ARow, AText);
end;


procedure TksVirtualListView.DoItemPickerSelected(Sender: TObject;
  ARow: TksVListItem; AText: string);
begin
  if Assigned(FOnItemPickerSelectedEvent) then
    FOnItemPickerSelectedEvent(Self, ARow, AText);
end;

procedure TksVirtualListView.Loaded;
begin
  inherited;
end;

procedure TksVirtualListView.LongTapTimerProc;
var
  AItem: TksVListItem;
begin
  if FLongTapTimer = 0 then
    Exit;
  KillTimer(FLongTapTimer);

  if FAniCalc = nil then
    Exit;
  if (FAniCalc.Down) then
  begin
    if (FMousePt.y > FMouseDownPos.y - 4) and (FMousePt.y < FMouseDownPos.y + 4)
    then
    begin
      AItem := FItems.ItemAtPos(FMousePt.x, FMousePt.y);
      DoItemClicked(AItem, False);
    end;
  end;
end;

procedure TksVirtualListView.DeselectAll;
var
  ICount: integer;
begin
  for ICount := 0 to Items.Count - 1 do
    Items[ICount].Selected := False;
end;

procedure TksVirtualListView.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    inherited EndUpdate;
    FItems.UpdateItemRects;
    UpdateScrollLimmits;
    Repaint;
  end;
end;

procedure TksVirtualListView.FocusControl(AItem: TksVListItem; AControl: TControl);
begin
  if FFocusedControl = AControl then
    Exit;
  UnfocusControl;
  FFocusedControl := AControl;
  AControl.Width := 150;
  AControl.Position.X := (Width - AControl.Width) - 30;
  AControl.Position.Y := (((AItem.ItemRect.Top + AItem.ItemRect.Bottom) - AControl.Height) / 2) - ScrollPos;
  AddObject(AControl);
  AControl.SetFocus;
  TEdit(AControl).SelStart := Length(TEdit(AControl).Text);
end;

function TksVirtualListView.GetIsEmpty: Boolean;
begin
  Result := FItems.Count = 0;
end;

function TksVirtualListView.GetTopItem: TksVListItem;
begin
  Result := nil;
  if FItems.Count > 0 then
    Result := FItems.Items[0];
end;

function TksVirtualListView.GetViewport: TRectF;
begin
  Result := RectF(0, 0, Width, Height);
  OffsetRect(Result, 0, FScrollPos);
end;

procedure TksVirtualListView.Invalidate;
begin
  InvalidateRect(ClipRect);
end;

procedure TksVirtualListView.AniCalcStart(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);
end;

procedure TksVirtualListView.AniCalcChange(Sender: TObject);
begin
  if FScrollingDisabled then
    Exit;
  ScrollPos := Round(FAniCalc.ViewportPosition.y);
end;

procedure TksVirtualListView.AniCalcStop(Sender: TObject);
begin
  TAnimator.AnimateFloat(FScrollBar, 'Opacity', 0);
  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;

procedure TksVirtualListView.BeginUpdate;
begin
  if FUpdateCount = 0 then
    inherited BeginUpdate;
  Inc(FUpdateCount);
end;

procedure TksVirtualListView.Paint;
var
  ICount: integer;
  AState: TCanvasSaveState;
  AItem: TksVListItem;
  AViewPort: TRectF;
  ATopItem: integer;
begin
  try
    if (FUpdateCount > 0) or (Locked) then
      Exit;

    AViewPort := Viewport;

    if (csDesigning in ComponentState) then
      DrawDesignBorder(claDimgray, claDimgray);


    AState := Canvas.SaveState;
    try
      Canvas.IntersectClipRect(ClipRect);
      if FAppearence.Background <> claNull then
        Canvas.Clear(FAppearence.Background);

      if FItems.Count = 0 then
        FNoItemsText.DrawToCanvas(Canvas, ClipRect);

      if FUpdateCount > 0 then
        Exit;

      DrawPullToRefresh;

      ATopItem := -1;

      for ICount := Items.Count - 1 downto 0 do
        if Items[ICount].FState = Deleted then
          Items.Delete(ICount, False);

      for ICount := 0 to Items.Count - 1 do
      begin
        AItem := Items[ICount];
        if (AItem.IsItemVisible(AViewPort)) and (AItem.Purpose <> TksVListItemPurpose.Header) then
        begin
          if ATopItem = -1 then
            ATopItem := ICount;
          AItem.DrawToCanvas(Canvas, Trunc(FScrollPos), False);
        end;
      end;

      // draw the headers...
      for ICount := 0 to Items.Count - 1 do
      begin
        AItem := Items[ICount];
        if (AItem.Purpose = TksVListItemPurpose.Header) then
        begin

          AItem.DrawToCanvas(Canvas, Trunc(FScrollPos), False);
        end
      end;

    finally
      Canvas.RestoreState(AState);
    end;
  except
    //
  end;
end;

procedure TksVirtualListView.ResetItemOffsets(AIgnore: TksVListItem);
var
  ICount: integer;
begin
  for ICount := FItems.Count - 1 downto 0 do
  begin
    if FItems[ICount].FOffset <> 0 then
    begin
      if FItems[ICount] <> AIgnore then
        FItems[ICount].SlideIn;
    end;
  end;
end;

procedure TksVirtualListView.Resize;
begin
  inherited;
  FItems.ClearCache;

  FItems.UpdateItemRects;
  UpdateScrollLimmits;
  InvalidateRect(ClipRect);
  if FScrollBar.Visible <> FShowScrollBar then
    FScrollBar.Visible := FShowScrollBar;
end;

procedure TksVirtualListView.ScrollBarChanged(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ScrollPos := Round(FScrollBar.Value);
  FAniCalc.ViewportPositionF := PointF(0, FScrollPos);
  FAniCalc.UpdatePosImmediately;
{$ENDIF}
end;

procedure TksVirtualListView.ScrollTo(const Value: integer);
var
  ANewValue: integer;
begin
  ANewValue := Value;
  if ANewValue < 0 then
    ANewValue := 0;
  if ANewValue > FMaxScrollPos then
    ANewValue := FMaxScrollPos;
  if ((ANewValue - Height) < FMaxScrollPos) and (ANewValue >= 0) then
  begin
    ScrollPos := ANewValue;
    UpdateScrollLimmits;
    FAniCalc.UpdatePosImmediately;
    Invalidate;
  end;
end;

procedure TksVirtualListView.ScrollToBottom(AAnimated: Boolean);
begin
  //Application.ProcessMessages;
  if AAnimated then
    TAnimator.AnimateIntWait(Self, 'ScrollPos', FMaxScrollPos)
  else
    ScrollPos := FMaxScrollPos;
  FAniCalc.ViewportPositionF := PointF(0, FMaxScrollPos);
 FAniCalc.UpdatePosImmediately;
 Application.ProcessMessages;
end;

procedure TksVirtualListView.ScrollToFirstChecked;
var
  APos: integer;
begin
  APos := 0;
  if FItems.GetCheckedCount > 0 then
    APos := Round(FItems.GetFirstChecked.ItemRect.Top)-(FItemHeight*3);

  ScrollTo(APos);
end;

procedure TksVirtualListView.ScrollToItem(AItem: TksVListItem);
var
  i: TksVListItem;
  APos: integer;
begin
  FItems.UpdateItemRects;
  APos := 0;
  for i in FItems do
  begin
    if i = AItem  then
      Break;
    APos := Trunc(i.ItemRect.Top);
  end;
  ScrollTo(APos);
end;

procedure TksVirtualListView.SelectItem(AItem: TksVListItem);
begin
  if FSelectionOptions.SelectionType = ksSingleSelect then
  begin
    if FAniCalc.Down then
      AItem.SelectItem(0)
    else
    begin
      case (FSelectionOptions.KeepSelection) of
        True: AItem.SelectItem(0);
        False: AItem.SelectItem(100);
      end;
    end
  end
  else
  begin
    AItem.Selected := not AItem.Selected;
  end;
end;

procedure TksVirtualListView.SetAppearence(const Value: TksVirtualListViewAppearence);
begin
  FAppearence.Assign(Value);
end;

procedure TksVirtualListView.SetCheckBoxOptions(const Value
  : TksVListCheckBoxOptions);
begin
  FCheckBoxOptions := Value;
end;

procedure TksVirtualListView.SetHeaderHeight(const Value: integer);
var
  ICount: integer;
begin
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    for ICount := 0 to Items.Count - 1 do
      if Items[ICount].Purpose = TksVListItemPurpose.Header then
        Items[ICount].FHeight := Value;
    Items.UpdateItemRects;
    Invalidate;
  end;
end;

procedure TksVirtualListView.SetItemHeight(const Value: integer);
var
  ICount: integer;
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    for ICount := 0 to Items.Count - 1 do
      if Items[ICount].Purpose = TksVListItemPurpose.None then
      Items[ICount].FHeight := Value;
    Items.UpdateItemRects;
    Invalidate;
  end;
end;

procedure TksVirtualListView.SetItemImage(AImageID: string; ABmp: TBitmap);
var
  AItem: TksVListitem;
begin
  for AItem in ITems do
  begin
    if AItem.Image.ID = AImageID then
    begin
      AItem.Image.Bitmap := ABmp;
      AItem.Changed;
    end;
  end;
end;

procedure TksVirtualListView.SetItemIndex(const Value: integer);
var
  ICount: integer;
begin
  FItemIndex := Value;
  for ICount := 0 to FItems.Count-1 do
    Items[ICount].Selected := ICount = FItemIndex;
  Invalidate;
end;

procedure TksVirtualListView.SetNoItemsText(const Value: TksNoItemsText);
begin
  FNoItemsText.Assign(Value);
end;

procedure TksVirtualListView.SetScrollPos(const Value: integer);
begin
  try
    if Round(Value) <> Round(FScrollPos) then
    begin
      UnfocusControl;
      PickerService.HidePickers;

      FItems.UpdateItemRects;
      FScrollBar.Opacity := 1;
      FScrollPos := Value;
      FScrollBar.Visible := FShowScrollBar;

      InvalidateRect(ClipRect);
      if Assigned(FOnScroll) then
        FOnScroll(Self);
      FScrollBar.OnChange := nil;
      FScrollBar.Value := Value;

      if value = 0 then
        FAniCalc.UpdatePosImmediately;

      if (value = 0) and (FPendingRefresh) then
      begin
        FAniCalc.UpdatePosImmediately;
        if FPullToRefresh.Enabled  then
        begin
          if Assigned(FOnPullRefresh) then
            FOnPullRefresh(Self);
        end;
      end;
     FScrollBar.OnChange := ScrollBarChanged;
    end;
  except
    //
  end;
end;

procedure TksVirtualListView.SwipeItem(AItem: TksVListItem;
  ASwipeDirection: TksVListSwipeDirection);
begin
  DoItemSwiped(AItem, ASwipeDirection);
end;



procedure TksVirtualListView.UncheckAll;
var
  AItem: TksVListItem;
begin
  for AItem in FItems do
    AItem.Checked := False;
end;

procedure TksVirtualListView.HideKeyboard;
var
  KeyboardService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(KeyboardService)) then
    KeyboardService.HideVirtualKeyboard;
end;

procedure TksVirtualListView.UnfocusControl;
begin
  if FFocusedControl <> nil then
  begin
    FFocusedControl.Root.SetFocused(nil);
    RemoveObject(FFocusedControl);
    FFocusedControl := nil;
    HideKeyboard;
    //if FFilterEdit <> nil then
    //  FFilterEdit.Unfocus;
  end;
end;

procedure TksVirtualListView.UpdateScrollLimmits;
var
  Targets: array of TAniCalculations.TTarget;
begin
  if FAniCalc <> nil then
  begin
    FAniCalc.OnStop := nil;
    FAniCalc.OnChanged := nil;
    SetLength(Targets, 2);
    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);
    Targets[1].TargetType := TAniCalculations.TTargetType.Max;

    FMaxScrollPos := Round(Max((FTotalItemHeight - Height), 0));
    Targets[1].Point := TPointD.Create(100, FMaxScrollPos);
    FAniCalc.SetTargets(Targets);

    FAniCalc.ViewportPosition := PointF(0, FScrollPos);
    FAniCalc.OnChanged := AniCalcChange;
    FAniCalc.OnStop := AniCalcStop;
  end;
  FScrollBar.Max := FTotalItemHeight;
  FScrollBar.ViewportSize := Height;
  FScrollBar.Position.y := 0;
  FScrollBar.Position.x := Width - FScrollBar.Width;
  FScrollBar.Height := Height;
  FScrollBar.Visible := FTotalItemHeight > Height;
end;

function TksVirtualListView.CreateTimer(AInterval: integer; AProc: TTimerProc)
  : TFmxHandle;
begin
  Result := 0;
  if FTimerService <> nil then
    Result := FTimerService.CreateTimer(AInterval, AProc);
end;

procedure TksVirtualListView.KillTimer(var ATimer: TFmxHandle);
begin
  if FTimerService <> nil then
  begin
    if (ATimer <> 0) then
    begin
      try
        FTimerService.DestroyTimer(ATimer);
      except
      end;
      ATimer := 0;
    end;

  end;
end;

procedure TksVirtualListView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  ABtn: TksVListActionButton;
  ACanDelete: Boolean;
begin
  inherited;
  try
    Root.SetFocused(nil);
    if FScrollPos < 0 then
      Exit;


    KillTimer(FLongTapTimer);
    HideKeyboard;
    UnfocusControl;
    //if FFilterEdit <> nil then
    //  FFilterEdit.Unfocus;
    FMouseDownTime := Now;
    FMouseDownPos := PointF(x, y);
    if not FScrollingDisabled then
      FAniCalc.MouseDown(x, y);
    FMouseDownItem := FItems.ItemAtPos(x, y);

    if (FMouseDownItem <> nil) and (FMouseDownItem.FOffset <> 0) then
    begin
      ABtn := FMouseDownItem.FActionButtons.ButtonAtXY(x, y);
      if ABtn <> nil then
      begin
        FAniCalc.MouseUp(x, y);
        if ABtn.IsDeleteButton then
        begin
          ACanDelete := True;
          if Assigned(FCanDeleteItem) then
            FCanDeleteItem(Self, FMouseDownItem, ACanDelete);
          if ACanDelete then
          begin
            FMouseDownItem.DeleteItem;
            Exit;
          end;
        end
        else
        begin

          if Assigned(FOnActionButtonClick) then
            FOnActionButtonClick(Self, FMouseDownItem, ABtn);
        end;
      end;
      FMouseDownItem.SlideIn;
      FMouseDownItem := nil;
      FAniCalc.MouseLeave;
      Exit;
    end;

    ResetItemOffsets(nil);

    if FMouseDownItem <> nil then
      FLongTapTimer := CreateTimer(C_LONG_TAP_DURATION, LongTapTimerProc)
  except
    //on E:Exception do
    //  raise Exception.Create('MouseDown - '+E.Message);
  end;
end;

procedure TksVirtualListView.MouseMove(Shift: TShiftState; x, y: single);
begin
  try
    FMousePt := PointF(x, y);
    if FAniCalc.Down then
      FAniCalc.MouseMove(x, y);
    if (ssLeft in Shift) then
      FPendingRefresh := ((ScrollPos <= -50) and (FAniCalc.Down));
    if (FAniCalc.Down) and (FMouseDownPos.y <> y) then
    begin
      if FSelectionOptions.KeepSelection = False then
        DeselectAll;
    end;
  except
    //on E:Exception do
    //  raise Exception.Create('MouseMove - '+E.Message);
  end;
end;

procedure TksVirtualListView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  x, y: single);
var
  ATapRect: TRectF;
  ASwipeRect: TRectF;
  ATapDuration: Int64;
  AItem: TksVListItem;
  ASwipeDirection: TksVListSwipeDirection;
  ADidSwipe: Boolean;
  AObj: TksVListItemBaseObject;
begin
  try
    inherited;
    if FMouseDownItem <> nil then
    begin
      if (FMouseDownItem.State in [Deleting, Deleted, Sliding]) then
      begin
        Exit;
      end;
    end;

    // check for quick tap (within 300 ms)
    FAniCalc.MouseUp(x, y);

    if (FMouseDownItem <> nil) and (FMouseDownItem.FOffset <> 0) then
    begin
      if (FMouseDownItem.State in [Deleting, Deleted]) then
        Exit;
      // check for action button tap...
      if FMouseDownItem.State = Normal then
      begin
        FMouseDownItem.SlideIn;
        Exit;
      end;
    end;

    ATapRect := RectF(FMouseDownPos.x - 8, FMouseDownPos.y - 8,
      FMouseDownPos.x + 8, FMouseDownPos.y + 8);
    ASwipeRect := RectF(0, FMouseDownPos.y - 32, Width, FMouseDownPos.y + 32);
    ATapDuration := MilliSecondsBetween(FMouseDownTime, Now);

    AItem := FMouseDownItem;
    if AItem <> nil then
    begin
      // swipe...
      ADidSwipe := False;
      if PtInRect(ASwipeRect, PointF(x, y)) then
      begin

        if ATapDuration <= C_LONG_TAP_DURATION then
        begin
          // swipe
          if (x < FMouseDownPos.x - 16) or (x > FMouseDownPos.x + 16) then
          begin
            if x < (FMouseDownPos.x) then
              ASwipeDirection := ksSwipeFromRight
            else
              ASwipeDirection := ksSwipeFromLeft;

            DoItemSwiped(AItem, ASwipeDirection);
            FMouseDownItem := nil;
            Exit;
          end;
        end;
      end;

      // tap and long tap
      if (PtInRect(ATapRect, PointF(x, y))) and (ADidSwipe = False) then
      begin
        if ATapDuration <= C_LONG_TAP_DURATION then
        begin
          // tap
          DoItemClicked(AItem, True);
        end
        else
        begin
          // long tap
          DoItemLongTap(AItem);
          if FSelectionOptions.FKeepSelection = False then
            FMouseDownItem.Selected := False;
        end;
      end;

    end;

    if FMouseDownItem <> nil then
    begin
      AObj := nil;
      if (y > (FMouseDownPos.Y-8)) and (y < (FMouseDownPos.Y+8))  then
      begin
        if FMouseDownItem.Objects <> nil then
          AObj := FMouseDownItem.Objects.ObjectAtPos(FMouseDownItem, x, y);
        if AObj <> nil then
          AObj.Clicked(x-AObj.FObjectRect.Left, y-AObj.FObjectRect.Top);
      end;

    end;
  except
    //on E:Exception do
    //  raise Exception.Create('MouseUp - '+E.Message);
  end;

end;

procedure TksVirtualListView.MouseWheel(Shift: TShiftState; WheelDelta: integer;
  var Handled: Boolean);
var
  Offset: single;
  ANewPos: single;
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  if (not Handled) then
  begin
    if not(ssHorizontal in Shift) then
    begin
      FAniCalc.UpdatePosImmediately;
      Offset := Height / 14;
      Offset := Offset * -1 * (WheelDelta / 120);
      ANewPos := Max(FScrollPos + Offset, 0);
      ANewPos := Min(ANewPos, (Max((FTotalItemHeight - Height), 0)));
      ScrollTo(Floor(ANewPos));
      Handled := True;
    end
  end;
end;

{ TksVListItemList }


function TksVListItemList.Add: TksVListItem;
begin
  Result := TksVListItem.Create(Self);
  Add(Result);
  Result.Background := FOwner.Appearence.ItemBackground;
  Result.OnEditInput := FOwner.DoItemEditInput;
  Result.OnDateSelected := FOwner.DoItemDateSelected;
  Result.OnDateTimeSelected := FOwner.DoItemDateTimeSelected;
  Result.OnTimeSelected := FOwner.DoItemTimeSelected;
  Result.BeforeSelectPickerItem := FOwner.DoBeforeItemPickerSelected;
  Result.OnSelectPickerItem := FOwner.DoItemPickerSelected;
  Changed(True);
end;

function TksVListItemList.Add(ATitle, ASubTitle, ADetail: string;
  const AAccessory: TksAccessoryType = atNone): TksVListItem;
begin
  Result := Add;
  Result.Title.Text := ATitle;
  //Result.Title.Font.Size := 14;
  Result.SubTitle.Text := ASubTitle;
  Result.SubTitle.Font.Size := 14;
  Result.Quantity.Text := '';
  Result.Quantity.Font.Size := 14;

  Result.Detail.Text := ADetail;
  Result.Detail.Font.Size := 14;
  Result.Accessory.AccessoryType := AAccessory;
  Changed(True);
end;

function TksVListItemList.Add(ATitle, ASubTitle, ADetail,AQuantity: string; const AAccessory: TksAccessoryType = atNone): TksVListItem;
begin
 Result := Add(ATitle, ASubtitle, ADetail, AAccessory);
 result.Quantity.Text:=AQuantity;
end;

function TksVListItemList.Add(ATitle, ASubTitle, ADetail: string; AImage: TBitmap; const AAccessory: TksAccessoryType = atNone): TksVListItem;
begin
  Result := Add(ATitle, ASubtitle, ADetail, AAccessory);
  Result.Image.Bitmap := AImage;
end;

function TksVListItemList.AddChatBubble(AText, ASender: string; AColor, ATextColor: TAlphaColor; ALeftAlign: Boolean): TksVListItem;
begin
  Result := Add;
  with Result.AddChatBubble(AText, ASender, not ALeftAlign) do
  begin
    FColor := AColor;
    FTextColor := ATextColor;
  end;
end;

function TksVListItemList.AddHeader(AText: string): TksVListItem;
begin
  Result := Add(AText, '', '');
  Result.Background := GetColorOrDefault(FOwner.Appearence.HeaderColor, claNull);
  Result.Title.Font.Size := 12;
  Result.Title.TextSettings.FontColor := claBlack;
  Result.Detail.Font.Size := 12;
  Result.Detail.TextSettings.FontColor := claDimgray;
  Result.Purpose := Header;
  Result.CanSelect := False;
  Result.Title.VertAlign := TVerticalAlignment.taAlignBottom;
end;

function TksVListItemList.InsertHeader(AIndex: integer;AText: string): TksVListItem;
begin
  Result := insert(Aindex,AText, '', '','');
  Result.Background := GetColorOrDefault(FOwner.Appearence.HeaderColor, claNull);
  Result.Title.Font.Size := 13;
  Result.Title.TextSettings.FontColor := claBlack;
  Result.Detail.Font.Size := 12;
  Result.Detail.TextSettings.FontColor := claDimgray;
  Result.Purpose := Header;
  Result.CanSelect := False;
  Result.Title.VertAlign := TVerticalAlignment.taAlignBottom;
end;

function TksVListItemList.AddInputSelector(ATitle, ASubTitle, ADetail,
  ATagStr: string): TksVListItem;
begin
  Result := Add(ATitle, ASubTitle, ADetail, nil, atMore);
  Result.SelectorType := TksVListItemSelectorType.ksSelectorEdit;
  Result.TagStr := ATagStr;
end;

function TksVListItemList.AddPickerSelector(ATitle, ASubTitle, ADetail: string;
  AImage: TBitmap; ATagStr: string; AItems: TStrings; const ADefaultItem: string = ''; const AUseActionSheet: Boolean = False): TksVListItem;
var
  ICount: integer;
begin
  Result := Add(ATitle, ASubTitle, ADetail, AImage, atMore);
  if AItems <> nil then
  begin
    for ICount := 0 to AItems.Count-1 do
      Result.PickerItems.Add(AItems[ICount]);
  end;
  Result.FDefaultPickerItem := ADefaultItem;
  Result.FUseActionSheet := AUseActionSheet;
  Result.SelectorType := TksVListItemSelectorType.ksSelectorPicker;
  Result.TagStr := ATagStr;
end;

function TksVListItemList.AddPickerSelector(ATitle, ASubTitle, ADetail: string;
  AImage: TBitmap; ATagStr: string; AItems: array of string; const ADefaultItem: string = ''; const AUseActionSheet: Boolean = False): TksVListItem;
var
  AStr: string;
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    for AStr in AItems do
      AStrings.Add(AStr);
    Result := AddPickerSelector(ATitle, ASubTitle, ADetail, AImage, ATagStr, AStrings, ADefaultItem, AUseActionSheet);
  finally
    FreeAndNil(AStrings);
  end;
end;

function TksVListItemList.AddPickerSelector(ATitle, ASubTitle, ADetail: string;
  AImage: TBitmap; ATagStr: string; const AUseActionSheet: Boolean = False): TksVListItem;
begin
  Result := AddPickerSelector(ATitle, ASubTitle, ADetail, AImage, ATagStr, [], '', AUseActionSheet);
end;

function TksVListItemList.AddSeperator(const AText: string = ''): TksVListItem;
var
  AObj: TksVListItemTextObject;
begin
  Result := Add('', '', '');

  Result.Background := claNull;
  Result.Purpose := TksVListItemPurpose.Seperator;

  if AText <> '' then Result.Height := 48;

  AObj := Result.AddText(0, 0, AText);
  AObj.VertAlign := TVerticalAlignment.taAlignBottom;
  AObj.TextSettings.FontColor := claDimgray;

  AObj.Font.Size := 12;
  AObj.Top := -5;
  Result.CanSelect := False;
end;

function TksVListItemList.AddDateSelector(ATitle, ASubTitle: string; ASelected: TDateTime;
  AImage: TBitmap; ATagStr: string): TksVListItem;
var
  AStr: string;
begin
  AStr := '';
  if ASelected > 0 then
    AStr := FormatDateTime('ddd, dd mmm, yyyy', ASelected);
  Result := Add(ATitle, ASubTitle, AStr, AImage, atMore);
  Result.FSelectedDate := ASelected;
  Result.SelectorType := TksVListItemSelectorType.ksSelectorDate;
  Result.TagStr := ATagStr;
end;

function TksVListItemList.AddDateTimeSelector(ATitle, ASubTitle: string;
  ASelected: TDateTime; AImage: TBitmap; ATagStr: string): TksVListItem;
var
  AStr: string;
begin
  AStr := '';
  if ASelected > 0 then
    AStr := FormatDateTime('ddd, dd mmm, yyyy - hh:nn', ASelected);
  Result := Add(ATitle, ASubTitle, AStr, AImage, atMore);
  Result.FSelectedDateTime := ASelected;
  Result.SelectorType := TksVListItemSelectorType.ksSelectorDateTime;
  Result.TagStr := ATagStr;
end;

function TksVListItemList.AddTimeSelector(ATitle, ASubTitle: string; ASelected: TDateTime;
  AImage: TBitmap; ATagStr: string): TksVListItem;
begin
  Result := Add(ATitle, ASubTitle, FormatDateTime('hh:nn', ASelected), AImage, atMore);
  Result.FSelectedTime := ASelected;
  Result.SelectorType := TksVListItemSelectorType.ksSelectorTime;
  Result.TagStr := ATagStr;
end;

function TksVListItemList.Insert(AIndex: integer;
  ATitle, ASubTitle, ADetail,AQuantity: string;
  const AAccessory: TksAccessoryType = atNone): TksVListItem;
begin
  Result := TksVListItem.Create(Self);
  Result.Title.Text := ATitle;
  Result.SubTitle.Text := ASubTitle;
  Result.Detail.Text := ADetail;
  Result.Quantity.Text := AQuantity;
  Result.Accessory.AccessoryType := AAccessory;
  inherited Insert(AIndex, Result);
  Changed(True);
end;

procedure TksVListItemList.Clear;
begin
  inherited Clear;
  Changed(True);
end;


procedure TksVListItemList.ClearCache;
var
  AItem: TksVListItem;
begin
  for AItem in Self do
    AItem.ClearCache;
end;

constructor TksVListItemList.Create(AOwner: TksVirtualListView);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TksVListItemList.Delete(AIndex: integer; AAnimate: Boolean);
begin
  if (AIndex < 0) or (AIndex > (Count - 1)) then
    Exit;
  if AAnimate then
    items[AIndex].DeleteItem
  else
  begin
    Delete(AIndex);
    FOwner.UpdateScrollLimmits;
    FOwner.DoItemDeleted;

  end;
end;

procedure TksVListItemList.Delete(AItem: TksVListItem; const AAnimate: Boolean = False);
begin;
  Delete(IndexOf(AItem), AAnimate);
end;

function TksVListItemList.GetCheckedCount: integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count-1 do
    if Items[ICount].Checked then
      Result := Result +1;
end;

function TksVListItemList.GetFirstChecked: TksVListItem;
var
  AItem: TksVListItem;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.Checked then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

function TksVListItemList.ItemAtPos(x, y: single): TksVListItem;
var
  ICount: integer;
  ARect: TRectF;
begin
  Result := nil;
  y := y + FOwner.ScrollPos;
  for ICount := 0 to Count - 1 do
  begin
    ARect := Items[ICount].ItemRect;

    if (Y >= ARect.Top) and (y < ARect.Bottom) then
    begin
      Result := Items[ICount];
      Exit;
    end;
  end;
end;


function TksVListItemList.ItemByTagStr(ATagStr: string): TksVListItem;
var
  AItem: TksVListItem;
begin
  Result := nil;
  for AItem in Self do
  begin
    if AItem.TagStr = ATagStr then
    begin
      Result := AItem;
      Exit;
    end;
  end;
end;

procedure TksVListItemList.SetCheckedByTagStr(ATagStr: string;
  AChecked: Boolean);
var
  AItem: TksVListItem;
begin
  AItem := ItemByTagStr(ATagStr);
  if AItem <> nil then
    AItem.Checked := AChecked;
end;

procedure TksVListItemList.Changed(AUpdateScrollLimits: Boolean);
begin
  if not Assigned(FOwner) then
    Exit;
  if FOwner.FUpdateCount > 0 then
    Exit;
  if AUpdateScrollLimits then
  begin
    UpdateItemRects;
    FOwner.UpdateScrollLimmits;
  end;
  FOwner.Invalidate;
end;

procedure TksVListItemList.UpdateItemRects;
var
  ICount: integer;
  ARect: TRectF;
  AYPos: integer;
  AItem: TksVListItem;
begin
  AYPos := 0;
  FOwner.FTotalItemHeight := 0;
  for ICount := 0 to Count - 1 do
  begin
    AItem := Items[ICount];

    AItem.FAbsoluteIndex := ICount;
    AItem.FIndex := ICount;
    ARect := RectF(0, AYPos, FOwner.Width, AYPos + AItem.Height);

    if AItem.Title.FMaxWidth = 0 then AItem.Title.FMaxWidth := Round(FOwner.Width-50);
    if AItem.SubTitle.FMaxWidth = 0 then AItem.SubTitle.FMaxWidth := Round(FOwner.Width-50);

    AItem.FItemRect := ARect;
    AYPos := AYPos + AItem.Height;
    FOwner.FTotalItemHeight := FOwner.FTotalItemHeight + AItem.Height;
  end;
end;

{ TksVListItemTextObject }


function TksVListItemTextObject.ActualTextWidth: single;
var
  ARect: TRectF;
begin
  if FActualTextWidth > 0 then
  begin
    Result := FActualTextWidth;
    Exit;
  end;

  ARect := RectF(0, 0, FWidth, MaxSingle);
  if ARect.Width = 0 then
    ARect.Width := MaxSingle;

  TCanvasManager.MeasureCanvas.Font.Assign(FTextSettings.Font);
  TCanvasManager.MeasureCanvas.MeasureText(ARect, FText, FTextSettings.WordWrap, [], FTextSettings.HorzAlign, TTextAlign.Leading);
  Result := ARect.Width;
  FActualTextWidth := ARect.Width;
end;

procedure TksVListItemTextObject.BeforeRenderText(ACanvas: TCanvas; ARect: TRectF);
begin
  //
end;


function TksVListItemTextObject.CalculateTextWidth(AText: string; AFont: TFont; AWordWrap: Boolean;
  const AMaxWidth: single = 0; const APadding: single = 0): single;
var
  APoint: TPointF;
begin
  ATextLayout.BeginUpdate;
  // Setting the layout MaxSize
  if AMaxWidth > 0 then
    APoint.X := AMaxWidth
  else
    APoint.x := MaxSingle;
  APoint.y := 100;
  ATextLayout.MaxSize := APoint;
  ATextLayout.Text := AText;
  ATextLayout.WordWrap := AWordWrap;
  ATextLayout.Padding.Rect := RectF(APadding, APadding, APadding, APadding);
  ATextLayout.Font.Assign(AFont);
  ATextLayout.HorizontalAlign := TTextAlign.Leading;
  ATextLayout.VerticalAlign := TTextAlign.Leading;
  ATextLayout.EndUpdate;
  Result := ATextLayout.Width + (1/GetScreenScale);
end;

function TksVListItemTextObject.CalculateSize: TSizeF;
begin
  if (FAutoSize) or (FWidth = 0) then
    FWidth := CalculateTextWidth(FText, FTextSettings.Font, FTextSettings.WordWrap, FMaxWidth, 0);
  if (FAutoSize) or (FHeight = 0) then
    FHeight := CalculateTextHeight(FText, FTextSettings.Font, FTextSettings.WordWrap, FTextSettings.Trimming, FWidth, 0);

  Result.Width := FMaxWidth+2;
  Result.Height := FHeight+2;
end;

function TksVListItemTextObject.CalculateWidth: single;
begin
  Result := CalculateTextWidth(FText, FTextSettings.Font, False);
end;

procedure TksVListItemTextObject.Changed;
begin
  FTextSize := Point(0, 0);
  CalculateSize;
  inherited;
end;

procedure TksVListItemTextObject.ClearCache;
begin
  inherited;
  FTextSize := PointF(0, 0);
  FMaxWidth := 0;
end;

constructor TksVListItemTextObject.Create(AItem: TksVListItem);
begin
  inherited Create(AItem);
  FTextSize := PointF(0, 0);
  FTextLayout := nil;//TTextLayoutManager.DefaultTextLayout.Create;
  FTextSettings := TTextSettings.Create(nil);
  {$IFDEF MSWINDOWS}
  FTextSettings.Font.Family := 'Arial';
  {$ENDIF}
  FTextSettings.Trimming := TTextTrimming.Character;
  FTextSettings.OnChanged := TextSettingsChanged;
  FMaxWidth := 0;
  FActualTextWidth := 0;
  FText := '';
  FAutoSize := True;
  FPasswordField := False;
end;

destructor TksVListItemTextObject.Destroy;
begin
  FreeAndNil(FTextSettings);
  FreeAndNil(FTextLayout);
  inherited;
end;

procedure TksVListItemTextObject.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
var
  ARect: TRectF;
  ATextColor: TAlphaColor;
  AText: string;
begin

  if (FText = '') or (FVisible = False) then
    Exit;

  if FTextLayout = nil then FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;

  if FTextSize.IsZero then
  begin

    FTextSize := CalculateSize;
    ARect := CalcObjectRect(AItemRect);

    FTextLayout.Trimming := TTextTrimming.None;
    if (FMaxWidth > 0) and (FMaxWidth < ARect.Width) then
    begin
      ARect.Width := FMaxWidth;
      FTextLayout.Trimming := TTextTrimming.Character;
    end;
    FTextLayout.BeginUpdate;
    AText := FText;
    if FPasswordField then
      AText := StringOfChar('*', Length(AText));
    FTextLayout.Text := AText;
    FTextLayout.WordWrap := FTextSettings.WordWrap;
    FTextLayout.Font.Assign(FTextSettings.Font);
    FTextLayout.HorizontalAlign := FTextSettings.HorzAlign;
    FTextLayout.VerticalAlign := FTextSettings.VertAlign;
    FTextLayout.Padding.Rect := RectF(0,0,0,0);
    FTextLayout.Trimming := FTextSettings.Trimming;

    if FTextSettings.WordWrap  then
      FTextLayout.Trimming := TTextTrimming.None;


    FTextLayout.TopLeft := PointF(ARect.Left, ARect.Top);
    FTextLayout.MaxSize := PointF(ARect.Width, FTextSize.Y);
    FTextLayout.EndUpdate;
  end;

  ARect := CalcObjectRect(AItemRect);

  FTextLayout.TopLeft := PointF(ARect.Left, ARect.Top);
  FTextLayout.MaxSize := PointF(ARect.Width, FTextSize.y);

  ACanvas.Font.Assign(FTextSettings.Font);

  ATextColor := FTextSettings.FontColor;

  if (FOwner.Selected) and (FOwner.FOwner.FOwner.Appearence.SelectedFontColor <> claNull) then
    ATextColor := FOwner.FOwner.FOwner.Appearence.SelectedFontColor;
  FTextLayout.Color := ATextColor;

  ACanvas.Fill.Kind := TBrushKind.None;
  FTextLayout.RenderLayout(ACanvas);
end;

function TksVListItemTextObject.GetFont: TFont;
begin
  Result := FTextSettings.Font;
end;

function TksVListItemTextObject.MatchesFilter(AFilter: string): Boolean;
begin
  Result := Pos(LowerCase(AFilter), LowerCase(FText)) > 0;
end;

procedure TksVListItemTextObject.SetFont(const Value: TFont);
begin
  FTextSettings.Font.Assign(Value);
  Changed;
end;

procedure TksVListItemTextObject.SetMaxWidth(const Value: integer);
begin
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    Changed;
  end;
end;

procedure TksVListItemTextObject.SetPasswordField(const Value: Boolean);
begin
  if FPasswordField <> Value then
  begin
    FPasswordField := Value;
    Changed;
  end;
end;

procedure TksVListItemTextObject.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TksVListItemTextObject.TextSettingsChanged(Sender: TObject);
begin
  Changed;
end;

{ TksVListItemBaseObject }

function TksVListItemBaseObject.CalcObjectRect(AItemRect: TRectF): TRectF;
var
  ALeft: single;
begin
  ALeft := Left;
  if FUsePercentForXPos then
    ALeft := (AItemRect.Width / 100) * ALeft;

  Result := RectF(ALeft, Top, ALeft + (FWidth), Top + (FHeight));

  OffsetRect(Result, AItemRect.Left, AItemRect.Top);

  case FVertAlign of
    taVerticalCenter: OffsetRect(Result, 0, (AItemRect.Height - FHeight) / 2);
    taAlignBottom: OffsetRect(Result, 0, AItemRect.Height - FHeight);
  end;

  case FHorzAlign of
    taCenter: OffsetRect(Result, (AItemRect.Width - FWidth) / 2, 0);
    taRightJustify:
    begin
      OffsetRect(Result, (AItemRect.Width - FWidth), 0);
    end;
  end;
  FObjectRect := Result;
end;

procedure TksVListItemBaseObject.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  if FOwner <> nil then
    FOwner.Changed;
end;

procedure TksVListItemBaseObject.ClearCache;
begin
  //
end;

procedure TksVListItemBaseObject.Clicked;
begin
  //
end;

constructor TksVListItemBaseObject.Create(AItem: TksVListItem);
begin
  inherited Create;
  FOwner := AItem;
  FVisible := True;
  FVertAlign := TVerticalAlignment.taVerticalCenter;
  FUsePercentForXPos := False;
end;

procedure TksVListItemBaseObject.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
begin
  //
end;

function TksVListItemBaseObject.GetListview: TksVirtualListView;
begin
  Result := (FOwner.Fowner.FOwner as TksVirtualListView);
end;

procedure TksVListItemBaseObject.SetHeight(const Value: single);
begin
  FHeight := Value;
  Changed;
end;


procedure TksVListItemBaseObject.SetHorzAlign(const Value: TAlignment);
begin
  if FHorzAlign <> Value then
  begin
    FHorzAlign := Value;
    Changed;
  end;
end;

procedure TksVListItemBaseObject.SetLeft(const Value: single);
begin
  FLeft := Value;
  Changed;
end;

procedure TksVListItemBaseObject.SetSize(AWidth, AHeight: single);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  Changed;
end;

procedure TksVListItemBaseObject.SetTop(const Value: single);
begin
  FTop := Value;
  Changed;
end;

procedure TksVListItemBaseObject.SetUsePercentForXPos(const Value: Boolean);
begin
  if FUsePercentForXPos <> Value then
  begin
    FUsePercentForXPos := Value;
    Changed;
  end;
end;

procedure TksVListItemBaseObject.SetVertAlign(const Value: TVerticalAlignment);
begin
  if FVertAlign <> Value then
  begin
    FVertAlign := Value;
    Changed;
  end;
end;

procedure TksVListItemBaseObject.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TksVListItemBaseObject.SetWidth(const Value: single);
begin
  FWidth := Value;
  Changed;
end;

{ TksVListCheckBoxOptions }

procedure TksVListCheckBoxOptions.Changed;
begin
  FOwner.Invalidate;
end;

constructor TksVListCheckBoxOptions.Create(AOwner: TksVirtualListView);
begin
  inherited Create;
  FOwner := AOwner;
  FVisible := False;
  FMode := ksSingleSelect;
  FAlignment := ksCbRightAlign;
  FMakeBold := False;
end;

procedure TksVListCheckBoxOptions.SetAlignment(const Value
  : TksVListCheckBoxAlign);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

procedure TksVListCheckBoxOptions.SetMakeBold(const Value: Boolean);
begin
  if FMakeBold <> Value then
  begin
    FMakeBold := Value;
    Changed;
  end;
end;

procedure TksVListCheckBoxOptions.SetMode(const Value: TksSelectionType);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

procedure TksVListCheckBoxOptions.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TksVListSelectionOptions }

procedure TksVListSelectionOptions.Changed;
begin
  FOwner.Invalidate;
end;

constructor TksVListSelectionOptions.Create(AOwner: TksVirtualListView);
begin
  inherited Create;
  FOwner := AOwner;
  FSelectionType := ksSingleSelect;
  FKeepSelection := False;
end;

procedure TksVListSelectionOptions.SetKeepSelection(const Value: Boolean);
begin
  if FKeepSelection <> Value then
  begin
    FKeepSelection := Value;
    Changed;
  end;
end;

procedure TksVListSelectionOptions.SetSelectionType
  (const Value: TksSelectionType);
begin
  if Value <> FSelectionType then
  begin
    FSelectionType := Value;
    if FSelectionType = ksMultiSelect then
      FKeepSelection := True;
    Changed;
  end;
end;

{ TksVListItemImageObject }

constructor TksVListItemImageObject.Create(AItem: TksVListItem);
begin
  inherited;

  FBitmap := TBitmap.Create;;
  FRenderImage := TBitmap.Create;
  //FCached := nil;
  FBackground := claNull;
  FOpacity := 1;
  FBadge := 0;
end;

destructor TksVListItemImageObject.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FRenderImage);
  inherited;
end;

                        {
procedure DownloadImage(lv: TksVirtualListView; AItemID: string; AUrl: string);
var
  AHttp: THttpClient;
  AStream: TStream;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      AHttp := THTTPClient.Create;
      AStream := TMemoryStream.Create;
      try
        AHttp.Get(AUrl, AStream);
        AStream.Position := 0;
        TThread.Synchronize(nil,
          procedure
          begin
            lv.finItemByTagStr()
            //AItem.Bitmap.LoadFromStream(AStream);
            //AItem.Changed;
          end
        );
      finally
        AHttp.Free;
        AStream.Free;
      end;
    end
  ).Start;
end;
        } {
procedure DownloadImageAsync(lv: TksVirtualListView; AId, AUrl: string);
//begin
  //TThread.CreateAnonymousThread(
  //  procedure
    var
      AHttp: THttpClient;
      AStream: TStream;
    begin
      AHttp := THTTPClient.Create;
      AStream := TMemoryStream.Create;
      try
        AHttp.Get(AUrl, AStream);
        AStream.Position := 0;
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          var
            AItem: TksVListItem;
          begin
            for AItem in lv.Items do
            begin
              if AItem.Image.FDownloadID = AId then
              begin
                AItem.image.Bitmap.LoadFromStream(AStream);
                //AItem.Changed;
                Exit;
              end;
            end;
          end
        );
      finally
        AHttp.Free;
        AStream.Free;
      end;
//    end
//  ).Start;
end;    }
   {
procedure TksVListItemImageObject.DownloadGraphicFromURL(AUrl: string);
begin
  if AUrl <> '' then
    DownloadImageAsync(ListView, FDownloadID, AUrl);
end;
      }
procedure TksVListItemImageObject.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
var
  ARect: TRectF;
begin
  inherited;
  ARect := CalcObjectRect(AItemRect).Truncate;
  if FBackground <> claNull then
  begin
    ACanvas.Fill.Color := FBackground;
    ACanvas.Fill.Kind := TBrushKind.Solid;
    ACanvas.FillRect(ARect, 6, 6, AllCorners, 1);
    InflateRect(ARect, -4, -4);
  end;

  if FRenderImage.IsEmpty then
    FRenderImage.Assign(FBitmap);

  ARect := ARect.Round;
  ACanvas.DrawBitmap(FRenderImage, RectF(0, 0, FBitmap.Width, FBitmap.Height), ARect, FOpacity, False);
end;

function TksVListItemImageObject.GetIsEmpty: Boolean;
begin
  Result := FBitmap = nil;
  if not Result then
    Result := FBitmap.IsEmpty;
end;

procedure TksVListItemImageObject.SetBackground(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    if Value = claNull then
      Exit;
    FBackground := Value;
    Changed;
  end;
end;

procedure TksVListItemImageObject.SetBadge(const Value: integer);
begin
  FBadge := Value;
  Changed;
end;

procedure TksVListItemImageObject.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  FRenderImage.Clear(claNull);
  FRenderImage.SetSize(0, 0);
end;

procedure TksVListItemImageObject.SetImageShape(const Value: TksImageShape);
var
  ABmp: TBitmap;
begin
  if FImageShape <> Value then
  begin
    FImageShape := Value;

    FRenderImage.Assign(FBitmap);
    if Value = ksImageCircle then
    begin
      if FBitmap.IsEmpty then
        Exit;
      FRenderImage.Clear(claNull);
      ABmp := TBitmap.Create(FBitmap.Width, FBitmap.Height);
      try
        ABmp.Clear(claNull);
        ABmp.Canvas.BeginScene;
        ABmp.Canvas.Fill.Bitmap.Bitmap := FBitmap;
        ABmp.Canvas.Fill.Kind := TBrushKind.Bitmap;
        ABmp.Canvas.FillEllipse(RectF(0, 0, FBitmap.Width, FBitmap.Height), 1);
        ABmp.Canvas.EndScene;
        FRenderImage.Assign(ABmp);
      finally
        ABmp.Free;
      end;
    end;
    Changed;
  end;
end;

procedure TksVListItemImageObject.SetOpacity(const Value: single);
begin
  FOpacity := Value;
  Changed;
end;

procedure TksVListItemImageObject.SetOpaqueColor(AColor: TAlphaColor);
begin
  if AColor = claNull then
    Exit;
  ReplaceOpaqueColor(FBitmap, AColor);
  Changed;
end;

procedure TksVListItemImageObject.SetProperties(ABitmap: TBitmap; AOpaqueColor,
  ABackgroundColor: TAlphaColor);
begin
  Bitmap := ABitmap;
  SetOpaqueColor(AOpaqueColor);
  SetBackground(ABackgroundColor);
end;

{ TksVListItemAccessoryObject }

procedure TksVListItemAccessoryObject.Changed;
begin
  inherited;
  RedrawAccessory;
end;

constructor TksVListItemAccessoryObject.Create(AItem: TksVListItem);
begin
  inherited;
  FAccessoryType := atNone;
  FColor := claNull;
end;



procedure TksVListItemAccessoryObject.RedrawAccessory;
begin
  Bitmap := AAccessories.GetAccessoryImage(FAccessoryType);
  FWidth := Bitmap.Width / GetScreenScale;
  FHeight := Bitmap.Height / GetScreenScale;
  if FColor <> claNull then
    ReplaceOpaqueColor(Bitmap, FColor);
end;

procedure TksVListItemAccessoryObject.SetAccessoryType
  (const Value: TksAccessoryType);
begin
  if FAccessoryType <> Value then
  begin
    FAccessoryType := Value;
    RedrawAccessory;
    Changed;
  end;
end;

procedure TksVListItemAccessoryObject.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  RedrawAccessory;
end;

procedure TksVListItemAccessoryObject.SetOpaqueColor(AColor: TAlphaColor);
begin
  Color := AColor;
end;

{ TksVksListActionButton }

constructor TksVListActionButton.Create(AIsDelete: Boolean);
begin
  inherited Create;
  FIcon := TBitmap.Create;
  FTextColor := claWhite;
  FWidth := 80;
  FTagObject := nil;
end;

destructor TksVListActionButton.Destroy;
begin
  FreeAndNil(FIcon);
  if FTagObject <> nil then FreeAndNil(FTagObject);
  inherited;
end;

procedure TksVListActionButton.DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
var
  ATextRect: TRectF;
  AIconRect: TRectF;
begin
  FButtonRect := ARect;
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.Fill.Color := FColor;
  ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);
  ACanvas.Fill.Color := FTextColor;
  ACanvas.Font.Size := 12;
  ATextRect := ARect;
  ATextRect.Height := CalculateTextHeight(FText, ACanvas.Font, False,
    TTextTrimming.Character);
  if FText = '' then
    ATextRect.Height := 0;

  if FIcon.IsEmpty = False then
  begin
    AIconRect := RectF(ARect.Left, ARect.Top, ARect.Left + (ARect.Height / 2.5),
      ARect.Top + (ARect.Height / 2.5));
    OffsetRect(AIconRect, (ARect.Width - AIconRect.Width) / 2,
      ((ARect.Height - AIconRect.Height) / 2));
    if FText <> '' then
      OffsetRect(AIconRect, 0, -6);
    OffsetRect(ATextRect, 0, AIconRect.Bottom - ATextRect.Top);
    ACanvas.DrawBitmap(FIcon, RectF(0, 0, FIcon.Width, FIcon.Height),
      AIconRect, 1, False);
  end
  else
    OffsetRect(ATextRect, 0, (ARect.Height - ATextRect.Height) / 2);
  ACanvas.FillText(ATextRect, FText, False, 1, [], TTextAlign.Center,
    TTextAlign.Center);
end;

procedure TksVListActionButton.SetAccessory(const Value: TksAccessoryType);
begin
  FIcon.Assign(AAccessories.GetAccessoryImage(Value));
  FIcon.ReplaceOpaqueColor(FTextColor);

end;

procedure TksVListActionButton.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
  FIcon.ReplaceOpaqueColor(FTextColor);

end;

{ TksVListActionButtons }

function TksVListActionButtons.AddButton(AText: string;
  AColor, ATextColor: TAlphaColor; const AIcon: TksAccessoryType;
  const AWidth: integer): TksVListActionButton;
begin
  Result := TksVListActionButton.Create(False);
  Result.Width := AWidth;
  Result.Text := AText;
  Result.Color := AColor;
  Result.TextColor := ATextColor;
  Result.Accessory := AIcon;
  Add(Result);
end;

function TksVListActionButtons.ButtonAtXY(x, y: single): TksVListActionButton;
var
  ICount: integer;
begin
  Result := nil;
  for ICount := 0 to Count - 1 do
  begin
    if PtInRect(Items[ICount].FButtonRect, PointF(x, y)) then
    begin
      Result := Items[ICount];
      Exit;
    end;
  end;
end;

constructor TksVListActionButtons.Create(AOwner: TksVListItem);
begin
  inherited Create;
end;

procedure TksVListActionButtons.DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
var
  ICount: integer;
  AWidth: single;
  AXPos: single;
  ABtnRect: TRectF;
begin
  AXPos := 0;
  AWidth := ARect.Width / Count;
  for ICount := 0 to Count - 1 do
  begin
    ABtnRect := RectF(ARect.Left, ARect.Top, ARect.Left + AWidth, ARect.Bottom);
    OffsetRect(ABtnRect, AXPos, 0);
    Items[ICount].DrawToCanvas(ACanvas, ABtnRect);
    AXPos := AXPos + AWidth;
  end;
end;

function TksVListActionButtons.GetTotalWidth: integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count - 1 do
    Result := Result + Items[ICount].Width;
end;

{ TksVListPullToRefreshOptions }

constructor TksVListPullToRefreshOptions.Create;
begin
  inherited Create;
  FPullText := 'PULL TO REFRESH';
  FReleaseText := 'RELEASE TO REFRESH';
  FEnabled := False;
end;

{ TksVListDeleteButton }

constructor TksVListDeleteButton.Create;
begin
  inherited Create;
  FColor := claRed;
  FTextColor := claWhite;
  FEnabled := False;
  FText := 'Delete';
  FShowImage := True;
  FWidth := 60;
end;

{ TksScrollBar }

constructor TksScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FDesignInteractive := False;
  HitTest := False;
end;

{ TksVirtualListViewAppearence }

procedure TksVirtualListViewAppearence.Assign(ASource: TPersistent);
var
  Src : TksVirtualListViewAppearence;
begin
  if (ASource is TksVirtualListViewAppearence) then
  begin
    Src := TksVirtualListViewAppearence(ASource);

    FBackground                := Src.FBackground;
    FItemBackground            := Src.FItemBackground;
    FSeparatorColor            := Src.FSeparatorColor;
    FHeaderColor               := Src.FHeaderColor;
    FSelectedColor             := Src.FSelectedColor;
    FSelectedFontColor         := Src.SelectedFontColor;
  end
  else
    inherited;
end;

constructor TksVirtualListViewAppearence.Create(AListView: TksVirtualListView);
begin
  inherited Create;
  FListView := AListView;
  FItemBackground := claWhite;
  FBackground := claWhite;
  FSeparatorColor := claDarkgray;//$FFF0F0F0;
  FSelectedColor := C_VLIST_DEFAULT_SELECTED_COLOR;
  FSelectedFontColor := claNull;
  FHeaderColor := claNull;
  FHeaderFontColor := claNull;
end;

destructor TksVirtualListViewAppearence.Destroy;
begin
  inherited;
end;

procedure TksVirtualListViewAppearence.SetBackground(const Value: TAlphaColor);
begin
  FBackground := Value;
end;

procedure TksVirtualListViewAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
end;

procedure TksVirtualListViewAppearence.SetHeaderFontColor(const Value: TAlphaColor);
begin
  FHeaderFontColor := Value;
end;

procedure TksVirtualListViewAppearence.SetItemBackground(const Value: TAlphaColor);
begin
  FItemBackground := Value;
end;

procedure TksVirtualListViewAppearence.SetSelectedColor(
  const Value: TAlphaColor);
begin
  FSelectedColor := Value;
end;

procedure TksVirtualListViewAppearence.SetSelectedFontColor(const Value: TAlphaColor);
begin
  FSelectedFontColor := Value;
end;

procedure TksVirtualListViewAppearence.SetSeparatorBackground(
  const Value: TAlphaColor);
begin
  FSeparatorColor := Value;
end;

{ TksNoItemsText }

procedure TksNoItemsText.Changed;
begin
  FOwner.Invalidate;
end;

constructor TksNoItemsText.Create(AListView: TksVirtualListView);
begin
  inherited Create;
  FOwner := AListView;
  FFont := TFont.Create;
  FFont.Size := 18;
  FEnabled := False;
  FText := '';
  FTextColor := claSilver;
end;

destructor TksNoItemsText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TksNoItemsText.DrawToCanvas(ACanvas: TCanvas; ARect: TRectF);
begin
  if FEnabled = False then
    Exit;
  ACanvas.Font.Assign(FFont);
  ACanvas.Fill.Color := FTextColor;
  ACanvas.FillText(ARect, FText, False, 1, [], TTextAlign.Center, TTextAlign.Center);
end;

procedure TksNoItemsText.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Changed;
end;

procedure TksNoItemsText.SetFont(const Value: TFont);
begin
  FFont := Value;
  Changed;
end;

procedure TksNoItemsText.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;

{ TksVListItemShapeObject }

constructor TksVListItemShapeObject.Create(AItem: TksVListItem);
begin
  inherited;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, claBlack);
  FFill := TBrush.Create(TBrushKind.Solid, claNull);
  FCornerRadius := 0;
end;

destructor TksVListItemShapeObject.Destroy;
begin
  FreeAndNil(FStroke);
  FreeAndNil(FFill);
  inherited;
end;

procedure TksVListItemShapeObject.DrawToCanvas(ACanvas: TCanvas; AItemRect: TRectF);
var
  ARect: TRectF;
begin
  inherited;
  ARect := CalcObjectRect(AItemRect);
  ACanvas.Fill.Kind := TBrushKind.Solid;
  ACanvas.FillRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1, FFill);

  ACanvas.Stroke.Thickness := GetScreenScale;
  ACanvas.DrawRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1, FStroke);
end;

{ TksVListItemBubbleObject }

constructor TksVListItemBubbleObject.Create(AItem: TksVListItem);
begin
  inherited;
  FColor := claDodgerblue;
  FTextColor := claWhite;
end;

procedure TksVListItemBubbleObject.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  Changed;
end;

procedure TksVListItemBubbleObject.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
  Changed;
end;

{ TksVListObjectList }

procedure TksVListObjectList.ClearCache;
var
  ICount: integer;
begin
  for ICount := 0 to Count-1 do
    Items[ICount].ClearCache;
end;


function TksVListObjectList.ObjectAtPos(AItem: TksVListItem; x, y: single): TksVListItemBaseObject;
var
  AObj: TksVListItemBaseObject;
begin
  Result := nil;
  for AObj in Self do
  begin
    if PtInRect(AObj.FObjectRect, PointF(x, y)) then
    begin
      Result := AObj;
      Exit;
    end;
  end;
end;

function TksVListObjectList.ObjectByID(AID: string): TksVListItemBaseObject;
var
  AObj: TksVListItemBaseObject;
begin
  Result := nil;
  for AObj in Self do
  begin
    if AObj.ID = AID then
    begin
      Result := AObj;
      Exit;
    end;
  end;
end;

{ TksVListItemSwitchObject }


procedure TksVListItemSwitchObject.Clicked;
var
  lv: TksVirtualListView;
begin
  inherited;
  lv := ListView;
  Toggle;
  if Assigned(lv.FOnItemSwitchClick) then
    lv.FOnItemSwitchClick(lv, FOwner, FID, FChecked);
end;

constructor TksVListItemSwitchObject.Create(AItem: TksVListItem);
begin
  inherited;
  FWidth := SwitchWidth;
  FHeight := SwitchHeight;
  FChecked := False;
end;



procedure TksVListItemSwitchObject.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
var
  ARect: TRectF;

begin
  inherited;
  ARect := CalcObjectRect(AItemRect);
  ACanvas.Stroke.Color := claBlack;
  ACanvas.Stroke.Thickness := 1;

  SwitchImage(ACanvas, ARect, FChecked);
end;

procedure TksVListItemSwitchObject.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;

    Changed;
  end;
end;

procedure TksVListItemSwitchObject.Toggle;
begin
  Checked := not Checked;
end;

{ TksVListItemProgressBarObject }

procedure TksVListItemProgressBarObject.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
var
  ARect: TRectF;
  ABarRect: TRectF;
  AState: TCanvasSaveState;
begin
  ARect := CalcObjectRect(AItemRect);
  AState := ACanvas.SaveState;
  try
    ACanvas.IntersectClipRect(ARect);

    acanvas.Fill.Kind := TBrushKind.Solid;
    aCanvas.Fill.Color := claWhite;
    aCanvas.FillRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);

    if FValue > 0 then
    begin
      aCanvas.Fill.Color := FFill.Color;
      ABarRect := ARect;
      ABarRect.Width := (ABarRect.Width / FMax) * FValue;
      aCanvas.FillRect(ABarRect, FCornerRadius, FCornerRadius, AllCorners, 1);
    end;
    acanvas.Stroke.Color := FStroke.Color;
    acanvas.Stroke.Thickness := 1;
    ACanvas.DrawRect(ARect, FCornerRadius, FCornerRadius, AllCorners, 1);

  finally
    ACanvas.RestoreState(AState);
  end;
end;

procedure TksVListItemProgressBarObject.SetMax(const Value: integer);
begin
  FMax := Value;
  Changed;
end;

procedure TksVListItemProgressBarObject.SetValue(const Value: integer);
begin
  FValue := Value;
  Changed;
end;

{ TksVListItemSegmentButtons }

procedure TksVListItemSegmentButtons.Clicked(x, y: single);
var
  ABtnWidth: single;
  AIndex: integer;
  lv: TksVirtualListView;
  ACurrentIndex: integer;
begin
  inherited;
  lv := ListView;
  ACurrentIndex := FItemIndex;
  ABtnWidth := FObjectRect.Width / FCaptions.Count;
  for AIndex := 1 to FCaptions.Count do
  begin
    if (AIndex * ABtnWidth) > x then
    begin

      FItemIndex := AIndex-1;

      Break;
    end;
  end;

  if (FItemIndex = ACurrentIndex) and (FCaptions.Count = 2) then
  begin
    // toggle...
    if FItemIndex = 0 then
      FItemIndex := 1
    else
      FItemIndex := 0;
  end;

  if Assigned(lv.FOnItemSegmentButtonClick) then
    lv.FOnItemSegmentButtonClick(lv, FOwner, FID, FItemIndex);
end;

constructor TksVListItemSegmentButtons.Create(AItem: TksVListItem);
begin
  inherited;
  FCaptions := TStringList.Create;
  FItemIndex := 0;
  FColor := claDodgerblue;
  FHideInactiveText := False;
end;

destructor TksVListItemSegmentButtons.Destroy;
begin
  FreeAndNil(FCaptions);
  inherited;
end;

procedure TksVListItemSegmentButtons.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
var
  ARect: TRectF;
  AState: TCanvasSaveState;
  ABtnWidth: single;
  ICount: integer;
  APos: single;
  r: TRectF;
begin
  if (FVisible = False) then
    Exit;
  //inherited;
  ARect := CalcObjectRect(AItemRect);

  AState := ACanvas.SaveState;
  try
    ACanvas.IntersectClipRect(ARect);
    ACanvas.Stroke.Thickness := 1;///GetScreenScale;

    ACanvas.Stroke.Color := FColor;
    ACanvas.Fill.Color := FColor;
    ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);

    if FCaptions.Count > 0 then
    begin
      ABtnWidth := ARect.Width / FCaptions.Count;
      APos := ARect.Left;

      for ICount := 0 to FCaptions.Count-1 do
      begin
        APos := APos + ABtnWidth;

        if ICount = FItemIndex then
          ACanvas.Fill.Color := FColor
        else
          ACanvas.Fill.Color := claWhite;
        ACanvas.Fill.Kind := TBrushKind.Solid;

        r := RectF(APos-ABtnWidth, ARect.Top, APos, ARect.Bottom);
        ACanvas.FillRect(r, 0, 0, AllCorners, 1);

        ACanvas.Stroke.Color := FColor;

        ACanvas.DrawLine(PointF(r.Right, r.Top), PointF(r.Right, r.Bottom), 1);


        ACanvas.Font.Size := 12;
        if FItemIndex = ICount then
          ACanvas.Fill.Color := claWhite
        else
          ACanvas.Fill.Color := FColor;
        ACanvas.FillText(r, FCaptions[ICount], False, 1, [], TTextAlign.Center, TTextAlign.Center);
      end;
    end;

  finally
    ACanvas.RestoreState(AState);
  end;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Thickness := 1;
    ACanvas.Stroke.Color := FColor;
    ACanvas.DrawRect(ARect, 0, 0, AllCorners, 1);
end;

procedure TksVListItemSegmentButtons.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  changed;
end;

procedure TksVListItemSegmentButtons.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
  Changed;
end;

{ TksVListItemLineObject }

constructor TksVListItemLineObject.Create(AItem: TksVListItem);
begin
  inherited;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, claBlack);
end;

destructor TksVListItemLineObject.Destroy;
begin
  FreeAndNil(FStroke);
  inherited;
end;

procedure TksVListItemLineObject.DrawToCanvas(ACanvas: TCanvas;
  AItemRect: TRectF);
var
  ARect: TRectF;
begin
  inherited;
  ARect := CalcObjectRect(AItemRect);
  ACanvas.DrawLine(ARect.TopLeft, ARect.BottomRight, 1, FStroke);
end;

{ TksAniCalc }

procedure TksAniCalc.UpdatePosImmediately;
begin
  inherited UpdatePosImmediately(True);
end;

initialization

  AScreenScale := 0;
  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;

  ASwitchBmp[True] := TBitmap.Create;
  ASwitchBmp[True].BitmapScale := GetScreenScale;
  ASwitchBmp[False] := TBitmap.Create;
  ASwitchBmp[False].BitmapScale := GetScreenScale;


finalization

  ATextLayout.Free;

  FreeAndNil(ASwitchBmp[True]);
  FreeAndNil(ASwitchBmp[False]);

end.



