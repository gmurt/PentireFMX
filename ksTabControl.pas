{*******************************************************************************
*                                                                              *
*  TksTabControl - tab control component with badge support                    *
*                                                                              *
*  https://bitbucket.org/gmurt/kscomponents                                    *
*                                                                              *
*  Copyright 2017 Graham Murt                                                  *
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
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksTabControl;

interface

uses
  Classes, FMX.Types, FMX.Controls, FMX.Graphics, Types, System.UITypes,
  FMX.StdCtrls, System.Generics.Collections, FMX.Objects,
  System.UIConsts, FMX.Layouts;

type
  TksTabControl = class;
  TksTabItem = class;

  TksTabBarTheme = (ksTbCustom, ksTbLightTabs, ksTbDarkTabs);
  TksTabBarPosition = (ksTbpBottom, ksTbpNone, ksTbpTop);
  TksTabBarHighlightStyle = (ksTbHighlightSingleColor, ksTbHighlightFullColour);

  TksTabBarClickTabEvent = procedure(Sender: TObject; ATab: TksTabItem) of object;

  TksTabItemIcon = (Custom, AlarmClock, BarChart, Barcode, Bell, BookCover, BookCoverMinus, BookCoverPlus, BookMark, BookOpen,
                    Calendar, Camera, Car, Clock, CloudDownload, CloudUpload, Cross, Document, Download, Earth, Email,
                    Fax, FileList, FileMinus, FilePlus, Files, FileStar, FileTick, Flag, Folder, FolderMinus,
                    FolderPlus, FolderStar, Home, Inbox, Incoming, ListBullets, ListCheckBoxes, ListImages, ListNumbered, ListTicked,
                    Location, More, Note, Outgoing,
                    PaperClip, Photo, PieChart, Pin, Presentation, Search, Settings, Share, ShoppingCart, Spanner, Speaker,
                    Star, Tablet, Tag, Telephone, Telephone2, TelephoneBook, Tick, Timer, Trash, Upload,
                    User, UserEdit, UserGroup, Users, UserSearch,
                    VideoCamera, VideoPlayer, Viewer,
                    Wifi, Window, Write);

  TksTabBarAppearence = class(TPersistent)
  private
    [weak]FTabControl: TksTabControl;
    FSelected: TAlphaColor;
    FNormal: TAlphaColor;
    FBackground: TAlphaColor;
    FBadgeColor: TAlphaColor;
    FTheme: TksTabBarTheme;
    FSelectedBackground: TAlphaColor;
    procedure SetColours(ASelected, ANormal, ABackground, ASelectedBackground, ABadge: TAlphaColor);
    procedure SetBackground(const Value: TAlphaColor);
    procedure SetNormal(const Value: TAlphaColor);
    procedure SetSelected(const Value: TAlphaColor);
    procedure SetTheme(const Value: TksTabBarTheme);
    procedure Changed;
    procedure SetBadgeColor(const Value: TAlphaColor);
    procedure SetSelectedBackground(const Value: TAlphaColor);
  public
    constructor Create(ATabControl: TksTabControl);
  published
    property SelectedColor: TAlphaColor read FSelected write SetSelected default claDodgerblue;
    property NormalColor: TAlphaColor read FNormal write SetNormal default claGray;
    property BackgroundColor: TAlphaColor read FBackground write SetBackground default $FFEEEEEE;
    property SelectedBackgroundColor: TAlphaColor read FSelectedBackground write SetSelectedBackground default claWhite;
    property BadgeColor: TAlphaColor read FBadgeColor write SetBadgeColor default claDodgerblue;
    property Theme: TksTabBarTheme read FTheme write SetTheme default ksTbLightTabs;
  end;

  TksTabItem = class(TControl)
  private
    FIcon: TBitmap;
    FIconType: TksTabItemIcon;
    FBackground: TAlphaColor;
    FTabIndex: integer;
    FText: string;
    FBadgeValue: integer;
    FBadge: TBitmap;
    FHighlightStyle: TksTabBarHighlightStyle;
    //FCachedBmp: TBitmap;
    procedure SetText(const Value: string);
    procedure SetBadgeValue(const Value: integer);
    procedure UpdateTabs;
    procedure SetIcon(const Value: TBitmap);
    procedure SetIconType(const Value: TksTabItemIcon);
    procedure SetTabIndex(const Value: integer);
    procedure SetHighlightStyle(const Value: TksTabBarHighlightStyle);
    procedure FadeOut(ADuration: single);
    procedure FadeIn(ADuration: single);
    procedure SetBackground(const Value: TAlphaColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure DrawTab(ACanvas: TCanvas; AIndex: integer; ARect: TRectF);
    procedure RedrawBadge;
  published
    property Text: string read FText write SetText;
    property BadgeValue: integer read FBadgeValue write SetBadgeValue default 0;
    property Icon: TBitmap read FIcon write SetIcon;
    property StandardIcon: TksTabItemIcon read FIconType write SetIconType;
    property TabIndex: integer read FTabIndex write SetTabIndex;// stored False;
    property HitTest;
    property HighlightStyle: TksTabBarHighlightStyle read FHighlightStyle write SetHighlightStyle default ksTbHighlightSingleColor;
    property Background: TAlphaColor read FBackground write SetBackground default claWhite;
  end;

  TksTabItemList = class(TObjectList<TksTabItem>)
  private
    [weak]FTabControl: TksTabControl;
  protected
    procedure Notify(const Value: TksTabItem; Action: TCollectionNotification); override;
  public
    constructor Create(ATabControl: TksTabControl);

  end;

  TksTabBar = class(TControl)
  private
    [weak]FTabControl: TksTabControl;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Width;
    property Height;
    property Position;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksTabControl = class(TControl, IItemsContainer)
  private
    FTabBar: TksTabBar;
    FTabIndex: integer;
    FTabs: TksTabItemList;
    FAppearence: TksTabBarAppearence;
    FBeforeChange: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnClickTab: TksTabBarClickTabEvent;
    FTabPosition: TksTabBarPosition;
    FHasIOSHomeBar: Boolean;
    procedure SetTabIndex(const Value: integer);
    function GetTabRect(AIndex: integer): TRectF;
    function GetTabIndexFromXPos(AXPos: single): integer;
    function GetTabFromXPos(AXPos: single): TksTabItem;
    function GetTabCount: integer;
    function GetActiveTab: TksTabItem;
    function GetSelectedTab: TksTabItem;
    procedure SetTabBarPosition(const Value: TksTabBarPosition);
    procedure SetActiveTab(const Value: TksTabItem);
    function GetNextTabName: string;
    procedure SetHasIosHomeBar(const Value: Boolean);
  protected
    procedure DoRealign; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Paint; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    procedure DoClickTab(ATab: TksTabItem);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTabs;
    function AddTab: TksTabItem;
    function GetTabBarHeight: integer;

    procedure PrevTab;
    procedure NextTab;
    property ActiveTab: TksTabItem read GetActiveTab write SetActiveTab;
    property Tabs: TksTabItemList read FTabs;
    property SelectedTab: TksTabItem read GetSelectedTab;
    procedure FadeToNextTab(const ADelaySeconds: single = 0.5);
    procedure FadeToPrevTab(const ADelaySeconds: single = 0.5);
    procedure FadeToTab(ATab: TksTabItem; const ADelaySeconds: single = 0.5);
  published
    property Align;
    property Appearence: TksTabBarAppearence read FAppearence write FAppearence;
    property TabIndex: integer read FTabIndex write SetTabIndex default -1;
    property Margins;
    property Position;
    property Width;
    property Size;
    property HasIosHomeBar: Boolean read FHasIOSHomeBar write SetHasIosHomeBar default False;
    property Height;

    property Opacity;
    property TabPosition: TksTabBarPosition read FTabPosition write SetTabBarPosition default ksTbpBottom;
    property Visible;
    // events
    property BeforeChange: TNotifyEvent read FBeforeChange write FBeforeChange;
    property OnResize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClickTab: TksTabBarClickTabEvent read FOnClickTab write FOnClickTab;
  end;

  {$R *.dcr}
  {$R glyphs.res}

  procedure Register;

implementation

uses SysUtils, Math, FMX.Forms, TypInfo, FMX.Ani, FMX.Utils, FMX.Platform;

var
  AScreenScale: single;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksTabControl]);
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
  //x,y: Integer;
  AMap: TBitmapData;
  PixelColor: TAlphaColor;
  PixelWhiteColor: TAlphaColor;
  C: PAlphaColorRec;
begin
  TThread.Synchronize(nil,procedure
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

{ TksTabSheTksTabItemet }

procedure TksTabItem.BeforeDestruction;
begin
  inherited;
  if (Parent is TksTabControl) then
    TksTabControl(Parent).Tabs.Remove(Self);
end;

constructor TksTabItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FCachedBmp := TBitmap.Create;
  FIcon := TBitmap.Create;
  ClipChildren := True;
  Stored := True;
  Locked := True;
  Text := Name;
  FHighlightStyle := ksTbHighlightSingleColor;
  FBackground := claWhite;
  FBadgeValue := 0;
end;

destructor TksTabItem.Destroy;
begin
  //FreeAndNil(FCachedBmp);
  FreeAndNil(FIcon);
  inherited;
end;


procedure TksTabItem.DrawTab(ACanvas: TCanvas; AIndex: integer; ARect: TRectF);

procedure DrawEllipse(ACanvas: TCanvas; ARect: TRectF; AColor: TAlphaColor);
  begin
    ACanvas.Fill.Color := AColor;
    ACanvas.FillEllipse(ARect, 1);
    ACanvas.Stroke.Color := AColor;
    ACanvas.Stroke.Thickness := 1;
    ACanvas.DrawEllipse(ARect, 1);
  end;

var
  AAppearence: TksTabBarAppearence;
  ABmp: TBitmap;
  ADestRect: TRectF;
  r: TRectF;
  ABadgeRect: TRectF;
begin
  r := ARect;

  AAppearence := TksTabControl(Parent).Appearence;


  ACanvas.Fill.Color := AAppearence.NormalColor;

  if AIndex = TksTabControl(Parent).TabIndex then
  begin
    ACanvas.Fill.Color := AAppearence.SelectedBackgroundColor;
    ACanvas.FillRect(r, 0, 0, AllCorners, 1);
    ACanvas.Fill.Color := AAppearence.SelectedColor;
  end;

  r.Bottom := 50;
  InflateRect(r, 0, -3);


  ACanvas.Font.Size := 10;
  ACanvas.FillText(r, FText, False, 1, [], TTextAlign.Center, TTextAlign.Trailing);
  InflateRect(r, 0, -3);

  ADestRect := RectF(0, 0, 25, 25);
  OffsetRect(ADestRect, ARect.Left + ((ARect.Width - ADestRect.Width) / 2), 6);

  ABmp := TBitmap.Create;
  try
    ABmp.Assign(FIcon);

    if FHighlightStyle = ksTbHighlightSingleColor then
    begin
      // single colour selected/unselected icons...
      if (AIndex = TksTabControl(Parent).TabIndex) then
        ReplaceOpaqueColor(ABmp, AAppearence.SelectedColor)
      else
        ReplaceOpaqueColor(ABmp, AAppearence.NormalColor);
    end
    else
    begin
      // single colour selected/unselected icons...
      if (AIndex <> TksTabControl(Parent).TabIndex) then
        ReplaceOpaqueColor(ABmp, AAppearence.NormalColor);
    end;


    ACanvas.DrawBitmap(ABmp, RectF(0,0,ABmp.Width,ABmp.Height), ADestRect, 1, True);


    if (FBadgeValue <> 0) and (FBadge <> nil) then
    begin
      ABadgeRect := RectF(ADestRect.Left, ADestRect.Top, ADestRect.Left+ 16, ADestRect.Top+16);
      OffsetRect(ABadgeRect, ADestRect.Width-7, -2);

      ACanvas.DrawBitmap(FBadge, RectF(0, 0, FBadge.Width, FBadge.Height), ABadgeRect, 1);
      ACanvas.Fill.Color := claWhite;
      ACanvas.Font.Size := 10;
      if FBadgeValue > 0 then
        ACanvas.FillText(ABadgeRect, IntToStr(FBadgeValue), False, 1, [], TTextAlign.Center);
    end;

  finally
    FreeAndNil(ABmp);
  end;
end;

procedure TksTabItem.FadeIn(ADuration: single);
begin
  TAnimator.AnimateFloatWait(Self, 'Opacity', 1, ADuration);
end;

procedure TksTabItem.FadeOut(ADuration: single);
begin
  TAnimator.AnimateFloatWait(Self, 'Opacity', 0, ADuration);
end;

procedure TksTabItem.RedrawBadge;
var
  s: single;
begin
  if FBadgeValue = 0 then
    FreeAndNil(FBadge)
  else
  begin
    s := GetScreenScale;
    FBadge := TBitmap.Create(Round(32*s), Round(32*s));
    FBadge.Clear(claNull);
    //FBadge.Canvas.Fill.Color := claRed;
    FBadge.Canvas.Fill.Kind := TBrushKind.Solid;
    FBadge.Canvas.FillEllipse(RectF(0, 0, FBadge.Width, FBadge.Height), 1);
  end;
end;

procedure TksTabItem.SetIconType(const Value: TksTabItemIcon);
var
  AStream: TResourceStream;
  AEnumName: String;
begin
  if Value <> TksTabItemIcon.Custom  then
  begin
    AEnumName := GetENumName(TypeInfo(TksTabItemIcon), Ord(Value));
    AStream := TResourceStream.Create(HInstance, AEnumName, RT_RCDATA);
    try
      FIcon.LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
    FHighlightStyle := TksTabBarHighlightStyle.ksTbHighlightSingleColor;
  end;
  FIconType := Value;
  UpdateTabs;
end;



procedure TksTabItem.SetBackground(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    TksTabControl(Parent).Repaint;
  end;
end;

procedure TksTabItem.SetBadgeValue(const Value: integer);
begin
  if FBadgeValue <> Value then
  begin
    FBadgeValue := Value;
    RedrawBadge;
    UpdateTabs;
  end;
end;

procedure TksTabItem.SetHighlightStyle(const Value: TksTabBarHighlightStyle);
begin
  if FHighlightStyle <> Value then
  begin
    FHighlightStyle := Value;
    UpdateTabs;
  end;
end;

procedure TksTabItem.SetIcon(const Value: TBitmap);
begin
  FIcon.Assign(Value);
  FIconType := TksTabItemIcon.Custom;
  UpdateTabs;
end;

procedure TksTabItem.SetTabIndex(const Value: integer);
var
  ATabs: TksTabItemList;
  ANewIndex: integer;
  ICount: integer;
begin
  if FTabIndex <> Value then
  begin
    ATabs := TksTabControl(Parent).Tabs;
    ANewIndex := Value;
    if ANewIndex < 0 then FTabIndex := 0;
    if ANewIndex > ATabs.Count-1 then ANewIndex := ATabs.Count-1;
    //ATabs.Move(FTabIndex, ANewIndex);
    ATabs.Exchange(FTabIndex, ANewIndex);
    for ICount := 0 to ATabs.Count-1 do
      ATabs[ICount].FTabIndex := ICount;
    UpdateTabs;
  end;
end;

procedure TksTabItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateTabs;
  end;
end;


procedure TksTabItem.UpdateTabs;
begin
  TksTabControl(Parent).UpdateTabs;
end;

{ TksTabControl }

function TksTabControl.AddTab: TksTabItem;
var
  ATab: TksTabItem;
begin
  ATab := TksTabItem.Create(Root.GetObject as TForm);
  AddObject(ATab);
  Result := ATab;
end;


constructor TksTabControl.Create(AOwner: TComponent);
begin
  inherited;
  FHasIOSHomeBar := False;
  FTabs := TksTabItemList.Create(Self);
  FTabBar := TksTabBar.Create(Self);
  FAppearence := TksTabBarAppearence.Create(Self);
  SetAcceptsControls(True);
  Size.Height := 250;
  Size.Width := 250;

  FTabBar.Align := TAlignLayout.Bottom;
  FTabPosition := ksTbpBottom;
  AddObject(FTabBar);
end;

destructor TksTabControl.Destroy;
begin
  FreeAndNil(FTabs);
  FAppearence.DisposeOf;
  inherited;
end;

procedure TksTabControl.DoAddObject(const AObject: TFmxObject);
var
  ATab: TksTabItem;
begin
  inherited;

  if AObject.ClassType = TksTabItem then
  begin

    ATab := TksTabItem(AObject);
    FTabs.Add(ATab);
    if not (csLoading in ComponentState) then
    begin
      ATab.Name := GetNextTabName;
      ATab.Text := ATab.Name;
    end;
    UpdateTabs;
    Exit;
  end;

  if (SelectedTab <> nil) and (AObject <> FTabBar) then
  begin
    SelectedTab.AddObject(AObject);
  end;
end;

procedure TksTabControl.DoClickTab(ATab: TksTabItem);
begin
  if Assigned(FOnClickTab) then
    FOnClickTab(Self, ATab);
end;

procedure TksTabControl.DoRealign;
begin
  inherited;
  UpdateTabs;
  Repaint;
end;

function TksTabControl.GetActiveTab: TksTabItem;
begin
  if InRange(TabIndex, 0, GetTabCount - 1) then
    Result := Tabs[TabIndex]
  else
    Result := nil;
end;


function TksTabControl.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := Tabs[AIndex];
end;

function TksTabControl.GetItemsCount: Integer;
begin
  Result := FTabs.Count;
end;

function TksTabControl.GetNextTabName: string;
var
  AIndex: integer;
  AForm: TForm;
begin
  AForm := (Root.GetObject as TForm);
  AIndex := 0;
  while AForm.FindComponent('ksTabItem'+IntToStr(AIndex)) <> nil do
    Inc(AIndex);

  Result := 'ksTabItem'+IntToStr(AIndex);
  {repeat
    Inc(AIndex);
    Result := 'ksTabItem'+IntToStr(AIndex);
  until AOwner.FindComponent(Result) = nil;   }
end;

function TksTabControl.GetSelectedTab: TksTabItem;
begin
  Result := nil;
  if (FTabIndex > -1) and (FTabIndex <= FTabs.Count-1) then
    Result := FTabs[FTabIndex];
end;

function TksTabControl.GetTabBarHeight: integer;
begin
  Result := 50;
  if FHasIOSHomeBar then
    Result := Result + 20;
end;

function TksTabControl.GetTabCount: integer;
begin
  Result := 0;
  if FTabs <> nil then
    Result := FTabs.Count;
end;

function TksTabControl.GetTabFromXPos(AXPos: single): TksTabItem;
var
  ICount: integer;
begin
  Result := nil;
  for ICount := 0 to GetTabCount-1 do
  begin
    if PtInRect(GetTabRect(ICount), PointF(AXPos, 1)) then
    begin
      Result := Tabs[ICount];
      Exit;
    end;
  end;
end;

function TksTabControl.GetTabIndexFromXPos(AXPos: single): integer;
var
  ICount: integer;
begin
  Result := -1;
  for ICount := 0 to GetTabCount-1 do
  begin
    if PtInRect(GetTabRect(ICount), PointF(AXPos, 1)) then
    begin
      Result := ICount;
      Exit;
    end;
  end;
end;

function TksTabControl.GetTabRect(AIndex: integer): TRectF;
var
  AWidth: single;
begin
  AWidth := Width / GetTabCount;
  Result := RectF(0, 0, AWidth, GetTabBarHeight);
  OffsetRect(Result, AWidth * AIndex, 0)
end;

procedure TksTabControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

end;

procedure TksTabControl.NextTab;
begin
  TabIndex := TabIndex +1;
end;

procedure TksTabControl.Paint;
var
  ARect: TRectF;
begin
  inherited;

  if Locked then
    Exit;

  Canvas.Fill.Color := FAppearence.BackgroundColor;
  if (SelectedTab <> nil) then
  begin
    Canvas.Fill.Color := FAppearence.BackgroundColor;
    if (SelectedTab <> nil) then
    begin
      if SelectedTab.Background <> claNull then
        Canvas.Fill.Color := SelectedTab.Background;
    end;
    Canvas.Fill.Kind := TBrushKind.Solid;
    ARect := ClipRect;
    if TabPosition <> TksTabBarPosition.ksTbpNone then
      ARect.Bottom := ARect.Bottom - FTabBar.Height;
    if SelectedTab.Background <> claNull then
      Canvas.Fill.Color := SelectedTab.Background;
  end;
  Canvas.Fill.Kind := TBrushKind.Solid;
  ARect := ClipRect;
  if TabPosition <> TksTabBarPosition.ksTbpNone then
    ARect.Bottom := ARect.Bottom - FTabBar.Height;

    Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
    if (csDesigning in ComponentState) then
    begin
      DrawDesignBorder(claDimgray, claDimgray);
      Canvas.Fill.Color := claDimgray;;
  Canvas.FillRect(ARect, 0, 0, AllCorners, 1);
  if (csDesigning in ComponentState) then
  begin
    DrawDesignBorder(claDimgray, claDimgray);
    Canvas.Fill.Color := claDimgray;;



      {$IFDEF MSWINDOWS}
      if GetTabCount = 0 then
      begin
        Canvas.Font.Size := 14;
        Canvas.FillText(ClipRect, 'Right-click to add tabs', True, 1, [], TTextAlign.Center);
      end;
      {$ENDIF}

    end;

    {$IFDEF MSWINDOWS}
    if GetTabCount = 0 then
    begin
      Canvas.Font.Size := 14;
      Canvas.FillText(ClipRect, 'Right-click to add tabs', True, 1, [], TTextAlign.Center);
    end;
  {$ENDIF}
  end;

end;

procedure TksTabControl.PrevTab;
begin
  TabIndex := TabIndex -1;
end;

procedure TksTabControl.Resize;
begin
  inherited;
  UpdateTabs;
  Repaint;
end;

procedure TksTabControl.SetActiveTab(const Value: TksTabItem);
begin
  if Value <> nil then
  begin
    TabIndex := Tabs.IndexOf(Value);
  end;
end;

procedure TksTabControl.SetHasIosHomeBar(const Value: Boolean);
begin
  //if FHasIOSHomeBar <> Value then
  begin
    FHasIOSHomeBar := Value;
    FTabBar.Height := GetTabBarHeight;
    UpdateTabs;
  end;
end;

procedure TksTabControl.FadeToNextTab(const ADelaySeconds: single = 0.5);
begin
  if TabIndex < FTabs.Count-1 then
    FadeToTab(Tabs[TabIndex+1], ADelaySeconds);
end;

procedure TksTabControl.FadeToPrevTab(const ADelaySeconds: single = 0.5);
begin
  if TabIndex > 0 then
    FadeToTab(Tabs[TabIndex-1], ADelaySeconds);
end;

procedure TksTabControl.FadeToTab(ATab: TksTabItem; const ADelaySeconds: single = 0.5);
var
  APrevTab: TksTabItem;
begin
  APrevTab := ActiveTab;
  ActiveTab.FadeOut(ADelaySeconds/2);
  ATab.Opacity := 0;
  ActiveTab := ATab;
  ATab.FadeIn(ADelaySeconds/2);
  APrevTab.Opacity := 1;
end;

procedure TksTabControl.SetTabBarPosition(const Value: TksTabBarPosition);
begin
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    UpdateTabs;
  end;
end;

procedure TksTabControl.SetTabIndex(const Value: integer);
begin
  if (Tabs.Count > 0) and ((Value < 0) or (Value > Tabs.Count-1)) then
    Exit;
  if FTabIndex <> Value then
  begin
    if Assigned(FBeforeChange) then
      FBeforeChange(Self);


    FTabIndex := Value;
    UpdateTabs;
    if Assigned(FOnChange) then
      FOnChange(Self);

  end;
end;

procedure TksTabControl.UpdateTabs;
var
  ICount: integer;
  ATab: TksTabItem;
begin
  for ICount := 0 to GetTabCount-1 do
  begin
    case FTabPosition of
      ksTbpBottom: FTabBar.Align := TAlignLayout.Bottom;
      ksTbpTop: FTabBar.Align := TAlignLayout.Top;
    end;
    ATab := Tabs[ICount];
    //ATab.FCachedBmp.SetSize(0, 0);
    ATab.RedrawBadge;
    ATab.FTabIndex := ICount;
    ATab.Width := Self.Width;
    case FTabPosition of
      ksTbpBottom: ATab.Height := Self.Height-GetTabBarHeight;
      ksTbpNone: ATab.Height := Self.Height;
      ksTbpTop: ATab.Height := Self.Height-50;
    end;
    ATab.Position.Y := 0;
    if FTabPosition = ksTbpTop then
      ATab.Position.Y := 50;

    ATab.Position.X := 0;

    ATab.Visible := (ICount = FTabIndex);
    ATab.Realign;
  end;
  InvalidateRect(ClipRect);
  {$IFDEF ANDROID}
  Application.ProcessMessages;
  {$ENDIF}
end;

{ TksTabBar }

constructor TksTabBar.Create(AOwner: TComponent);
begin
  inherited;
  FTabControl := (AOwner as TksTabControl);
  Size.Height := FTabControl.GetTabBarHeight;
  Stored := False;
  FDesignInteractive := True;
end;

procedure TksTabBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ATab: TksTabItem;
begin
  inherited;
  ATab := TksTabControl(FTabControl).GetTabFromXPos(X);
  if ATab.HitTest = False then
    Exit;
  TksTabControl(Parent).TabIndex := TksTabControl(FTabControl).GetTabIndexFromXPos(X);

  {$IFDEF MSWINDOWS}
  if (csDesigning in ComponentState) and
     (TCommonCustomForm(FTabControl.Owner).Designer <> nil) then
  begin
    if ATab <> nil then
    begin
      if ATab.HitTest = False then
        Exit;
      TCommonCustomForm(FTabControl.Owner).Focused := nil;
      TCommonCustomForm(FTabControl.Owner).Designer.SelectComponent(ATab);
      TCommonCustomForm(FTabControl.Owner).Designer.Modified;
    end;
    Repaint;
  end;
 {$ENDIF}
  if (ATab <> nil) then
  begin
    if ATab.HitTest then
      FTabControl.DoClickTab(ATab);
  end;
end;

procedure TksTabBar.Paint;
var
  AState: TCanvasSaveState;
  ICount: integer;
  ARect: TRectF;
  ATabControl: TksTabControl;
begin
  inherited;
  //if Locked then
  //  Exit;
  if Locked then
    Exit;

  ATabControl := TksTabControl(FTabControl);
  with Canvas do
  begin
    AState := SaveState;
    try
      ARect := RectF(0, 0, Size.Width, Size.Height);
      if (csDesigning in ComponentState) then
        InflateRect(ARect, -1, -1);
      IntersectClipRect(ARect);

      if FTabControl.TabPosition = ksTbpNone then
      Exit;


      Fill.Kind := TBrushKind.Solid;

      if ATabControl.Appearence.BackgroundColor <> claNull then
        Canvas.Clear(ATabControl.Appearence.BackgroundColor);

      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := claDimgray;

      if (csDesigning in ComponentState) then
        DrawDesignBorder(claDimgray, claDimgray);



      for ICount := 0 to TksTabControl(FTabControl).GetTabCount-1 do
      begin
        //if ATabControl.Tabs[ICount].Visible then
        ATabControl.Tabs[ICount].DrawTab(Canvas, ICount, ATabControl.GetTabRect(ICount));
      end;

      //Stroke.Color := claRed;
      case ATabControl.TabPosition of
        ksTbpBottom: DrawRectSides(ARect, 0, 0, AllCorners,1, [TSide.Top]);
        ksTbpTop: DrawRectSides(ARect, 0, 0, AllCorners,1, [TSide.Bottom]);
      end;

    finally

      RestoreState(AState);
      Font.Size := 10;

    end;
  end;
end;

{ TksTabBarAppearence }

procedure TksTabBarAppearence.Changed;
begin
  TksTabControl(FTabControl).UpdateTabs;
  //TksTabControl(FTabControl).Repaint;
end;

constructor TksTabBarAppearence.Create(ATabControl: TksTabControl);
begin
  inherited Create;
  FTabControl := ATabControl;
  FSelected := claDodgerblue;
  FNormal := claGray;
  FBackground := $FFEEEEEE;
  FSelectedBackground := claWhite;
  FBadgeColor := claDodgerblue;
  FTheme := ksTbLightTabs;

end;

procedure TksTabBarAppearence.SetBackground(const Value: TAlphaColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    FTheme := ksTbCustom;
    Changed;
  end;
end;

procedure TksTabBarAppearence.SetBadgeColor(const Value: TAlphaColor);
begin
  if FBadgeColor <> Value then
  begin
    FBadgeColor := Value;
    FTheme := ksTbCustom;
    Changed;
  end;
end;

procedure TksTabBarAppearence.SetColours(ASelected, ANormal, ABackground, ASelectedBackground, ABadge: TAlphaColor);
begin
  FSelected := ASelected;
  FNormal := ANormal;
  FBackground := ABackground;
  FSelectedBackground := ASelectedBackground;
  FBadgeColor := ABadge;
end;

procedure TksTabBarAppearence.SetNormal(const Value: TAlphaColor);
begin
  if FNormal <> Value then
  begin
    FNormal := Value;
    FTheme := ksTbCustom;
    Changed;
  end;
end;

procedure TksTabBarAppearence.SetSelected(const Value: TAlphaColor);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    FTheme := ksTbCustom;
    Changed;
  end;
end;

procedure TksTabBarAppearence.SetSelectedBackground(const Value: TAlphaColor);
begin
  if FSelectedBackground <> Value then
  begin
    FSelectedBackground := Value;
    Changed;
  end;
end;

procedure TksTabBarAppearence.SetTheme(const Value: TksTabBarTheme);
begin
  if FTheme <> Value then
  begin
    if Value = ksTbLightTabs then SetColours(claDodgerblue, claGray, $FFEEEEEE, claWhite, claDodgerblue);
    if Value = ksTbDarkTabs then SetColours(claWhite, claGray, $FF202020, $FF3C3C3C, claRed);
    FTheme := Value;
    Changed;
  end;
end;

{ TksTabItemList }

constructor TksTabItemList.Create(ATabControl: TksTabControl);
begin
  inherited Create(False);
  FTabControl := ATabControl;
end;


procedure TksTabItemList.Notify(const Value: TksTabItem; Action: TCollectionNotification);
begin
  inherited;
  if csDestroying in FTabControl.ComponentState then
    Exit;
  if (Action = TCollectionNotification.cnRemoved) and (not (csDesigning in FTabControl.ComponentState)) then
  begin
    Value.DisposeOf;
    FTabControl.UpdateTabs;
  end;
end;

initialization

  RegisterFmxClasses([TksTabControl, TksTabItem]);
  AScreenScale := 0;

end.




