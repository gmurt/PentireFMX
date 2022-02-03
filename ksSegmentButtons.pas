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

unit ksSegmentButtons;

interface


uses
  Classes, FMX.Types, FMX.Controls, FMX.Graphics, Types, System.UITypes,
  FMX.StdCtrls, System.Generics.Collections, FMX.Objects, FMX.Effects,
  System.UIConsts;

type
  TksSegmentButton = class;
  TksSegmentButtons = class;

  TksSelectSegmentButtonEvent = procedure(Sender: TObject; AIndex: integer; AButton: TksSegmentButton) of object;


  TksSegmentSpeedButton = class(TControl)
  private
    FBadge: integer;
    FIsPressed: Boolean;
    FText: string;
    FIndex: integer;
    FOwner: TksSegmentButtons;
    procedure Changed;
    procedure SetBadge(const Value: integer);
    procedure SetIsPressed(const Value: Boolean);
    procedure SetText(const Value: string);

  protected
    procedure Paint; override;
    //procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  public
    constructor Create(AOwner: TComponent); override;
    property Badge: integer read FBadge write SetBadge;
    property IsPressed: Boolean read FIsPressed write SetIsPressed;
    property Text: string read FText write SetText;
    property Index: integer read FIndex write FIndex;
  end;

  TKsSegmentButton = class(TCollectionItem)
  private
    FButton: TksSegmentSpeedButton;
    FID: string;
    FText: string;
    FVisible: Boolean;
    procedure SetText(const Value: string);
    function GetBadgeValue: integer;
    procedure SetBadgeValue(const Value: integer);
    function GetBoundsRect: TRectF;

    function GetIndex: integer;
    procedure SetVisible(const Value: Boolean);  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ID: string read FID write FID;
    property Text: string read FText write SetText;
    property BoundsRect: TRectF read GetBoundsRect;
    property Index: integer read GetIndex;
    property BadgeValue: integer read GetBadgeValue write SetBadgeValue;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TksSegmentButtonCollection = class(TCollection)
  private
    FSegmentButtons: TKsSegmentButtons;
  protected
    function GetOwner: TPersistent; override;
    function GetItem(Index: Integer): TKsSegmentButton;
    procedure SetItem(Index: Integer; Value: TKsSegmentButton);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AButtons: TKsSegmentButtons);
    function Add: TKsSegmentButton;
    function Insert( Index: Integer ): TKsSegmentButton;
    function VisibleCount: integer;
    property Items[index: Integer]: TKsSegmentButton read GetItem write SetItem; default; // default - Added by Fenistil
  end;


  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksSegmentButtons = class(TControl)
  private
    FGroupID: string;
    FInitialIndex: integer;
    FItemIndex: integer;
    FBtnWidth: single;
    FFontSize: integer;
    FOnChange: TNotifyEvent;
    FSegments: TksSegmentButtonCollection;
    FTintColor: TAlphaColor;
    FBackgroundColor: TAlphaColor;
    FOnSelectSegment: TksSelectSegmentButtonEvent;
    FChanged: Boolean;
    FMouseUpCalled: Boolean;
    procedure SetItemIndex(const Value: integer);
    procedure SetSegments(const Value: TksSegmentButtonCollection);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetTintColor(const Value: TAlphaColor);
    function GetSelected: TKsSegmentButton;
    function GetSelectedID: string;
    procedure SetSelectedID(const Value: string);
    function ButtonFromPos(x,y: single): TksSegmentButton;
    procedure SetFontSize(const Value: integer);
  protected
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateButtons;
    procedure SelectSegmentByText(AText: string);
    property Selected: TKsSegmentButton read GetSelected;
    property SelectedID: string read GetSelectedID write SetSelectedID;
  published
    property Align;
    property FontSize: integer read FFontSize write SetFontSize default 14;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property Margins;
    property Padding;
    property Position;
    property Width;

    property TintColor: TAlphaColor read FTintColor write SetTintColor default claNull;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default claNull;
    property Segments: TksSegmentButtonCollection read FSegments write SetSegments;
    property Size;
    property Height;
    property Visible;
    // events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnMouseDown;
    property OnMouseUp;
    property OnSelectSegment: TksSelectSegmentButtonEvent read FOnSelectSegment write FOnSelectSegment;
  end;

  {$R *.dcr}

  procedure Register;

  {}


implementation

uses SysUtils,  Math, FMX.Forms,  FMX.TextLayout;

procedure Register;
begin
  RegisterComponents('Pentire FMX', [TksSegmentButtons]);
end;

function GetColorOrDefault(AColor, ADefaultIfNull: TAlphaColor): TAlphaColor;
begin
  Result := AColor;
  if Result = claNull then
    Result := ADefaultIfNull;
end;

{ TKsSegmentButton }

procedure TKsSegmentButton.Assign(Source: TPersistent);
begin
  if (Source is TKsSegmentButton) then
  begin
    FID := (Source as TKsSegmentButton).ID;
    FText := (Source as TKsSegmentButton).Text;
  end;
end;


constructor TKsSegmentButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FButton := TksSegmentSpeedButton.Create((Collection as TksSegmentButtonCollection).FSegmentButtons);
  FVisible := True;
end;

destructor TKsSegmentButton.Destroy;
begin
  {$IFDEF NEXTGEN}
  FButton.DisposeOf;
  {$ELSE}
  FButton.Free;
  {$ENDIF}
  inherited;
end;

function TKsSegmentButton.GetBadgeValue: integer;
begin
  Result := FButton.Badge;
end;

function TKsSegmentButton.GetBoundsRect: TRectF;
begin
  Result := FButton.BoundsRect;
end;

function TKsSegmentButton.GetIndex: integer;
begin
  Result := FButton.Index;
end;

procedure TKsSegmentButton.SetBadgeValue(const Value: integer);
begin
  FButton.Badge := Value;
end;

procedure TKsSegmentButton.SetText(const Value: string);
begin

  FText := Value;
  (Collection as TksSegmentButtonCollection).FSegmentButtons.UpdateButtons;

end;

procedure TKsSegmentButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    (Collection as TksSegmentButtonCollection).FSegmentButtons.UpdateButtons;
  end;
end;

{procedure TKsSegmentButton.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

 TksSegmentButtons }


procedure TksSegmentButtons.Assign(Source: TPersistent);
begin
  if (Source is TksSegmentButtons) then
  begin
    FGroupID := (Source as TksSegmentButtons).FGroupID;
    FItemIndex := (Source as TksSegmentButtons).FItemIndex;
    FBtnWidth := (Source as TksSegmentButtons).FBtnWidth;
    FOnChange := (Source as TksSegmentButtons).FOnChange;
    FTintColor := (Source as TksSegmentButtons).FTintColor;
    FBackgroundColor := (Source as TksSegmentButtons).FBackgroundColor;
    FSegments.Assign((Source as TksSegmentButtons).Segments);
  end;
end;

function TksSegmentButtons.ButtonFromPos(x, y: single): TksSegmentButton;
var
  ICount: integer;
  ABtn: TksSegmentButton;
begin
  Result := nil;
  for ICount := 0 to FSegments.Count-1 do
  begin
    ABtn := FSegments.Items[ICount] as TksSegmentButton;
    if PtInRect(ABtn.BoundsRect, PointF(x, y)) then
    begin
      Result := ABtn;
      Exit;
    end;
  end;
end;

constructor TksSegmentButtons.Create(AOwner: TComponent);
var
  AGuid: TGUID;
begin
  inherited;
  FSegments := TksSegmentButtonCollection.Create(Self);
  SetAcceptsControls(False);
  CreateGUID(AGuid);
  FGroupID := AGuid.ToString;
  FGroupID := StringReplace(FGroupID, '{', '', [rfReplaceAll]);
  FGroupID := StringReplace(FGroupID, '-', '', [rfReplaceAll]);
  FGroupID := StringReplace(FGroupID, '}', '', [rfReplaceAll]);
  FBackgroundColor := claNull;
  FTintColor := claNull;
  Size.Height := 50;
  Size.Width := 300;
  FFontSize := 14;
  FChanged := False;
end;

destructor TksSegmentButtons.Destroy;
begin
  FreeAndNil(FSegments);
  inherited;
end;



procedure TksSegmentButtons.DoMouseLeave;
begin
  inherited;
  if FMouseUpCalled then
    Exit;
  if FInitialIndex <> FItemIndex then
  begin
    FItemIndex := FInitialIndex;
    FChanged := False;
    UpdateButtons;

     {
    if Assigned(FOnSelectSegment) then
      FOnSelectSegment(Self, FItemIndex, Segments[FItemIndex]); }

  end;
end;

function TksSegmentButtons.GetSelected: TKsSegmentButton;
begin
  Result := nil;
  if (FItemIndex > -1) and (FItemIndex < FSegments.Count) then
    Result := FSegments[FItemIndex];
end;

function TksSegmentButtons.GetSelectedID: string;
begin
  Result := '';
  if Selected <> nil then
    Result := Selected.ID;
end;

procedure TksSegmentButtons.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
var
  ASegment: TksSegmentButton;
begin
  inherited;
  FMouseUpCalled := False;
  FInitialIndex := FItemIndex;
  HitTest := False;
  try
    FChanged := False;
    ASegment := ButtonFromPos(x, y);
    if ASegment <> nil then
    begin
      if ASegment.Index <> FItemIndex then
      begin
        ItemIndex := ASegment.Index;

      end;
    end;
    //Application.ProcessMessages;
  finally
    HitTest := True;
  end;
end;

procedure TksSegmentButtons.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  FMouseUpCalled := True;
  if FChanged then
  begin
    if Assigned(FOnSelectSegment) then
      FOnSelectSegment(Self, FItemIndex, Segments[FItemIndex]);
  end;
end;

procedure TksSegmentButtons.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) then
  begin
    DrawDesignBorder(claDimgray, claDimgray);
    if FSegments.Count = 0 then
      Canvas.FillText(ClipRect, 'Add "Segments" in the Object Inspector', True, 1, [], TTextAlign.Center);
  end;
end;

procedure TksSegmentButtons.Resize;
begin
  inherited;
  UpdateButtons;
end;

procedure TksSegmentButtons.UpdateButtons;
var
  ICount: integer;
  i: integer;
begin
  if FSegments.VisibleCount = 0 then
    Exit;

  FBtnWidth := (Width-16) / FSegments.VisibleCount;
  i := 0;
  for ICount := 0 to FSegments.Count-1 do
  begin
    if FSegments[ICount].FButton <> nil then
    begin
      FSegments[ICount].FButton.Visible := FSegments[ICount].Visible;
      if FSegments[Icount].Visible then
      begin
        if Assigned(FSegments[ICount].FButton) then
        begin
          if ContainsObject(FSegments[ICount].FButton) = False then
            AddObject(FSegments[ICount].FButton);

          with FSegments[ICount].FButton do
          begin

            Index := ICount;

            if Selected <> nil then
            begin
              if (Selected.Visible = False) and (FSegments.VisibleCount > 0) then
                ItemIndex := ICount;
            end;

            IsPressed := ICount = FItemIndex;
            Width := FBtnWidth;
            Height := 34;

            Text := FSegments[ICount].Text;
            Position.Y := (Self.Height - Height) / 2;
            Position.X := (i * FBtnWidth)+8;

            i := i + 1;
          end;
        end;
      end;
    end;
  end;
end;

procedure TksSegmentButtons.SelectSegmentByText(AText: string);
var
  ICount: integer;
  ASeg: TKsSegmentButton;
begin
  for ICount := 0 to FSegments.Count-1 do
  begin
    ASeg := FSegments.Items[ICount];
    if ASeg.Text = AText then
    begin
      ItemIndex := ICount;
      Exit;
    end;
  end;
end;

procedure TksSegmentButtons.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  UpdateButtons;
end;


procedure TksSegmentButtons.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
  UpdateButtons;
end;

procedure TksSegmentButtons.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FChanged := True;
    FItemIndex := Value;
    FInitialIndex := Value;
    UpdateButtons;
    Repaint;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TksSegmentButtons.SetSegments(const Value: TksSegmentButtonCollection);
begin
  FSegments.Assign(Value);
end;

procedure TksSegmentButtons.SetSelectedID(const Value: string);
var
  ICount: integer;
begin
  for ICount := 0 to Segments.Count-1 do
  begin
    if Segments[ICount].ID = Value then
    begin
      ItemIndex := ICount;
      Exit;
    end;
  end;
  ItemIndex := -1;
end;

procedure TksSegmentButtons.SetTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
  UpdateButtons;
end;

{ TksSegmentButtonCollection }

function TksSegmentButtonCollection.Add: TKsSegmentButton;
begin
  Result := inherited Add as TKsSegmentButton;
  //FSegmentButtons.UpdateButtons;
end;

constructor TksSegmentButtonCollection.Create(AButtons: TKsSegmentButtons);
begin
  inherited Create(TKsSegmentButton);
  FSegmentButtons := AButtons;
end;

function TksSegmentButtonCollection.GetItem(Index: Integer): TKsSegmentButton;
begin
  Result := inherited Items[index] as TKsSegmentButton;
end;

function TksSegmentButtonCollection.GetOwner: TPersistent;
begin
  Result := FSegmentButtons;
end;

function TksSegmentButtonCollection.Insert(Index: Integer): TKsSegmentButton;
begin
  Result := inherited insert( index ) as TKsSegmentButton;
  FSegmentButtons.UpdateButtons;
end;

procedure TksSegmentButtonCollection.SetItem(Index: Integer; Value: TKsSegmentButton);
begin
  inherited SetItem(index, Value);
end;

procedure TksSegmentButtonCollection.Update(Item: TCollectionItem);
begin
  inherited;
  (Owner as TksSegmentButtons).UpdateButtons;
end;

function TksSegmentButtonCollection.VisibleCount: integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to Count-1 do
  begin
    if Items[ICount].Visible then
      Result := Result + 1;
  end;
end;

{ TksSegmentSpeedButton }

procedure TksSegmentSpeedButton.Changed;
begin
  Repaint;
end;

constructor TksSegmentSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := (AOwner as TksSegmentButtons);
  Stored := False;
  HitTest := False;
end;

procedure RenderText(ACanvas: TCanvas; x, y, AWidth, AHeight: single;
  AText: string; AFont: TFont; ATextColor: TAlphaColor; AWordWrap: Boolean;
  AHorzAlign: TTextAlign; AVertAlign: TTextAlign; ATrimming: TTextTrimming;
  const APadding: single = 0); overload;
var
  ATextLayout: TTextLayout;
begin
  if AText = '' then
    Exit;
  ATextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    ATextLayout.BeginUpdate;
    ATextLayout.Text := AText;
    ATextLayout.WordWrap := AWordWrap;
    ATextLayout.Font.Assign(AFont);
    ATextLayout.Color := ATextColor;
    ATextLayout.HorizontalAlign := AHorzAlign;
    ATextLayout.VerticalAlign := AVertAlign;
    ATextLayout.Padding.Rect := RectF(APadding, APadding, APadding, APadding);
    ATextLayout.Trimming := ATrimming;
    if AWordWrap  then
      ATextLayout.Trimming := TTextTrimming.None;
    ATextLayout.TopLeft := PointF(x, y);
    ATextLayout.MaxSize := PointF(AWidth, AHeight);
    ATextLayout.EndUpdate;
    ATextLayout.RenderLayout(ACanvas);
  finally
    ATextLayout.Free;
  end;
end;

procedure RenderText(ACanvas: TCanvas; ARect: TRectF;
  AText: string; AFont: TFont; ATextColor: TAlphaColor; AWordWrap: Boolean;
  AHorzAlign: TTextAlign; AVertAlign: TTextAlign; ATrimming: TTextTrimming;
  const APadding: single = 0); overload;
begin
  RenderText(ACanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height, AText, AFont, ATextColor, AWordWrap, AHorzAlign, AVertAlign, ATrimming, APadding);
end;


procedure TksSegmentSpeedButton.Paint;
begin
  inherited;
  Canvas.Stroke.Color := claBlack;
  if FIsPressed then
    Canvas.Fill.Color := GetColorOrDefault(FOwner.TintColor, claDodgerblue)
  else
    Canvas.Fill.Color := FOwner.BackgroundColor;
  Canvas.Stroke.Color := GetColorOrDefault(FOwner.TintColor, claDodgerblue);
  Canvas.Stroke.Kind := TBrushKind.Solid;

  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
  Canvas.DrawRect(ClipRect, 0, 0, AllCorners, 1);

  if FIsPressed then
    Canvas.Fill.Color := claWhite
  else
    Canvas.Fill.Color := GetColorOrDefault(FOwner.TintColor, claDodgerblue);

  Canvas.Font.Size := FOwner.FontSize;

  RenderText(Canvas, ClipRect, FText, Canvas.Font, Canvas.Fill.Color, False, TTextAlign.Center, TTextAlign.Center, TTextTrimming.Character);
end;

procedure TksSegmentSpeedButton.SetBadge(const Value: integer);
begin
  FBadge := Value;
  Changed;
end;

procedure TksSegmentSpeedButton.SetIsPressed(const Value: Boolean);
begin
  FIsPressed := Value;
  Changed;
end;

procedure TksSegmentSpeedButton.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;

end.

