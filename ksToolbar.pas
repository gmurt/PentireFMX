{*******************************************************************************
*                                                                              *
*  PentireFMX                                                                  *                    formst
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

unit ksToolBar;

interface

uses
  Classes, FMX.StdCtrls, FMX.Graphics, FMX.Objects, FMX.Types,
  System.UITypes, System.UIConsts, FMX.Controls.Presentation, FMX.Controls,
  System.Types;

type
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksToolbar = class(TPresentedControl)
  private
    FButton: TSpeedButton;
    FTintColor: TAlphaColor;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FButtonColor: TAlphaColor;
    FText: string;
    FBackButtonText: string;
    FOnMenuButtonClick: TNotifyEvent;
    FShowMenuButton: Boolean;
    FOnBackButtonClick: TNotifyEvent;
    FBackButtonEnabled: Boolean;
    FShowBackButton: Boolean;
    FFirstPaint: Boolean;
    procedure Changed(Sender: TObject);
    procedure ButtonClicked(Sender: TObject);

    procedure SetShowMenuButton(const Value: Boolean);
    procedure SetTintColor(const Value: TAlphaColor);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetShowBackButton(const Value: Boolean);
    procedure SetButtonColor(const Value: TAlphaColor);
    procedure UpdateButton;
    procedure SetBackButtonText(const Value: string);
  protected
    procedure Paint; override;
    function GetDefaultSize: TSizeF; override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableBackButton;
    procedure EnableBackButton;
  published
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property Size;
    property TabOrder;

    property BackButtonText: string read FBackButtonText write SetBackButtonText;
    property TintColor: TAlphaColor read FTintColor write SetTintColor default claWhitesmoke;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default claBlack;
    property ButtonColor: TAlphaColor read FButtonColor write SetButtonColor default claDodgerblue;
    property ShowMenuButton: Boolean read FShowMenuButton write SetShowMenuButton default True;
    property ShowBackButton: Boolean read FShowBackButton write SetShowBackButton default True;
    property Padding;

    property OnClick;
    property OnMenuButtonClick: TNotifyEvent read FOnMenuButtonClick write FOnMenuButtonClick;
    property OnBackButtonClick: TNotifyEvent read FOnBackButtonClick write FOnBackButtonClick;
  end;


  procedure Register;

implementation

uses Math, System.TypInfo, SysUtils, ksPickers, FMX.DialogService,
  FMX.Platform, FMX.Forms, FMX.VirtualKeyboard, ksFormStack;

procedure Register;
begin
  RegisterComponents('Pentire FMX', [TksToolbar]);
end;



procedure HideKeyboard;
var
  KeyboardService: IFMXVirtualKeyboardService;
begin
  try
    if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(KeyboardService)) then
      KeyboardService.HideVirtualKeyboard;
  except

  end;
end;


{ TksToolbar }

procedure TksToolbar.ButtonClicked(Sender: TObject);
begin
  try
    if GlobalFormStack.Depth > 1 then
    begin

      if Assigned(FOnBackButtonClick) then
      begin

        FOnBackButtonClick(Self);

      end
      else
      begin

        GlobalFormStack.Pop;

      end;
    end
    else
    begin
      if Assigned(FOnMenuButtonClick) then
        FOnMenuButtonClick(Self);
    end;
  except
    //
  end;
end;



procedure TksToolbar.Changed(Sender: TObject);
begin
  InvalidateRect(ClipRect);
end;

constructor TksToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FFirstPaint := True;
  FButton := TSpeedButton.Create(Self);
  FButton.Align := TAlignLayout.Left;
  FButton.StyleLookup := 'detailstoolbutton';
  FButton.Width := 56;
  FButton.TextSettings.Trimming := TTextTrimming.None;
  FButton.TouchTargetExpansion.Rect := Rect(4, 4, 4, 4);
  FButtonColor := claDodgerblue;
  FButton.IconTintColor := claDodgerblue;
  FButton.TintColor := FButtonColor;
  FButton.CanFocus := True;
  FButton.Stored := False;
  FButton.CanFocus := False;
  FButton.StyledSettings := [TStyledSetting.Family,TStyledSetting.Style,TStyledSetting.FontColor];
  FButton.OnClick := ButtonClicked;
  AddObject(FButton);

  FFont := TFont.Create;
  Align := TAlignLayout.MostTop;

  FTintColor := $FFF9F9F9;
  FTextColor := claBlack;

  FFont.OnChanged := Changed;

  FShowMenuButton := True;
  FShowBackButton := True;
  FBackButtonEnabled := True;
  FBackButtonText := 'Back';
end;

destructor TksToolbar.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TksToolbar.DisableBackButton;
begin
  FBackButtonEnabled := False;
  UpdateButton;
end;

procedure TksToolbar.DoMouseLeave;
begin
  inherited;
end;

procedure TksToolbar.EnableBackButton;
begin
  FBackButtonEnabled := True;
  UpdateButton;
end;

function TksToolbar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(120, 44);
end;

procedure TksToolbar.Paint;
var
  AState: TCanvasSaveState;
begin
  inherited;
  FFirstPaint := False;
  UpdateButton;

  AState := Canvas.SaveState;
  try
    Canvas.IntersectClipRect(ClipRect);
    Canvas.Fill.Color := FTintColor;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);


    Canvas.Font.Assign(FFont);
    Canvas.Fill.Color := FTextColor;

    Canvas.FillText(ClipRect, FText, False, 1, [], TTextAlign.Center);

    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Color := claDimgray;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.DrawLine(PointF(0, Height), PointF(Width, Height), 1);
    FButton.Repaint;
  finally
    Canvas.RestoreState(AState);
  end;
end;

procedure TksToolbar.SetBackButtonText(const Value: string);
begin
  if FBackButtonText <> Value then
  begin
    FBackButtonText := Value;
    UpdateButton;
  end;
end;

procedure TksToolbar.SetButtonColor(const Value: TAlphaColor);
begin
  FButtonColor := Value;
  UpdateButton;
end;

procedure TksToolbar.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksToolbar.SetShowBackButton(const Value: Boolean);
begin
  FShowBackButton := Value;
  UpdateButton
end;

procedure TksToolbar.SetShowMenuButton(const Value: Boolean);
begin
  FShowMenuButton := Value;
  UpdateButton;
end;

procedure TksToolbar.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Repaint;
  end;
end;

procedure TksToolbar.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
  UpdateButton;
end;

procedure TksToolbar.SetTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then
  begin
    FTintColor := Value;
    UpdateButton;
  end;
end;

procedure TksToolbar.UpdateButton;
begin
  if Root is TCommonCustomForm then
  begin
    if GlobalFormStack.Depth = 1 then
    begin
      FButton.Visible := FShowMenuButton;
      if FButton.StyleLookup <> 'detailstoolbutton' then
        FButton.StyleLookup := 'detailstoolbutton';
    end
    else
    begin
      FButton.Visible := FShowBackButton;
      if FButton.StyleLookup <> 'backtoolbutton' then
        FButton.StyleLookup := 'backtoolbutton';

      FButton.Text := FBackButtonText;
      FButton.Width := 60;
      FButton.Font.Size := 16;
    end;
  end;
end;

initialization

  Classes.RegisterClass(TksToolbar);

end.
