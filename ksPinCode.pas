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

unit ksPinCode;

interface

uses {$IFDEF MSWINDOWS} Windows, {$ENDIF} Classes, FMX.StdCtrls, FMX.Graphics, FMX.Objects, ksProgressIndicator,
  System.UITypes, System.UIConsts, FMX.Types, FMX.Controls, System.Types;

type
  TksPinCodeSubmitEvent = procedure(Sender: TObject; ACode: string) of object;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksPinCode = class(TControl)
  private
    FIndicator: TksProgressIndicator;
    FTintColor: TAlphaColor;
    FLogo: TBitmap;
    FKeyRects: array[1..12] of TRectF;
    FCode: string;
    FPrompt: string;
    FOnSubmit: TksPinCodeSubmitEvent;
    FPinLength: integer;
    procedure SetTintColor(const Value: TAlphaColor);
    procedure SetLogo(const Value: TBitmap);
    function ButtonIndexFromPos(x, y: single): integer;
    procedure DoKeyPressed(AKey: string);
    procedure DoDeletePressed;
    procedure DoSubmit;
    procedure SetPrompt(const Value: string);
    procedure SetPinLength(const Value: integer);
  protected
    procedure Paint; override;
  protected
    procedure Resize; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
  published
    property Align;
    property Logo: TBitmap read FLogo write SetLogo;
    property Margins;
    property Padding;
    property Position;
    property Width;
    property TintColor: TAlphaColor read FTintColor write SetTintColor default $FF336A97;
    property Size;
    property Height;
    property PinLength: integer read FPinLength write SetPinLength default 4;
    property Prompt: string read FPrompt write SetPrompt;
    property Visible;
    property OnSubmit: TksPinCodeSubmitEvent read FOnSubmit write FOnSubmit;
  end;

  procedure Register;

implementation

uses Math, System.TypInfo, SysUtils, System.Threading,
  Fmx.Forms, ksCommon;

procedure Register;
begin
  RegisterComponents('Pentire FMX', [TksPinCode]);
end;
{ TksSpeedButton }

procedure TksPinCode.Reset;
begin
  FCode := '';
  FIndicator.Steps.CurrentStep := 0;
  Repaint;
end;

constructor TksPinCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndicator := TksProgressIndicator.Create(Self);
  FIndicator.Stored := False;
  FTintColor := $FF336A97;
  FIndicator.ActiveColor := FTintColor;
  FIndicator.OutlineColor := FTintColor;
  FIndicator.InactiveColor := claWhite;
  FIndicator.Steps.Size := 28;
  FIndicator.Steps.CurrentStep := 0;
  FIndicator.KeepHighlighted := True;
  FLogo := TBitmap.Create;
  Size.Width := 320;
  Size.Height := 480;
  AddObject(FIndicator);
  FCode := '';
  FPrompt := 'Enter PIN';
  FPinLength := 4;
  FIndicator.Steps.MaxSteps := FPinLength;
end;

destructor TksPinCode.Destroy;
begin
  FIndicator.DisposeOf;
  FreeAndNil(FLogo);
  inherited;
end;

procedure TksPinCode.DoDeletePressed;
begin
  if Length(FCode) > 0 then
    FCode := Copy(FCode, 1, Length(FCode)-1);
  FIndicator.Steps.CurrentStep := Length(FCode);
  //DoVibrate;
  Repaint;
end;

procedure TksPinCode.DoKeyPressed(AKey: string);
begin
  if Length(FCode) < 4 then
  begin
    FCode := FCode + AKey;
    FIndicator.Steps.CurrentStep := Length(FCode);
    //DoVibrate;
    {$IFDEF MSWINDOWS}
    OutputDebugString(PChar('Key Index Pressed: '+AKey));
    {$ENDIF}
  end;

  //Sleep(1000);
  //FIndicator.Visible := True;


  Repaint;

  if Length(Fcode) = 4 then
  begin
    Application.ProcessMessages;
    Sleep(200);
    DoSubmit;
  end;
end;

procedure TksPinCode.DoSubmit;
begin
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar('Code entered: '+FCode));
  {$ENDIF}
  if Assigned(FOnSubmit) then
    FOnSubmit(Self, FCode);
end;

function TksPinCode.ButtonIndexFromPos(x, y: single): integer;
var
  ICount: integer;
begin
  Result := -1;
  for ICount := 1 to 12 do
  begin
    if PtInRect(FKeyRects[ICount], PointF(x, y)) then
    begin
      Result := ICount-1;
      Exit;
    end;
  end;
end;

procedure TksPinCode.Loaded;
begin
  inherited;
  Resize;
end;

procedure TksPinCode.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  AIndex: integer;
begin
  inherited;
  AIndex := ButtonIndexFromPos(x, y);
  if AIndex > -1 then
  begin  
    case AIndex of
      0..8: DoKeyPressed(IntToStr(AIndex+1));
      10: DoKeyPressed('0');
      11: DoDeletePressed;
    end;
  end;
end;

procedure TksPinCode.Paint;
var
  rectKeypad: TRectF;
  rectText: TRectF;
  rectLogo: TRectF;
  ABtnWidth, ABtnHeight: single;
  ICount: integer;
  x,y: single;
  ABtnText: string;
begin
  inherited;
  if Locked then
    Exit;
  Canvas.Fill.Color := claWhite;
  Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);
  if (csDesigning in ComponentState) then
  begin
    DrawDesignBorder(claDimgray, claDimgray);
  end;
  rectKeypad := ClipRect;
  rectKeypad.Top := (rectKeypad.Height / 10) * 6;
  Canvas.Fill.Color := FTintColor;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(rectKeypad, 0, 0, AllCorners, 1);
  rectText := RectF(0, 0, Width, ((Height / 10) * 6) - 110);
  rectLogo := RectF((Width/2)-32, (rectText.Bottom/2)-32, (Width/2)+32, (rectText.Bottom/2)+32);

  begin
    if (csDesigning in ComponentState) then
    begin
      Canvas.Stroke.Dash := TStrokeDash.Dash;
      Canvas.Stroke.Thickness := GetScreenScale;
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.DrawRect(rectLogo, 0, 0, AllCorners, 1);
    end;
    if FLogo <> nil then
      Canvas.DrawBitmap(FLogo, RectF(0, 0, FLogo.Width, FLogo.Height), rectLogo, 1);
  end;
  Canvas.Font.Size := 20;
  Canvas.FillText(rectText, FPrompt, False, 1, [], TTextAlign.Center, TTextAlign.Trailing);
  
  ABtnWidth := rectKeypad.Width /3;
  ABtnHeight := rectKeypad.Height / 4;
  Canvas.Fill.Color := claWhite;
  x := 0;
  y := rectKeypad.Top;
  for ICount := 1 to 12 do
  begin
    case ICount of
      1..9: ABtnText := InTtoStr(ICount);
      10: ABtnText := '';
      11: ABtnText := '0';
      12: ABtnText := 'DEL';
    end;

    FKeyRects[ICount] := RectF(x, y, x+ABtnWidth, y+ABtnHeight);

    Canvas.FillText(FKeyRects[ICount], ABtnText, False, 1, [], TTextAlign.Center, TTextAlign.Center);
    x := x + ABtnWidth;
    if ICount mod 3 = 0 then
    begin
      y := y + ABtnHeight;
      x := 0;
    end;
  end;
end;
procedure TksPinCode.Resize;
begin
  inherited;
  FIndicator.Position.X := 0;
  FIndicator.Size.Width := Width;
  FIndicator.Position.Y := ((Height / 10) * 6) - 100;
end;

procedure TksPinCode.SetLogo(const Value: TBitmap);
begin
  FLogo := Value;
  Repaint;
end;

procedure TksPinCode.SetPinLength(const Value: integer);
begin
  FPinLength := Value;
  FIndicator.Steps.MaxSteps := Value;
  Repaint;
end;

procedure TksPinCode.SetPrompt(const Value: string);
begin
  if FPrompt <> Value then
  begin
    FPrompt := Value;
    Repaint;
  end;
end;

procedure TksPinCode.SetTintColor(const Value: TAlphaColor);
begin
  FTintColor := Value;
  Repaint;
end;

end.
