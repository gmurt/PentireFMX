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

unit ksCircleProgress;

interface

uses Classes, FMX.Graphics, System.UITypes, System.UIConsts, FMX.Controls;

type
	TksCircleProgressCaptionType = (ksCirclePercent, ksCircleNone,
		ksCircleCustom);

  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    pidiOSDevice32 or pidiOSDevice64 or
    pidiOSSimulator32 or pidiOSSimulator64 or
    pidAndroid32Arm or pidAndroid64Arm
    )]

	TksCircleProgress = class(TControl)
	private
		FBitmap: TBitmap;
		FValue, FValue2: single;
		FBackgroundColor: TAlphaColor;
		FColor, FColor2: TAlphaColor;
		FCaptionType: TksCircleProgressCaptionType;
		FText: string;
		FThickness: integer;
		procedure RecreateBitmap;
		procedure SetValue(const Value: single);
		procedure SetColor(const Value: TAlphaColor);
    procedure SetValue2(const Value: single);
		procedure SetColor2(const Value: TAlphaColor);
		procedure SetBackgroundColor(const Value: TAlphaColor);
		procedure SetCaptionType(const Value: TksCircleProgressCaptionType);
		procedure SetText(const Value: string);
		procedure SetThickness(const Value: integer);
	protected
		procedure Paint; override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure AnimateToValue(AValue: single; const ADurationSecs: integer = 1);
	published
		property Align;
		property CaptionType: TksCircleProgressCaptionType read FCaptionType
			write SetCaptionType default ksCirclePercent;
		property Height;
		property Width;
		property Size;
		property Margins;
		property Padding;
		property Position;
		property BackgroundColor: TAlphaColor read FBackgroundColor
			write SetBackgroundColor default claGainsboro;
		property Color: TAlphaColor read FColor write SetColor
			default claDodgerblue;
		property Value: single read FValue write SetValue;

    property Color2: TAlphaColor read FColor2 write SetColor2
			default claDarkmagenta;
		property Value2: single read FValue2 write SetValue2;


		property Text: string read FText write SetText;
		property Thickness: integer read FThickness write SetThickness default 15;
		property Visible;
	end;

procedure Register;

implementation

uses Math, SysUtils, Types, FMX.Types, FMX.Ani,
	FMX.Forms;

const
	C_SCALE = 3;

procedure Register;
begin
	RegisterComponents('Pentire FMX', [TksCircleProgress]);
end;

{ TksCircleProgress }

procedure TksCircleProgress.AnimateToValue(AValue: single;
	const ADurationSecs: integer = 1);
begin
	TAnimator.AnimateFloat(Self, 'Value', AValue, ADurationSecs);
end;

constructor TksCircleProgress.Create(AOwner: TComponent);
begin
	inherited;
	FBitmap := TBitmap.Create;
	FCaptionType := TksCircleProgressCaptionType.ksCirclePercent;
	FColor := claDodgerblue;
	FBackgroundColor := claGainsboro;
	FText := '';
	FValue := 0;
  FValue2 := 0;
	Width := 150;
	Height := 150;
	FThickness := 15;
	RecreateBitmap;
end;

destructor TksCircleProgress.Destroy;
begin
	FreeAndNil(FBitmap);
	inherited;
end;

procedure TksCircleProgress.Paint;
var
	r: TRectF;
	ACaption: string;
begin
	inherited;
	if Locked then
		Exit;
	r := RectF(0, 0, Width, Height);
	Canvas.BeginScene;
	Canvas.DrawBitmap(FBitmap, RectF(0, 0, FBitmap.Width, FBitmap.Height),
		r, 1, True);
	ACaption := '';
	case FCaptionType of
		ksCirclePercent:
			ACaption := ' ' + InTToStr(Round(FValue)) + '%';
		ksCircleCustom:
			ACaption := Text;
	end;
	Canvas.Fill.Color := FColor;
	Canvas.Font.Size := 24;
	Canvas.FillText(ClipRect, ACaption, False, 1, [], TTextAlign.Center,
		TTextAlign.Center);
	Canvas.EndScene;
end;

procedure TksCircleProgress.RecreateBitmap;
var
	AAngle: single;
	x1, y1, x2, y2, x3, y3: single;
	AThickness: integer;
	AScale: single;
begin
	AScale := Min(C_SCALE, 2); // GetScreenScale(False);
	FBitmap.SetSize(Round(Width * AScale), Round(Height * AScale));
	FBitmap.Clear(claNull);
	FBitmap.Canvas.BeginScene;
	try
		AAngle := 0;
		x1 := Round((Width * AScale) / 2);
		y1 := Round((Height * AScale) / 2);

		AThickness := Round(FThickness * AScale);

		FBitmap.Canvas.Stroke.Thickness := 4;
		FBitmap.Canvas.Stroke.Color := FBackgroundColor;
		FBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
		while AAngle < 360 do
		begin

			x2 := x1 + (cos(DegToRad(AAngle - 90)) * (((Width * AScale) / 2) -
				AThickness));
			y2 := y1 + (sin(DegToRad(AAngle - 90)) * (((Height * AScale) / 2) -
				AThickness));
			x3 := x1 + (cos(DegToRad(AAngle - 90)) * (((Width * AScale) / 2) - 4));
			y3 := y1 + (sin(DegToRad(AAngle - 90)) * (((Height * AScale) / 2) - 4));

			FBitmap.Canvas.DrawLine(PointF(x2, y2), PointF(x3, y3), 1);
			AAngle := AAngle + 1;
		end;

   	AAngle := 0;
		FBitmap.Canvas.Stroke.Thickness := 4;
		FBitmap.Canvas.Stroke.Color := FColor2;
		FBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;

    while AAngle < ((360 / 100) * FValue2) do
		begin

			x2 := x1 + (cos(DegToRad(AAngle - 90)) * (((Width * AScale) / 2) -
				AThickness));
			y2 := y1 + (sin(DegToRad(AAngle - 90)) * (((Height * AScale) / 2) -
				AThickness));
			x3 := x1 + (cos(DegToRad(AAngle - 90)) * (((Width * AScale) / 2) - 4));
			y3 := y1 + (sin(DegToRad(AAngle - 90)) * (((Height * AScale) / 2) - 4));

			FBitmap.Canvas.DrawLine(PointF(x2, y2), PointF(x3, y3), 1);
			AAngle := AAngle + 1;
		end;

    AAngle := 0;
   	FBitmap.Canvas.Stroke.Thickness := 4;
		FBitmap.Canvas.Stroke.Color := FColor;
		FBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;

		while AAngle < ((360 / 100) * FValue) do
		begin

			x2 := x1 + (cos(DegToRad(AAngle - 90)) * (((Width * AScale) / 2) -
				AThickness));
			y2 := y1 + (sin(DegToRad(AAngle - 90)) * (((Height * AScale) / 2) -
				AThickness));
			x3 := x1 + (cos(DegToRad(AAngle - 90)) * (((Width * AScale) / 2) - 4));
			y3 := y1 + (sin(DegToRad(AAngle - 90)) * (((Height * AScale) / 2) - 4));

			FBitmap.Canvas.DrawLine(PointF(x2, y2), PointF(x3, y3), 1);
			AAngle := AAngle + 1;
		end;




	finally
		FBitmap.Canvas.EndScene;
	end; // }
end;

procedure TksCircleProgress.SetBackgroundColor(const Value: TAlphaColor);
begin
	if FBackgroundColor <> Value then
	begin
		FBackgroundColor := Value;
		RecreateBitmap;
		InvalidateRect(ClipRect);
	end;
end;

procedure TksCircleProgress.SetCaptionType(const Value
	: TksCircleProgressCaptionType);
begin
	if FCaptionType <> Value then
	begin
		FCaptionType := Value;
		RecreateBitmap;
		InvalidateRect(ClipRect);
	end;
end;

procedure TksCircleProgress.SetColor(const Value: TAlphaColor);
begin
	if FColor <> Value then
	begin
		FColor := Value;
		RecreateBitmap;
		InvalidateRect(ClipRect);
	end;
end;

procedure TksCircleProgress.SetColor2(const Value: TAlphaColor);
begin
	if FColor2 <> Value then
	begin
		FColor2 := Value;
		RecreateBitmap;
		InvalidateRect(ClipRect);
	end;

end;

procedure TksCircleProgress.SetText(const Value: string);
begin
	if FText <> Value then
	begin
		FText := Value;
		InvalidateRect(ClipRect);
	end;
end;

procedure TksCircleProgress.SetThickness(const Value: integer);
begin
	if FThickness <> Value then
	begin
		FThickness := Value;
		RecreateBitmap;
		InvalidateRect(ClipRect);
	end;
end;

procedure TksCircleProgress.SetValue(const Value: single);
begin
	if FValue <> Value then
	begin
		FValue := Value;
		FValue := Max(FValue, 0);
		FValue := Min(FValue, 100);
		RecreateBitmap;
		Repaint;
{$IFDEF ANDROID}
		Application.ProcessMessages;
{$ENDIF}
	end;
end;

procedure TksCircleProgress.SetValue2(const Value: single);
begin
  if FValue2 <> Value then
	begin
		FValue2 := Value;
		FValue2 := Max(FValue2, 0);
		FValue2 := Min(FValue2, 100);
		RecreateBitmap;
		Repaint;
{$IFDEF ANDROID}
		Application.ProcessMessages;
{$ENDIF}
	end;
end;

end.
