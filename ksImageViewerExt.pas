{*******************************************************************************
*                                                                              *
*  TksImageViewerExt                                                           *
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

unit ksImageViewerExt;

interface

uses Classes, Types, FMX.InertialMovement, System.UITypes, 
  FMX.Graphics, FMX.Controls, FMX.Types, FMX.Objects;

type
  [ComponentPlatformsAttribute(
    pidAllPlatforms
    )]

  TksImageViewerExt = class(TControl)
  private
    FBitmap: TBitmap;
    FZoom: single;
    FAni: TAniCalculations;
    FMouseDownXY: TPointF;
    FPanning: Boolean;
    FStartDistance: integer;
    FStartZoom: single;
    procedure SetBitmap(const Value: TBitmap);

    procedure SetZoom(AValue: single);
    procedure DoPan(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ZoomIn;
    procedure ZoomOut;

  published
    property Align;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Zoom: single read FZoom write SetZoom;
    property Position;
    property Width;
    property Height;
    property Size;
    property Touch;
  end;

  procedure Register;

implementation

uses System.UIConsts, SysUtils, Math;


procedure Register;
begin
  RegisterComponents('Pentire FMX', [TksImageViewerExt]);
end;


{ TksImageViewer }

constructor TksImageViewerExt.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create;
  FAni := TAniCalculations.Create(Self);
  FAni.OnChanged := DoPan;
  FAni.BoundsAnimation := True;
  FAni.Animation := True;
  FZoom := 100;
  Width := 400;
  Height := 300;
end;


destructor TksImageViewerExt.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FAni);
  inherited;
end;



procedure TksImageViewerExt.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  ANewZoom: Extended;
begin
  inherited;

  if EventInfo.Distance > 0 then
    FAni.MouseLeave;
  if FStartDistance = 0 then
    FStartDistance := EventInfo.Distance;
  if FStartDistance > 0 then
  begin
    ANewZoom := FStartZoom*(EventInfo.Distance / FStartDistance);
    if ANewZoom < 1 then
      Exit;
    if (ANewZoom >= 10) and (ANewZoom <= 400) then
      Zoom := ANewZoom;
  end;
end;

procedure TksImageViewerExt.DoMouseLeave;
begin
  inherited;
  FAni.MouseLeave;
end;

procedure TksImageViewerExt.DoPan(Sender: TObject);
begin
  inherited;
  Repaint;
end;

procedure TksImageViewerExt.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseDownXY := PointF(X, Y);
  FAni.MouseDown(X, Y);
  FPanning := False;
  FStartDistance := 0;
  FStartZoom := FZoom;
end;

procedure TksImageViewerExt.MouseMove(Shift: TShiftState; X, Y: Single);
var
  ARect: TRectF;
begin
  inherited;
  ARect := RectF(FMouseDownXY.X-50, FMouseDownXY.Y-50, FMouseDownXY.X+50, FMouseDownXY.Y+50);
  if PtInRect(ARect, PointF(x, y)) then
    FPanning := True;
  if FPanning then
    FAni.MouseMove(X, Y)
  else
    FAni.MouseLeave;
end;

procedure TksImageViewerExt.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FAni.MouseUp(X, Y);
  FStartDistance := 0;
end;

procedure TksImageViewerExt.Paint;
var
  AState: TCanvasSaveState;
  ARect: TRectF;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder(DesignBorderColor, DesignBorderColor);
  AState := Canvas.SaveState;

  try
    ARect := RectF(0, 0, FBitmap.Width, FBitmap.Height);
    ARect.Offset(0-FAni.ViewportPosition.X, 0-FAni.ViewportPosition.Y);
    ARect.Width := FBitmap.Width * (FZoom / 100);
    ARect.Height := FBitmap.Height * (FZoom / 100);

    if ARect.Width < Width then
      ARect.Offset((Width - ARect.Width) / 2, 0);

    if ARect.Height < Height then ARect.Offset(0, (Height - ARect.Height) / 2);

    Canvas.IntersectClipRect(ClipRect);
    Canvas.DrawBitmap(FBitmap,
                      RectF(0, 0, FBitmap.Width, FBitmap.Height),
                      ARect,
                      1);
  finally
    Canvas.RestoreState(AState);
  end;
end;


procedure TksImageViewerExt.Resize;
begin
  inherited;
  SetZoom(FZoom);
end;

procedure TksImageViewerExt.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  InvalidateRect(ClipRect);
end;


procedure TksImageViewerExt.SetZoom(AValue: single);
var
  Targets: array of TAniCalculations.TTarget;
  AXPercent: Extended;
  AYPercent: Extended;
begin
  AXPercent := ((FAni.ViewportPosition.X) / ((FBitmap.Width*(FZoom/100))-(Width))) * 100;
  AYPercent := ((FAni.ViewportPosition.Y) / ((FBitmap.Height*(FZoom/100))-(Height))) * 100;

  if (FBitmap.Width * (FZoom/100)) < Width then AXPercent := 50;
  if (FBitmap.Height * (FZoom/100)) < Height then AYPercent := 50;

  FZoom := AValue;
  SetLength(Targets, 2);
  Targets[0].TargetType := TAniCalculations.TTargetType.Min;
  Targets[0].Point := TPointD.Create(0, 0);
  Targets[1].TargetType := TAniCalculations.TTargetType.Max;
  Targets[1].Point := TPointD.Create(Max(((FBitmap.Width*(FZoom/100))-Width), 0),
                                     Max(((FBitmap.Height*(FZoom/100))-Height), 0));

  FAni.SetTargets(Targets);

  FAni.ViewportPositionF := PointF((Targets[1].Point.X / 100) * AXPercent, (Targets[1].Point.Y / 100) * AYPercent);
  Repaint;
end;

procedure TksImageViewerExt.ZoomIn;
begin
  if Zoom > 20 then
    Zoom := Zoom + 10
  else
    Zoom := Zoom + 5;
end;

procedure TksImageViewerExt.ZoomOut;
begin
  if Zoom > 20 then
    Zoom := Zoom - 10
  else
    Zoom := Max(Zoom - 5, 5);
end;

end.
