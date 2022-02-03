{ *******************************************************************************
*                                                                              *
*  TksProgressIndicator                                                          *
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
*  See the License forthe specific language governing permissions and          *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksProgressIndicator;

interface


uses FMX.Controls, Classes, FMX.StdCtrls, FMX.Graphics, FMX.Types, FMX.Objects,
  System.UIConsts, System.UITypes;


type
  TksProgressIndicator = class;

  TksProgressIndicatorSteps = class(TPersistent)
  private
    [weak]FOwner: TksProgressIndicator;
    FMaxSteps: integer;
    FCurrentStep: integer;
    FSize: integer;
    FVisible: Boolean;
    procedure SetCurrentStep(const Value: integer);
    procedure SetMaxSteps(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure Changed;
    procedure SetSize(const Value: integer);
  public
    constructor Create(AOwner: TksProgressIndicator);
  published
    property MaxSteps: integer read FMaxSteps write SetMaxSteps default 5;
    property CurrentStep: integer read FCurrentStep write SetCurrentStep default 1;
    property Size: integer read FSize write SetSize default 12;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksProgressIndicator = class(TControl)
  private
    FSteps: TksProgressIndicatorSteps;
    FActiveColor: TAlphaColor;
    FInActiveColor: TAlphaColor;
    FBitmap: TBitmap;
    FOutlineColor: TAlphaColor;
    FKeepHighlighted: Boolean;
    procedure SetActiveColor(const Value: TAlphaColor);
    procedure SetInActiveColor(const Value: TAlphaColor);
    procedure Redraw;

    procedure Changed;
  protected
    procedure Paint; override;
    procedure Resize; override;

    procedure SetOutlineColor(const Value: TAlphaColor);
    procedure SetKeepHighlighted(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActiveColor: TAlphaColor read FActiveColor write SetActiveColor default claDodgerblue;
    property InactiveColor: TAlphaColor read FInActiveColor write SetInActiveColor default claSilver;
    property KeepHighlighted: Boolean read FKeepHighlighted write SetKeepHighlighted default False;
    property Align;
    property Position;
    property Width;
    property Height;
    property Steps: TksProgressIndicatorSteps read FSteps write FSteps;
    property OutlineColor: TAlphaColor read FOutlineColor write SetOutlineColor default claNull;
  end;

  procedure Register;

implementation

uses Types, SysUtils, FMX.Platform, ksCommon;


procedure Register;
begin
  RegisterComponents('Pentire FMX', [TksProgressIndicator]);
end;



{ TksProgressIndicatorSteps }

procedure TksProgressIndicatorSteps.Changed;
begin
  FOwner.Changed;
end;

constructor TksProgressIndicatorSteps.Create(AOwner: TksProgressIndicator);
begin
  inherited Create;
  FOwner := AOwner;
  FMaxSteps := 5;
  FCurrentStep := 1;
  FSize := 12;
  FVisible := True;
end;

procedure TksProgressIndicatorSteps.SetCurrentStep(const Value: integer);
begin
  FCurrentStep := Value;
  Changed;
end;

procedure TksProgressIndicatorSteps.SetMaxSteps(const Value: integer);
begin
  FMaxSteps := Value;
  Changed;

end;

procedure TksProgressIndicatorSteps.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TksProgressIndicatorSteps.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TksProgressIndicator }

procedure TksProgressIndicator.Changed;
begin
  Redraw;
  InvalidateRect(ClipRect);
end;

constructor TksProgressIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FSteps := TksProgressIndicatorSteps.Create(Self);
  FBitmap := TBitmap.Create;
  Size.Width := 200;
  Size.Height := 40;
  FActiveColor := claDodgerblue;
  FInActiveColor := claSilver;
  FKeepHighlighted := False;
  Redraw;
end;

destructor TksProgressIndicator.Destroy;
begin
  FreeAndNil(FSteps);
  FreeAndNil(FBitmap);
  inherited;
end;


procedure TksProgressIndicator.Paint;
begin
  inherited;
  Canvas.BeginScene;
  Canvas.DrawBitmap(FBitmap,
                    RectF(0, 0, FBitmap.Width, FBitmap.Height),
                    ClipRect,
                    1,
                    False);
  Canvas.EndScene;
end;

procedure TksProgressIndicator.Redraw;
var
  ARect: TRectF;
  AXPos: single;
  ASize: single;
  ICount: integer;
  AColor: TAlphaColor;
begin
  if FBitmap = nil then
    Exit;
  FBitmap.Resize(Round(Size.Width * (GetScreenScale*2)), Round(Size.Height * (GetScreenScale*2)));
  FBitmap.Clear(claNull);

  ASize :=  FSteps.Size * (GetScreenScale*2);

  if FSteps.Visible = False then
    Exit;
  ARect := RectF(0, 0, (FSteps.MaxSteps + (FSteps.MaxSteps-1)) * ASize, ASize);

  OffsetRect(ARect, ((Width * (GetScreenScale*2)) - ARect.Width) / 2, ((Height * (GetScreenScale*2)) - ARect.Height) / 2);
  AXpos := ARect.Left;

  FBitmap.Canvas.BeginScene;
  try
    for ICount := 0 to FSteps.MaxSteps-1 do
    begin
      AColor := FInactiveColor;
      if ((ICount+1) = FSteps.CurrentStep) then
        AColor := FActiveColor;
      if ((ICount+1) < FSteps.CurrentStep) and (FKeepHighlighted) then
        AColor := FActiveColor;

      FBitmap.Canvas.Stroke.Color := FOutlineColor;
      FBitmap.Canvas.Stroke.Thickness := GetScreenScale * 2;
      FBitmap.Canvas.Stroke.Kind := TBrushKind.Solid;
      FBitmap.Canvas.Fill.Color := AColor;
      FBitmap.Canvas.Fill.Kind := TBrushKind.Solid;


      FBitmap.Canvas.FillEllipse(RectF(AXPos, ARect.Top, AXpos+ASize, ARect.Top+ASize), 1);
      FBitmap.Canvas.DrawEllipse(RectF(AXPos, ARect.Top, AXpos+ASize, ARect.Top+ASize), 1);
      AXPos := AXPos + (ASize*2);
    end;
  finally
    FBitmap.Canvas.EndScene;
  end;
end;

procedure TksProgressIndicator.Resize;
begin
  inherited;
  Changed;
end;

procedure TksProgressIndicator.SetActiveColor(const Value: TAlphaColor);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    Repaint;
  end;
end;

procedure TksProgressIndicator.SetInActiveColor(const Value: TAlphaColor);
begin
  if FInActiveColor <> Value then
  begin
    FInActiveColor := Value;
    Redraw;
  end;
end;

procedure TksProgressIndicator.SetKeepHighlighted(const Value: Boolean);
begin
  FKeepHighlighted := Value;
  Redraw;
end;

procedure TksProgressIndicator.SetOutlineColor(const Value: TAlphaColor);
begin
  FOutlineColor := Value;
  Redraw;
end;

initialization

  Classes.RegisterClass(TksProgressIndicator);

end.
