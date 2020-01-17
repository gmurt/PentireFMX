{*******************************************************************************
*                                                                              *
*  TksFormStack                                                                *
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

unit ksFormStack;

interface

uses System.Generics.Collections, FMX.Graphics, FMX.Objects, System.Classes, FMX.Forms;

type
  TksFormStackTransitionType = (ttNone, ttSlideInFromRight, ttSlideUpFromBottom);

  ITransitionForm = interface
    ['{771D1CDA-322B-486D-8AB6-96714A7AB41B}']
    procedure AfterTransitionIntoView(AIsPush: Boolean; const AParams: TStrings = nil);
    procedure BeforeTransitionIntoView(AIsPush: Boolean; const AParams: TStrings = nil);

  end;

  TksFormStackTransition = class
  private
    FType: TksFormStackTransitionType;
    FForm: TCustomForm;
  end;

  TksFormStack = class
  private
    FUpdating: Boolean;
    FStack: TList<TksFormStackTransition>;
    procedure HideKeyboard;
    procedure AnimateForms(AFrom, ATo: TCustomForm; AReverse: Boolean; const ASlideUp: Boolean = False);
    function GetDepth: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Push(AForm: TCustomForm; const
                   ADirection: TksFormStackTransitionType = ttSlideInFromRight;
                   const AParams: TStrings = nil);
    procedure Pop;
    procedure Clear(ARootForm: TCommonCustomForm);
    property Depth: integer read GetDepth;
  end;

var
  GlobalFormStack: TksFormStack;

implementation

uses System.UIConsts, FMX.Ani, FMX.VirtualKeyboard, FMX.Platform, FMX.Types,
  SysUtils;

const
  C_SLIDE_SPEED = 0.2;

var
  Bmp1, Bmp2: TBitmap;


{ TksFormStack }

procedure TksFormStack.AnimateForms(AFrom, ATo: TCustomForm; AReverse: Boolean; const ASlideUp: Boolean = False);
var
  AImg1, AImg2: TImage;
begin
  AImg1 := TImage.Create(nil);
  AImg2 := TImage.Create(nil);
  try
    AFrom.Focused := nil;
    ATo.SetBounds(AFrom.Bounds);

    Bmp1.SetSize(AFrom.Width*2, AFrom.Height*2);
    Bmp2.SetSize(AFrom.Width*2, AFrom.Height*2);
    AFrom.PaintTo(Bmp1.Canvas);
    ATo.PaintTo(Bmp2.Canvas);
    AImg1.Bitmap.Assign(Bmp1);
    AImg2.Bitmap.Assign(Bmp2);

    if not AReverse then
    begin
      AImg1.SetBounds(0, 0, AFrom.Width, AFrom.Height);
      case ASlideUp of
        True: AImg2.SetBounds(0, AFrom.Height, AFrom.Width, AFrom.Height);
        False: AImg2.SetBounds(AFrom.Width, 0, AFrom.Width, AFrom.Height);
      end;

      ATo.AddObject(AImg1);
      ATo.AddObject(AImg2);
      ATo.Show;

      case ASlideUp of
        False: TAnimator.AnimateFloat(AImg1, 'Position.X', 0-(AImg1.Width/2), C_SLIDE_SPEED);
      end;

      case ASlideUp of
        True: TAnimator.AnimateFloatWait(AImg2, 'Position.Y', 0, C_SLIDE_SPEED);
        False: TAnimator.AnimateFloatWait(AImg2, 'Position.X', 0, C_SLIDE_SPEED);
      end;


      AFrom.Hide;
      ATo.RemoveObject(AImg1);

      ATo.RemoveObject(AImg2);
    end
    else
    begin
      AImg1.SetBounds(0, 0, ATo.Width, ATo.Height);
      case ASlideUp of
        False: AImg2.SetBounds(0-AFrom.Width/2, 0, AFrom.Width, AFrom.Height);
      end;


      ATo.AddObject(AImg2);
      ATo.AddObject(AImg1);

      ATo.Show;
      Application.ProcessMessages;

      case ASlideUp of
        //True: TAnimator.AnimateFloat(AImg2, 'Position.Y', 0, 1*0.3);
        False: TAnimator.AnimateFloat(AImg2, 'Position.X', 0, C_SLIDE_SPEED);

      end;

      if ASlideUp then
      begin
        ATo.RemoveObject(AImg2);
        Application.ProcessMessages;
      end;

      case ASlideUp of
        True: TAnimator.AnimateFloatWait(AImg1, 'Position.Y', AImg1.Height, C_SLIDE_SPEED);
        False: TAnimator.AnimateFloatWait(AImg1, 'Position.X', AImg1.Width, C_SLIDE_SPEED);
      end;


      AFrom.Hide;
      ATo.RemoveObject(AImg1);
      ATo.RemoveObject(AImg2);
    end;
  finally
    AImg1.Free;
    AImg2.Free;
  end;
end;

procedure TksFormStack.Clear(ARootForm: TCommonCustomForm);
var
  ARoot: TksFormStackTransition;
begin
  FStack.Clear;
  if ARootForm <> nil then
  begin
    ARoot := TksFormStackTransition.Create;
    ARoot.FForm := TCustomForm(ARootForm);
    FStack.Add(ARoot);
  end;
end;

constructor TksFormStack.Create;
begin
  inherited;
  FStack := TList<TksFormStackTransition>.Create;
  FUpdating := False;
end;

destructor TksFormStack.Destroy;
begin
  FStack.Free;
  inherited;
end;

function TksFormStack.GetDepth: integer;
begin
  Result := FStack.Count;
end;

procedure TksFormStack.Pop;
var
  AInf: ITransitionForm;
  AItem: TksFormStackTransition;
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    if FStack.Count > 0 then
    begin
      if FStack.Last.FType = ttNone then
      begin
        FStack[FStack.Count-2].FForm.Show;
        FStack.Last.FForm.Hide;
      end
      else
        AnimateForms(FStack.Last.FForm,
                     FStack[FStack.Count-2].FForm,
                     True,
                     FStack.Last.FType = ttSlideUpFromBottom);


      AItem := FStack.Last;
      FStack.Remove(FStack.Last);
      if Supports(FStack.Last.FForm, ITransitionForm, AInf) then
        AInf.AfterTransitionIntoView(False);
      AItem.Free;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TksFormStack.Push(AForm: TCustomForm;
                            const ADirection: TksFormStackTransitionType = ttSlideInFromRight;
                            const AParams: TStrings = nil);
var
  ATran: TksFormStackTransition;
  AInf: ITransitionForm;
  ALast: TCustomForm;
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    AForm.Focused := nil;

    if FStack.Count > 0 then
    begin
      AForm.Width := FStack.Last.FForm.Width;
      AForm.Height := FStack.Last.FForm.Height;
    end;

    AForm.Show;
    AForm.Hide;

    HideKeyboard;


    ALast := nil;
    if FStack.Count > 0 then
      ALast := FStack.Last.FForm;

    ATran := TksFormStackTransition.Create;
    ATran.FType := ADirection;
    ATran.FForm := AForm;
    FStack.Add(ATran);

    if Supports(AForm, ITransitionForm, AInf) then
      AInf.BeforeTransitionIntoView(True, AParams);


    if FStack.Count > 0 then
    begin
      if ALast = nil then
        AForm.Show
      else
      begin
        {$IFDEF ANDROID}
        AForm.Show;
        {$ELSE}
        if ADirection = ttNone then
          AForm.Show
        else
          AnimateForms(ALast, AForm, False, ADirection = ttSlideUpFromBottom);
        {$ENDIF}
      end;
    end
    else
      AForm.Show;


    if Supports(AForm, ITransitionForm, AInf) then
      AInf.AfterTransitionIntoView(True, AParams);

  finally
    FUpdating := False;
  end;
end;

procedure TksFormStack.HideKeyboard;
var
  KeyboardService: IFMXVirtualKeyboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(KeyboardService)) then
    KeyboardService.HideVirtualKeyboard;
end;

initialization

  GlobalFormStack := TksFormStack.Create;
  Bmp1 := TBitmap.Create;
  Bmp2 := TBitmap.Create;
  Bmp1.BitmapScale := 2;
  Bmp2.BitmapScale := 2;

finalization

  GlobalFormStack.Free;
  Bmp1.Free;
  Bmp2.Free;

end.

