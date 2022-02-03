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

unit ksFormStack;

interface

uses System.Generics.Collections, FMX.Graphics, FMX.Objects, System.Classes, FMX.Forms;

type
  TksFormStackTransitionType = (ttNone, ttSlideInFromRight, ttSlideUpFromBottom);

  ITransitionForm = interface
    ['{F1672424-EA18-46FB-A170-269BF541142B}']
    procedure BeforeTransitionIntoView(AIsPush: Boolean; const AParams: TStrings = nil);
    procedure AfterTransitionIntoView(AIsPush: Boolean; const AParams: TStrings = nil);
  end;


  TksFormStack = class
  private
    FStack: TList<TCommonCustomForm>;
    function GetDepth: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Push(AForm: TCustomForm;
                   const AParams: TStrings = nil);
    procedure Pop;
    procedure Clear(ARootForm: TCommonCustomForm);
    property Depth: integer read GetDepth;
  end;

var
  GlobalFormStack: TksFormStack;

implementation

uses System.UIConsts, FMX.Ani, FMX.Platform, FMX.Types,
  SysUtils;

const
  C_SLIDE_SPEED = 0.2;

var
  Bmp1, Bmp2: TBitmap;

{ TksFormStack }

procedure TksFormStack.Clear(ARootForm: TCommonCustomForm);
var
  ARoot: TCommonCustomForm;
begin
  FStack.Clear;
  if ARootForm <> nil then
  begin
    //ARoot := TksFormStackTransition.Create;
    ARoot := TCustomForm(ARootForm);
    FStack.Add(ARoot);
  end;
end;

constructor TksFormStack.Create;
begin
  inherited;
  FStack := TList<TCommonCustomForm>.Create;
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
  AToShow: TCommonCustomForm;
  AToHide: TCommonCustomForm;
  //l: integer;
  AInf: ITransitionForm;
begin
  if Screen.ActiveForm <> nil then
    Screen.ActiveForm.Focused := nil;

  if FStack.Count > 1 then
  begin
   //l := 2;
    AToShow := FStack[FStack.Count-2];
    AToHide := FStack.Last;
    AToHide.Focused := nil;
    FStack.Remove(AToHide);

    if Supports(AToShow, ITransitionForm, AInf) then
      AInf.BeforeTransitionIntoView(False, nil);

    AToShow.Show;
    AToHide.hide;
    if Supports(AToShow, ITransitionForm, AInf) then
      AInf.AfterTransitionIntoView(False, nil);
  end;
end;

procedure TksFormStack.Push(AForm: TCustomForm;
                            const AParams: TStrings = nil);
var
  AInf: ITransitionForm;
  ALast: TCommonCustomForm;
  AParamStr: string;

begin
  if (FStack.Count = 0) and (Screen.ActiveForm <> nil)  then
    FStack.Add(Screen.ActiveForm);

  ALast := nil;
  if FStack.Count > 0 then
    ALast := FStack.Last;

  if ALast = AForm then
    Exit;

  AParamStr := '';
  if AParams <> nil then
    AParamStr := AParams.Text;
  if Screen.ActiveForm <> nil then
    Screen.ActiveForm.Focused := nil;
  AForm.Focused := nil;

  if FStack.Count > 0 then
  begin
    AForm.Width := FStack.Last.Width;
    AForm.Height := FStack.Last.Height;
  end;
  FStack.Add(AForm);

  if Supports(AForm, ITransitionForm, AInf) then
    AInf.BeforeTransitionIntoView(True, AParams);

  AForm.Show;
  if ALast <> nil then
    ALast.Hide;

  if Supports(AForm, ITransitionForm, AInf) then
    AInf.AfterTransitionIntoView(True, AParams);
end;

initialization

  GlobalFormStack := TksFormStack.Create;
  Bmp1 := nil;
  Bmp2 := nil;

finalization

  GlobalFormStack.Free;
  Bmp1.Free;
  Bmp2.Free;

end.


