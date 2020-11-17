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
    FStack: TObjectList<TksFormStackTransition>;
    procedure HideKeyboard;
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

uses System.UIConsts, FMX.Ani, FMX.VirtualKeyboard, FMX.Platform, FMX.Types,
  SysUtils;

const
  C_SLIDE_SPEED = 0.2;

var
  Bmp1, Bmp2: TBitmap;

{ TksFormStack }

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
  FStack := TObjectList<TksFormStackTransition>.Create;
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
    if FStack.Count > 1 then
    begin
      FStack[FStack.Count-2].FForm.Show;
      FStack.Last.FForm.Hide;

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

    if FStack.Count > 1 then
    begin
      AForm.Width := FStack.Last.FForm.Width;
      AForm.Height := FStack.Last.FForm.Height;
    end;

    HideKeyboard;

    ALast := nil;
    if FStack.Count > 1 then
      ALast := FStack.Last.FForm;

    ATran := TksFormStackTransition.Create;
    ATran.FType := ttNone;
    ATran.FForm := AForm;
    FStack.Add(ATran);

    try
      if Supports(AForm, ITransitionForm, AInf) then
        AInf.BeforeTransitionIntoView(True, AParams);
    except
      //
    end;


    if FStack.Count > 1 then
    begin
      if ALast = nil then
        AForm.Show
      else
      begin
        AForm.Show;
      end;
    end
    else
      AForm.Show;

    try
      if Supports(AForm, ITransitionForm, AInf) then
        AInf.AfterTransitionIntoView(True, AParams);
    except
      //
    end;

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
  Bmp1 := nil;
  Bmp2 := nil;

finalization

  GlobalFormStack.Free;
  Bmp1.Free;
  Bmp2.Free;

end.

