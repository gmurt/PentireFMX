{*******************************************************************************
*                                                                              *
*  ksComponentEditors - design time component editors                          *
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

unit ksComponentEditors;

interface

uses Classes, DesignEditors, DesignIntf;

const
  C_VERB_ABOUT = 'About';
  C_ADD_TAB = '&Add Tab';
  C_NEXT_TAB = '&Next Tab';
  C_PREV_TAB = '&Previous Tab';

type


 TksBaseComponentEditor = class(TComponentEditor)
  private
    FVerbs: TStringList;
  protected
    procedure BuildVerbs(AVerbs: TStrings); virtual;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    destructor Destroy; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TksTabControlComponentEditor = class(TksBaseComponentEditor)
  private
  protected
    procedure BuildVerbs(AVerbs: TStrings); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
  end;




  procedure Register;

implementation

uses SysUtils, FMX.Types, FMX.Dialogs, ksTabControl;

procedure Register;
begin
  //RegisterComponentEditor(TksComponent, TksBaseComponentEditor);
 // RegisterComponentEditor(TksControl, TksBaseComponentEditor);

  // tab control...
  RegisterComponentEditor(TksTabControl, TksTabControlComponentEditor);
  RegisterComponentEditor(TksTabItem, TksTabControlComponentEditor);
end;

{ TksTabControlComponentEditor }

procedure TksTabControlComponentEditor.BuildVerbs(AVerbs: TStrings);
begin
  inherited;
  AVerbs.Add(C_ADD_TAB);
  AVerbs.Add('-');
  AVerbs.Add(C_PREV_TAB);
  AVerbs.Add(C_NEXT_TAB);
end;

procedure TksTabControlComponentEditor.ExecuteVerb(Index: Integer);
var
  ItemParent: TFmxObject;
  ATabControl: TksTabControl;
begin
  inherited;
  ATabControl := nil;
  if Component is TksTabControl then
    ATabControl := (Component as TksTabControl);
  if Component is TksTabItem then
    ATabControl := TksTabControl(TksTabItem(Component).Parent);

  if FVerbs[Index] = C_ADD_TAB then
  begin
    if ATabControl <> nil then
    begin
      ItemParent := TFmxObject((ATabControl as IItemsContainer).GetObject);
      Designer.CreateChild(TksTabItem, ItemParent);
    end;
  end;
  if FVerbs[Index] = C_PREV_TAB then ATabControl.PrevTab;
  if FVerbs[Index] = C_NEXT_TAB then ATabControl.NextTab;
end;

{ TksBaseComponentEditor }

procedure TksBaseComponentEditor.BuildVerbs(AVerbs: TStrings);
begin
end;

constructor TksBaseComponentEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited;
  FVerbs := TStringList.Create;
  BuildVerbs(FVerbs);
  if FVerbs.Count > 0 then
    FVerbs.Add('-');
  FVerbs.Add(C_VERB_ABOUT);
  FVerbs.Add('-');
end;

destructor TksBaseComponentEditor.Destroy;
begin
  FVerbs.Free;
  inherited;
end;

procedure TksBaseComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if FVerbs[Index] = C_VERB_ABOUT then
    ShowMessage('ksComponents - Components for FireMonkey'+#13+#13+
                'Twitter: @kscomponents'+#13+#13+
                'Copyright © 2017 Graham Murt');
end;

function TksBaseComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := FVerbs[Index];
end;

function TksBaseComponentEditor.GetVerbCount: Integer;
begin
  Result := FVerbs.Count;
end;

end.
