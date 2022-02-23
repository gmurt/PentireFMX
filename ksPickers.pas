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

unit ksPickers;

interface

{$IFDEF IOS}
{$DEFINE DPF}
{$ENDIF}

uses FMX.Pickers, Classes, FMX.Controls, System.Types, SysUtils
  {$IFDEF DPF}
  ,DPF.iOS.UIActIonSheet
  {$ENDIF}
  ;

type
  TksSelectPickerItemEvent = reference to procedure(Sender: TObject; AItem: string; AIndex: integer);
  TksSelectPickerDateTimeEvent = reference to procedure(Sender: TObject; ADate: TDateTime);
  //TksSelectPickerTimeEvent = reference to procedure(Sender: TObject; ATime: TDateTime);


  TksPickerService = class
  private
    [weak]FPicker: TCustomListPicker;
    [weak]FPrevPicker: TCustomListPicker;
    [weak]FDatePicker: TCustomDateTimePicker;
    [weak]FTimePicker: TCustomDateTimePicker;
    {$IFDEF IOS}
    [weak]FDateTimePicker: TCustomDateTimePicker;
    {$ENDIF}
    FPickerItems: TStrings;
    {$IFDEF DPF}
    FActionSheet: TDPFUIActionSheet;
    {$ENDIF}
    FPickerService: IFMXPickerService;

    FOnHide: TProc;


    FOnItemSelected: TksSelectPickerItemEvent;
    FOnDateSelected: TksSelectPickerDateTimeEvent;
    FOnTimeSelected: TksSelectPickerDateTimeEvent;
    {$IFDEF IOS}
    FOnDateTimeSelected: TksSelectPickerDateTimeEvent;
    {$ENDIF}
    //FOnHide: TNotifyEvent;
    {$IFDEF DPF}
    procedure DoActionSheetButtonClick(Sender: TObject; ButtonIndex: Integer);
    procedure DoSelectItem(Sender: TObject; const AItemIndex: integer);
    {$ENDIF}
    procedure DoItemSelected(Sender: TObject; const AValueIndex: Integer);
    procedure DoDateSelected(Sender: TObject; const ADate: TDateTime);
    function CreateListPicker: TCustomListPicker;
    procedure DoTimeSelected(Sender: TObject; const ATime: TDateTime);
    {$IFDEF IOS}
    procedure DoDateTimeSelected(Sender: TObject; const ADateTime: TDateTime);
    procedure DoActionSheetDismiss( Sender: TObject; ButtonIndex: Integer );
    {$ENDIF}
    procedure DoHide(Sender: TObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ShowActionSheet(AItems: array of string; ATitle: string; AOnSelect: TksSelectPickerItemEvent; const AOnHide: TProc = nil); overload;
    procedure ShowActionSheet(AItems: TStrings; ATitle: string; AOnSelect: TksSelectPickerItemEvent; const AOnHide: TProc = nil); overload;
    procedure ShowItemPicker(AParent: TControl; AItems: array of string; ATitle: string; AIndex: integer; AOnSelect: TksSelectPickerItemEvent); overload;
    procedure ShowItemPicker(AParent: TControl; AItems: TStrings; ATitle: string; AIndex: integer; AOnSelect: TksSelectPickerItemEvent; AOnHide: TNotifyEvent); overload;
    procedure ShowItemPicker(APoint: TPointF; AItems: TStrings; ATitle: string; AIndex: integer; AOnSelect: TksSelectPickerItemEvent); overload;
    procedure ShowDatePicker(ATitle: string; ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent; AOnHide: TProc); overload;
    procedure ShowDatePicker(AParent: TControl; ATitle: string; ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent; AOnHide: TProc); overload;
    procedure ShowDatePicker(APoint: TPointF; ATitle: string; ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent; AOnHide: TProc); overload;
    procedure ShowTimePicker(ATitle: string; ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent);

    procedure ShowDateTimePicker(ATitle: string; ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent);

    function CreateDatePicker: TCustomDateTimePicker;
    procedure HidePickers;//nst AForce: Boolean = False);
  end;

var
  PickerService: TksPickerService;

implementation

uses FMX.Platform, FMX.Forms,
  FMX.Types, System.UIConsts;

{ TksPickerService }

constructor TksPickerService.Create;
begin
  FPickerITems := TStringList.Create;
  {$IFDEF DPF}
  FActionSheet := TDPFUIActionSheet.Create(nil);
  {$ENDIF}
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    FPicker := CreateListPicker;
    FDatePicker := CreateDatePicker;
    FTimePicker := CreateDatePicker;
    FTimePicker.ShowMode := TDatePickerShowMode.Time;

    {$IFDEF IOS}
    FDateTimePicker := CreateDatePicker;
    FDateTimePicker.ShowMode := TDatePickerShowMode.DateTime;
    {$ENDIF}

  end;
end;

function TksPickerService.CreateDatePicker: TCustomDateTimePicker;
begin
  Result := FPickerService.CreateDateTimePicker;
end;

function TksPickerService.CreateListPicker: TCustomListPicker;
begin
  Result := FPickerService.CreateListPicker;
  Result.Values.Clear;
end;

destructor TksPickerService.Destroy;
begin
  FPicker.DisposeOf;
  FDatePicker.DisposeOf;
  FTimePicker.DisposeOf;
  {$IFDEF IOS}
  FDateTimePicker.DisposeOf;
  {$ENDIF}
  FreeAndNil(FPickerITems);
  {$IFDEF DPF}
  FActionSheet.DisposeOf;
  {$ENDIF}
  inherited;
end;

{$IFDEF DPF}
procedure TksPickerService.DoActionSheetButtonClick(Sender: TObject;
  ButtonIndex: Integer);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if ButtonIndex < FPickerITems.Count then
            DoSelectItem(Sender, ButtonIndex);

        end
      );
    end
  ).Start;
end;

procedure TksPickerService.DoActionSheetDismiss(Sender: TObject;
  ButtonIndex: Integer);
begin
  DoHide(Sender);
end;

{$ENDIF}


procedure TksPickerService.DoDateSelected(Sender: TObject;
  const ADate: TDateTime);
begin
  if Assigned(FOnDateSelected) then
    FOnDateSelected(Self, ADate);
end;

{$IFDEF IOS}
procedure TksPickerService.DoDateTimeSelected(Sender: TObject;
  const ADateTime: TDateTime);
begin
  if Assigned(FOnDateTimeSelected) then
    FOnDateTimeSelected(Self, ADateTime);
end;
{$ENDIF}


procedure TksPickerService.DoHide(Sender: TObject);
begin
  if Assigned(FOnHide) then
    FOnHide;
end;

procedure TksPickerService.DoTimeSelected(Sender: TObject;
  const ATime: TDateTime);
begin
  if Assigned(FOnTimeSelected) then
    FOnTimeSelected(Self, ATime);
end;

procedure TksPickerService.DoItemSelected(Sender: TObject;
  const AValueIndex: Integer);
begin
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Sender,
                    FPickerITems[AValueIndex],
                    AValueIndex);
end;


{$IFDEF DPF}
procedure TksPickerService.DoSelectItem(Sender: TObject;
  const AItemIndex: integer);
begin
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Sender, FPickerItems[AItemIndex], AItemIndex);
end;
{$ENDIF}

procedure TksPickerService.HidePickers;
begin
  try
    FPickerService.CloseAllPickers;
  except
    //
  end;
end;

procedure TksPickerService.ShowActionSheet(AItems: array of string;
  ATitle: string; AOnSelect: TksSelectPickerItemEvent; const AOnHide: TProc = nil);
var
  ICount: integer;
  AStrings: TStrings;
begin
  HidePickers;
  AStrings := TStringList.Create;
  try
    for ICount := Low(AItems) to High(AItems) do
    begin
      AStrings.Add(AItems[ICount]);
    end;
    ShowActionSheet(AStrings, ATitle, AOnSelect, AOnHide);
  finally
    FreeAndNil(AStrings);
  end;
end;

procedure TksPickerService.ShowActionSheet(AItems: TStrings; ATitle: string;
  AOnSelect: TksSelectPickerItemEvent; const AOnHide: TProc = nil);
  {$IFDEF DPF}
var
  ICount: integer;
  ABtn: TDPFActionSheetButtonItem;
  {$ENDIF}
begin
  {$IFDEF ANDROID}
  ShowItemPicker(nil, AItems, ATitle, -1, AOnSelect, nil);
  Exit;
  {$ENDIF}

  {$IFDEF DPF}
  FOnItemSelected := AOnSelect;

  FActionSheet.Title := UpperCase(ATitle);
  FActionSheet.Buttons.Clear;
  FPickerITems.Assign(AItems);

  FOnHide := AOnHide;
  FPicker.OnHide := DoHide;

  for ICount := 0 to AItems.Count-1 do
  begin
    ABtn := FActionSheet.Buttons.Add;
    ABtn.Title := {UpperCase}(AItems[ICount]);
    if Pos('[RED]', ABtn.Title) = 1 then
    begin
      ABtn.Title := StringReplace(ABtn.Title, '[RED]', '', []);
      ABtn.ButtonKind := TDPFActionSheetButtonKind.bkDestructive;
    end;
  end;
  with FActionSheet.Buttons.Add do
  begin
    ButtonKind := TDPFActionSheetButtonKind.bkCancel;
    Title := 'Cancel';
  end;

  FActionSheet.OnDismiss := DoActionSheetDismiss;
  FActionSheet.ShowMessage;
  FActionSheet.OnClick := DoActionSheetButtonClick;
  {$ELSE}
  ShowItemPicker(nil, AItems, '', -1, AOnSelect, DoHide);
  {$ENDIF}

end;

procedure TksPickerService.ShowDatePicker(AParent: TControl; ATitle: string;
  ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent; AOnHide: TProc);
begin
  HidePickers;
  FOnDateSelected := AOnSelect;

  FOnHide := AOnHide;

  FDatePicker := PickerService.CreateDatePicker;

  FDatePicker.OnDateChanged := DoDateSelected;
  if ASelected = 0 then
    ASelected := Date;
  FDatePicker.Date := ASelected;
  FDatePicker.Parent := AParent;
  FDatePicker.OnHide := DoHide;

  if AParent = nil then
  begin
    FDatePicker.AbsoluteTargetRect := RectF((Screen.ActiveForm.Width/2)-25,
                                            (Screen.ActiveForm.Height/2)-25,
                                            (Screen.ActiveForm.Width/2)+25,
                                            (Screen.ActiveForm.Height/2));
    FDatePicker.PreferedDisplayIndex := 1;
  end;

  FDatePicker.Show;
end;

procedure TksPickerService.ShowDatePicker(APoint: TPointF; ATitle: string; ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent; AOnHide: TProc);
begin
  HidePickers;
  FOnDateSelected := AOnSelect;
  FOnHide := AOnHide;

  FDatePicker := PickerService.CreateDatePicker;
  FDatePicker.OnDateChanged := DoDateSelected;
  if ASelected = 0 then
    ASelected := Date;
  FDatePicker.Date := ASelected;
  FDatePicker.OnHide := DoHide;

  FDatePicker.AbsoluteTargetRect := RectF(APoint.X, APoint.Y, APoint.X, APoint.Y);
  FDatePicker.Show;
end;

procedure TksPickerService.ShowDatePicker(ATitle: string; ASelected: TDateTime;
  AOnSelect: TksSelectPickerDateTimeEvent; AOnHide: TProc);
begin
  ShowDatePicker(nil, ATitle, ASelected, AOnSelect, AOnHide);
end;

procedure TksPickerService.ShowTimePicker(ATitle: string;
  ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent);
begin
  HidePickers;
  FOnTimeSelected := AOnSelect;

  FTimePicker := CreateDatePicker;
  FTimePicker.ShowMode := TDatePickerShowMode.Time;

  FTimePicker.OnDateChanged := DoTimeSelected;
  if ASelected = 0 then
    ASelected := EncodeTime(9,0,0,0);
  FTimePicker.Date := ASelected;
  {$IFDEF MSWINDOWS}
  FTimePicker.AbsoluteTargetRect := RectF(10,10,100,100);
  {$ENDIF}

  FTimePicker.Show;
end;


procedure TksPickerService.ShowDateTimePicker(ATitle: string;
  ASelected: TDateTime; AOnSelect: TksSelectPickerDateTimeEvent);
begin
  HidePickers;

  {$IFDEF IOS}
  FOnDateTimeSelected := AOnSelect;

  FDateTimePicker := CreateDatePicker;
  FDateTimePicker.ShowMode := TDatePickerShowMode.DateTime;
  FDateTimePicker.OnDateChanged := DoDateTimeSelected;
  if ASelected = 0 then
    ASelected := Trunc(Date)+EncodeTime(9,0,0,0);
  FDateTimePicker.Date := ASelected;
  FDateTimePicker.Show;
  {$ENDIF}

  {$IFDEF ANDROID}
  FOnDateSelected := AOnSelect;
  FDatePicker := CreateDatePicker;
  FDatePicker.OnDateChanged := DoDateSelected;
  if ASelected = 0 then
    ASelected := Trunc(Date)+EncodeTime(9,0,0,0);
  FDatePicker.Date := ASelected;
  FDatePicker.Show;

  //ShowDatePicker(ATitle, ASelected, nil)
  {$ENDIF}
end;


procedure TksPickerService.ShowItemPicker(AParent: TControl; AItems: TStrings; ATitle: string;
  AIndex: integer; AOnSelect: TksSelectPickerItemEvent; AOnHide: TNotifyEvent);
begin
  HidePickers;

  FPickerITems.Assign(AItems);

  FPicker := CreateListPicker;
  FPicker.OnHide := AOnHide;
  FPicker.Values.Assign(AItems);
  FPicker.ItemIndex := AIndex;
  FPicker.Parent := AParent;

  FOnItemSelected := AOnSelect;
  FPicker.OnValueChanged := DoItemSelected;
  {$IFDEF MSWINDOWS}
  FPicker.AbsoluteTargetRect := RectF(10,10,100,100);
  {$ENDIF}
  FPicker.Show;


  FPrevPicker := FPicker;
end;

procedure TksPickerService.ShowItemPicker(AParent: TControl; AItems: array of string;
  ATitle: string; AIndex: integer; AOnSelect: TksSelectPickerItemEvent);
var
  ICount: integer;
  AStrings: TStrings;
begin

  AStrings := TStringList.Create;
  try
    for Icount := Low(AItems) to High(AItems) do
    begin
      AStrings.Add(AItems[ICount]);
      ShowItemPicker(AParent, AStrings, ATitle, AIndex, AOnSelect, nil);
    end;
  finally
    FreeAndNil(AStrings);
  end;
end;

procedure TksPickerService.ShowItemPicker(APoint: TPointF; AItems: TStrings;
  ATitle: string; AIndex: integer; AOnSelect: TksSelectPickerItemEvent);
begin
  HidePickers;

  FPickerITems.Assign(AItems);

  {$IFDEF MSWINDOWS}
  if FPicker <> nil then
  begin
    FPicker.Free;
  end;
  {$ENDIF}

  FPicker := PickerService.CreateListPicker;

  FPicker.Values.Assign(AItems);
  FPicker.ItemIndex := AIndex;
  FPicker.AbsoluteTargetRect := RectF(APoint.X-10, APoint.Y-10, APoint.X+10, APoint.Y+10);

  FOnItemSelected := AOnSelect;

  FPicker.OnValueChanged := DoItemSelected;
  FPicker.Show;

  FPrevPicker := FPicker;
end;

initialization

  PickerService := TksPickerService.Create;

finalization

  FreeAndNil(PickerService);

end.
