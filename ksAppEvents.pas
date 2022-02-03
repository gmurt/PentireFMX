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

unit ksAppEvents;

interface

uses FMX.Objects, FMX.Platform;


type
  IksEventForm = interface
    ['{2A2C3294-795B-4CC3-85FE-99F9A29DEAA9}']
    procedure DoAppEvent(AAppEvent: TApplicationEvent);
  end;



implementation

uses
  System.SysUtils, System.Classes, FMX.Forms, fmx.dialogservice;

type
  TksAppEvents = class
  private
  public
    constructor Create; virtual;
    function DoAppEvent(AAppEvent: TApplicationEvent; AContext: TObject) : Boolean;
  end;

var
  AAppEvents: TksAppEvents;

{ TksAppEvents }

function TksAppEvents.DoAppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
var
  ICount: integer;
  AIntf: IksEventForm;
begin
  if aappevent <> TApplicationEvent.BecameActive then exit;

  for ICount := 0 to Screen.FormCount-1 do
  begin
    if Supports(Screen.Forms[ICount], IksEventForm, AIntf) then
    begin
      tthread.Synchronize(nil, procedure begin
        AIntf.DoAppEvent(AAppEvent);
    end
    );
    end;
  end;
  Result := True;
end;

constructor TksAppEvents.Create;
var
  AppEventSvc: IFMXApplicationEventService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventSvc)) then
    AppEventSvc.SetApplicationEventHandler(DoAppEvent);
end;

initialization

  AAppEvents := TksAppEvents.Create;

finalization

  AAppEvents.Free;

end.

