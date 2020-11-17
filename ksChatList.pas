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

unit ksChatList;

interface

uses Classes, FMX.Controls, FMX.Objects, FMX.StdCtrls, FMX.Types, System.UITypes,
  ksInputList, FMX.Edit;

type
  TksChatList = class;

  TksChatInsertPosition = (cipTop, cipBottom);


  TksChatToolbar = class(TToolBar)
  private
    FChatList: TksChatList;
    FEdit: TEdit;
    FButton: TButton;
    procedure DoClickSend(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TksChatList = class(TControl)
  private
    FToolbar: TksChatToolbar;
    FInputList: TksInputList;
    FSenderName: string;
    FInsertPosition: TksChatInsertPosition;
    FTimeFormat: string;
    FDateFormat: string;
    procedure SetSenderName(const Value: string);
    procedure SetInsertPosition(const Value: TksChatInsertPosition);
    procedure SetDateFormat(const Value: string);
    procedure SetTimeFormat(const Value: string);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure AddDateSeperator(ADate: TDateTime);
    function AddChatItem(AID, ASender, AText: string; ADateTime: TDateTime; AAlign: TTextAlign; AColor, ATextColor: TAlphaColor; const AUse24HourTime: Boolean = True): TksInputListChatItem;
  published
    property Position;
    property Width;
    property Height;
    property SenderName: string read FSenderName write SetSenderName;
    property InsertPosition: TksChatInsertPosition read FInsertPosition write SetInsertPosition default cipBottom;
    property TimeFormat: string read FTimeFormat write SetTimeFormat;
    property DateFormat: string read FDateFormat write SetDateFormat;
  end;

  procedure Register;

implementation

uses System.UIConsts, Sysutils;

procedure Register;
begin
	RegisterComponents('Pentire FMX', [TksChatList]);
end;

{ TksChatList }

function TksChatList.AddChatItem(AID, ASender, AText: string; ADateTime: TDateTime; AAlign: TTextAlign; AColor, ATextColor: TAlphaColor;
  const AUse24HourTime: Boolean): TksInputListChatItem;
begin
  Result := FInputList.Items.AddChatItem(AID, ASender, AText, ADateTime, AAlign, AColor, ATextColor);
end;

procedure TksChatList.AddDateSeperator(ADate: TDateTime);
begin
  FInputList.Items.AddSeperator(FormatDateTime(FDateFormat, ADate))
end;

procedure TksChatList.BeginUpdate;
begin
  inherited;
  FInputList.BeginUpdate;
end;

constructor TksChatList.Create(AOwner: TComponent);
begin
  inherited;
  FInputList := TksInputList.Create(Self);
  FToolbar := TksChatToolbar.Create(Self);

  Height := 400;
  Width := 300;

  FInputList.Stored := False;
  FToolbar.Stored := False;

  FInputList.BackgroundColor := claWhite;
  FInputList.ShowDividers := False;

  FToolbar.Align := TAlignLayout.Bottom;
  FInputList.Align := TAlignLayout.Client;

  AddObject(FToolbar);
  AddObject(FInputList);
end;

destructor TksChatList.Destroy;
begin
  FInputList.DisposeOf;
  FToolbar.DisposeOf;
  inherited;
end;

procedure TksChatList.EndUpdate;
begin
  FInputList.EndUpdate;
  inherited;
end;

procedure TksChatList.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
end;

procedure TksChatList.SetInsertPosition(const Value: TksChatInsertPosition);
begin
  FInsertPosition := Value;
end;

procedure TksChatList.SetSenderName(const Value: string);
begin
  FSenderName := Value;
end;

procedure TksChatList.SetTimeFormat(const Value: string);
begin
  FTimeFormat := Value;
end;

{ TksChatToolbar }

constructor TksChatToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FChatList := (AOwner as TksChatList);
  FEdit := TEdit.Create(Self);
  FButton := TButton.Create(Self);

  FEdit.Stored := False;
  FEdit.Align := TAlignLayout.Client;

  FButton.Stored := False;
  FButton.Align := TAlignLayout.Right;
  FButton.Text := 'SEND';
  FButton.OnClick := DoClickSend;

  AddObject(FEdit);
  AddObject(FButton);
end;

destructor TksChatToolbar.Destroy;
begin
  FEdit.DisposeOf;
  inherited;
end;

procedure TksChatToolbar.DoClickSend(Sender: TObject);
begin

  FChatList.AddChatItem('', FChatList.SenderName, FEdit.Text, Now, TTextAlign.Trailing, claDodgerblue, claWhite, True);
end;

end.
