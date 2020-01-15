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

unit ksSideMenu;

interface

uses Classes, FMX.Objects, ksInputList, FMX.Forms, FMX.Graphics, System.Generics.Collections;

type
  TksSideMenu = class;

  TksSelectSideMenuItemEvent = procedure(Sender: TObject; AMenuItemID: string) of object;
  TksShowFormEvent = procedure(Sender: TObject; AForm: TCommonCustomForm) of object;

  TksSideMenuItem = class
  private
    FID: string;
    FImage: TBitmap;
    FText: string;
    FMenu: TksSideMenu;
    procedure Changed;
    procedure SetID(const Value: string);
    procedure SetImage(const Value: TBitmap);
    procedure SetText(const Value: string);
  public
    constructor Create(AMenu: TksSideMenu);
    destructor Destroy; override;
    property ID: string read FID write SetID;
    property Image: TBitmap read FImage write SetImage;
    property Text: string read FText write SetText;
  end;

  TksSideMenuItemList = class(TObjectList<TksSideMenuItem>)
  private
    FMenu: TksSideMenu;
  public
    constructor Create(AMenu: TksSideMenu);
    procedure Add(AID, AText: string; AImg: TBitmap); overload;
  end;

  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidiOS or pfidAndroid or pidWinNX32 or pidWinIoT32)]
  TksSideMenu = class(TComponent)
  private
    FCachedForm: TBitmap;
    FItems: TksSideMenuItemList;
    FImage: TImage;
    FMenu: TksInputList;
    FCallingForm: TCommonCustomForm;
    FOnSelectItem: TksSelectSideMenuItemEvent;
    FBeforeShowForm: TksShowFormEvent;
    FAfterShowForm: TksShowFormEvent;
    procedure UpdateMenu;
    procedure OffsetScreenClick(Sender: TObject);
    procedure DoItemClick(Sender: TObject; AItem: TksBaseInputListItem; AID: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenMenu(ACallingForm: TCommonCustomForm);
    procedure CloseMenu(ATargetForm: TCommonCustomForm);
    property Items: TksSideMenuItemList read FItems;
  published
    property AfterShowForm: TksShowFormEvent read FAfterShowForm write FAfterShowForm;
    property BeforeShowForm: TksShowFormEvent read FBeforeShowForm write FBeforeShowForm;
    property OnSelectItem: TksSelectSideMenuItemEvent read FOnSelectItem write FOnSelectItem;
  end;

  procedure Register;


implementation



uses SysUtils, FMX.Platform, System.UIConsts, FMX.Types, FMX.Ani, FMX.Utils, System.UITypes, ksFormStack;

const
  C_MENU_SLIDE_SPEED  = 0.15;
  C_MENU_WIDTH        = 250;

var
  AScreenScale: single;

procedure Register;
begin
	RegisterComponents('Pentire FMX', [TksSideMenu]);
end;


function GetScreenScale: single;
var
  Service: IFMXScreenService;
begin
  if AScreenScale > 0 then
  begin
    Result := AScreenScale;
  end
  else
  begin
    Service := IFMXScreenService(TPlatformServices.Current.GetPlatformService(IFMXScreenService));
    Result := Service.GetScreenScale;
    AScreenScale := Result;
  end;
end;

procedure GenerateFormBitmap(AForm: TCommonCustomForm; ABmp: TBitmap);
var
  AScale: single;
begin
  TThread.Synchronize (TThread.CurrentThread,
      procedure ()
      begin
        AScale := GetScreenScale;
        if ABmp.IsEmpty then
        begin
          ABmp.BitmapScale := AScale;
          ABmp.Width := Round(AForm.ClientWidth * AScale);
          ABmp.Height := Round(AForm.ClientHeight * AScale);
        end;
        ABmp.Clear(claWhite);
        ABmp.Canvas.BeginScene;
        TForm(AForm).PaintTo(ABmp.Canvas);
        ABmp.Canvas.EndScene;
      end);
end;

procedure ReplaceOpaqueColor(ABmp: TBitmap; Color : TAlphaColor);
var
  //x,y: Integer;
  AMap: TBitmapData;
  PixelColor: TAlphaColor;
  PixelWhiteColor: TAlphaColor;
  C: PAlphaColorRec;
begin
  TThread.Synchronize(nil,procedure
                    var
                    x,y: Integer;
                    begin
                      if (Assigned(ABmp)) then
  begin
    if ABmp.Map(TMapAccess.ReadWrite, AMap) then
    try
      AlphaColorToPixel(Color   , @PixelColor, AMap.PixelFormat);
      AlphaColorToPixel(claWhite, @PixelWhiteColor, AMap.PixelFormat);
      for y := 0 to ABmp.Height - 1 do
      begin
        for x := 0 to ABmp.Width - 1 do
        begin
          C := @PAlphaColorArray(AMap.Data)[y * (AMap.Pitch div 4) + x];
          if (C^.Color<>claWhite) and (C^.A>0) then
            C^.Color := PremultiplyAlpha(MakeColor(PixelColor, C^.A / $FF));
        end;
      end;
    finally
      ABmp.Unmap(AMap);
    end;
  end;
  end);
end;

{ TksSideMenu }

procedure TksSideMenu.CloseMenu(ATargetForm: TCommonCustomForm);
begin
  if ATargetForm = nil then
    ATargetForm := FCallingForm;

  if ATargetForm <> FCallingForm then
  begin
    ATargetForm.SetBounds(FCallingForm.Bounds);

    if Assigned(FBeforeShowForm) then
      FBeforeShowForm(Self, ATargetForm);

    GenerateFormBitmap(ATargetForm, FCachedForm);
    FImage.Bitmap := FCachedForm;
    Application.ProcessMessages;

  end;

  TAnimator.AnimateFloatWait(FImage, 'Position.X', 0, C_MENU_SLIDE_SPEED);
  ATargetForm.Show;

  if Assigned(FAfterShowForm) then
    FAfterShowForm(Self, ATargetForm);


    Application.ProcessMessages;
  begin
    FCallingForm.RemoveObject(FMenu);
    FCallingForm.RemoveObject(FImage);
  end;
end;

constructor TksSideMenu.Create(AOwner: TComponent);
begin
  inherited;
  FCachedForm := TBitmap.Create;
  FItems := TksSideMenuItemList.Create(Self);
  FMenu := TksInputList.Create(nil);
  FMenu.ItemHeight := 60;
  FMenu.OnItemClick := DoItemClick;
  FImage := TImage.Create(nil);
  FImage.OnClick := OffsetScreenClick;
  FImage.SetBounds(0,0,0,0);

  FMenu.BackgroundColor := claBlack;
end;

destructor TksSideMenu.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FCachedForm);
  FMenu.DisposeOf;
  FImage.DisposeOf;
  inherited;
end;



procedure TksSideMenu.DoItemClick(Sender: TObject; AItem: TksBaseInputListItem; AID: string);
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self, AID);
end;

procedure TksSideMenu.OffsetScreenClick(Sender: TObject);
begin
  CloseMenu(FCallingForm);
end;

procedure TksSideMenu.OpenMenu(ACallingForm: TCommonCustomForm);
begin
  FCallingForm := ACallingForm;
  GenerateFormBitmap(ACallingForm, FCachedForm);

  FImage.Visible := False;
  FMenu.Visible := False;

  if FImage.Width = 0 then
  begin
    FImage.SetBounds(0, 0, ACallingForm.Width, ACallingForm.Height);
    FImage.Bitmap := FCachedForm;
    FImage.WrapMode := TImageWrapMode.Original;
  end;

  ACallingForm.AddObject(FMenu);
  ACallingForm.AddObject(FImage);

  FMenu.SetBounds(0, 0, C_MENU_WIDTH, ACallingForm.Height);
  FMenu.Width := C_MENU_WIDTH;
  FMenu.BackgroundColor := claBlack;

  FImage.Visible := True;
  FMenu.Visible := True;
  TAnimator.AnimateFloat(FImage, 'Position.X', C_MENU_WIDTH, C_MENU_SLIDE_SPEED, TAnimationType.&In, TInterpolationType.Quadratic);

end;

procedure TksSideMenu.UpdateMenu;
var
  AItem: TksSideMenuItem;
  AInputListItem: TksBaseInputListItem;
  ABmp: TBitmap;
begin
  FMenu.ClearItems;
  for AItem in FItems do
  begin
    ABmp := TBitmap.Create;
    try
      ABmp.Assign(AItem.Image);
      ReplaceOpaqueColor(ABmp, claWhite);
      AInputListItem := FMenu.Items.AddItem(AItem.ID,
                                            ABmp,
                                            AItem.Text,
                                            atMore);
      AInputListItem.TextColor := claWhite;
      AInputListItem.BackgroundColor := claBlack;
      AInputListItem.SelectedColor := claDarkgray;
    finally
      FreeAndNil(ABmp);
    end;
  end;
end;

{ TksSideMenuItem }


procedure TksSideMenuItem.Changed;
begin
  FMenu.UpdateMenu;
end;

constructor TksSideMenuItem.Create(AMenu: TksSideMenu);
begin
  FImage := TBitmap.Create;
  FMenu := AMenu;
end;

destructor TksSideMenuItem.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TksSideMenuItem.SetID(const Value: string);
begin
  FID := Value;
  Changed;
end;

procedure TksSideMenuItem.SetImage(const Value: TBitmap);
begin
  FImage := Value;
  Changed;
end;

procedure TksSideMenuItem.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;

{ TksSideMenuItemList }

procedure TksSideMenuItemList.Add(AID, AText: string; AImg: TBitmap);
var
  AItem: TksSideMenuItem;
begin
  AItem := TksSideMenuItem.Create(FMenu);
  AItem.FID := AID;
  AItem.FText := AText;
  AItem.FImage.Assign(AImg);
  Add(AItem);
  FMenu.UpdateMenu;
end;

constructor TksSideMenuItemList.Create(AMenu: TksSideMenu);
begin
  inherited Create(True);
  FMenu := AMenu;
end;

end.
