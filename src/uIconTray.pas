unit uIconTray;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Forms, Vcl.Graphics, Winapi.Messages,
  System.SysUtils, Winapi.ShellApi, Vcl.Menus;

const
  wm_DefaultIconMessage = WM_USER + 100;

type
  TIconTrayApplication = class;

  EIconTrayApplication = class(Exception);

  {!~ Event handler to handle mouse events from the icon tray.

  Parameters

    IconTrayApplication:     Event source.
    MessageId:               Message identifier of the mouse message }
  TTrayMouseEvent = procedure (IconTrayApplication: TIconTrayApplication;
                      const MessageId: Longint) of object;

  {!~De TIconTrayApplication class represents the current program in the icon
  tray. }
  TIconTrayApplication = class(TComponent)
  private
    FIcon:                   TIcon;
    FIconMessage:            Integer;
    FToolTip:                string;
    FTrayIcon:               TNotifyIconData;
    FHandle:                 HWnd;
    FUpdateIconDisabled:     Byte;
    FUpdateIconRequired:     Boolean;
    FOnUpdateIcon:           TNotifyEvent;
    FOnClick:                TNotifyEvent;
    FOnDblClick:             TNotifyEvent;   
    FOnMouse:                TTrayMouseEvent;
    FPopupMenu:              TPopupMenu;
    FApplicationIcon:        Boolean;
    FVisible:                Boolean;
    FOwnerForm: TCustomForm;

    function                 GetIcon: TIcon;
    procedure                SetIcon(const Value: TIcon);
    procedure                IconChanged(Sender: TObject);
    procedure                SetIconMessage(const Value: Integer);
    procedure                SetToolTip(const Value: string);
    function                 GetHandle: HWnd;
    procedure                SetApplicationIcon(const Value: Boolean);
    function                 StoreIcon: Boolean;

    procedure                HandleNeeded;
    procedure                SetVisible(const Value: Boolean);
    procedure DeleteHandle;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetOwnerForm(const Value: TCustomForm);
    function GetIsMainIcon: Boolean;
    procedure SetIsMainIcon(const Value: Boolean);
  protected
    procedure                HandleWndProc(var Msg: TMessage); virtual;

    procedure                UpdateIcon;
    procedure                DoOnUpdateIcon; virtual;
    procedure                DoOnClick; virtual;
    procedure                DoOnDblClick; virtual;
    procedure                DoOnMouse(const MessageId: Longint); virtual;
    procedure                Notification(AComponent: TComponent;
                               Operation: TOperation); override;
    procedure                Loaded; override;
  public
    constructor              Create(AOwner: TComponent); override;
    destructor               Destroy; override;
    procedure                ShowOwnerForm;
    procedure                DisableUpdateIcon;
    procedure                EnableUpdateIcon;
    procedure                Show;
    procedure                Hide;
    property                 Handle: HWnd
      read                   GetHandle;

    {!~The OwnerForm property can be used to set the owner form. If the
    component is placed on a data module, the property must be set before the
    ShowOwnerForm method can be used }
    property OwnerForm: TCustomForm read FOwnerForm write SetOwnerForm;
  published

    property                 Icon: TIcon
      read                   GetIcon
      write                  SetIcon
      stored                 StoreIcon;
    property                 IconMessage: Integer
      read                   FIconMessage
      write                  SetIconMessage
      default                wm_DefaultIconMessage;
    property                 ToolTip: string
      read                   FToolTip
      write                  SetToolTip;
    property                 PopupMenu: TPopupMenu
      read                   FPopupMenu
      write                  SetPopupMenu;
    property                 ApplicationIcon: Boolean
      read                   FApplicationIcon
      write                  SetApplicationIcon
      stored                 False;
    property                 OnUpdateIcon: TNotifyEvent
      read                   FOnUpdateIcon
      write                  FOnUpdateIcon;
    property                 OnClick: TNotifyEvent
      read                   FOnClick
      write                  FOnClick;
    property                 OnDblClick: TNotifyEvent
      read                   FOnDblClick
      write                  FOnDblClick;
    property                 OnMouse: TTrayMouseEvent
      read                   FOnMouse
      write                  FOnMouse;
    property                 Visible: Boolean
      read                   FVisible
      write                  SetVisible
      default                True;
    property                 IsMainIcon: Boolean
      read                   GetIsMainIcon
      write                  SetIsMainIcon
      default                False;
  end;

var
  IconTrayApplication: TIconTrayApplication = nil;

implementation

{ TIconTrayApplication }

constructor TIconTrayApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not Assigned(IconTrayApplication) and not (csDesigning in ComponentState)
  then
    IsMainIcon := True;
  FIcon := TIcon.Create;
  FIcon.Assign(Application.Icon);
  FApplicationIcon := True;
  FIcon.OnChange := IconChanged;
  FIconMessage := wm_DefaultIconMessage;

  { Init data structure }
  with FTrayIcon do
  begin
    CbSize := System.sizeof(FTrayIcon);
    uID := 1;
  end; { with FTrayIcon }
  FVisible := True;
end;

destructor TIconTrayApplication.Destroy;
begin
  Destroying;
  FUpdateIconDisabled := 0; { Forceer update enabled, om de boel te kunnen
                              opruimen }
  SetPopupMenu(nil);
  UpdateIcon;
  FIcon.Free;
  FIcon := nil;
  DeleteHandle;
  inherited Destroy;
  if IsMainIcon then
    IconTrayApplication := nil;
end;

procedure TIconTrayApplication.DeleteHandle;
begin
  if (FHandle <> 0) and IsWindow(FHandle) then
  begin
    DeallocateHWnd(FHandle);
    FHandle := 0;
  end;
end;

function TIconTrayApplication.GetIcon: TIcon;
begin
  Result := FIcon;
end;

procedure TIconTrayApplication.SetIcon(const Value: TIcon);
begin
  FIcon.Assign(Value);
end;

function TIconTrayApplication.StoreIcon: Boolean;
begin
  Result := not FApplicationIcon;
end;

procedure TIconTrayApplication.IconChanged(Sender: TObject);
begin
  FApplicationIcon := False;
  UpdateIcon;
end;

procedure TIconTrayApplication.SetIconMessage(const Value: Integer);
begin
  if Value <> FIconMessage then
  begin
    FIconMessage := Value;
    UpdateIcon;
  end;
end;

function TIconTrayApplication.GetHandle: HWnd;
begin
  HandleNeeded;
  Result := FHandle;
end;

procedure TIconTrayApplication.HandleNeeded;
begin
  if FHandle <> 0 then Exit;
  FHandle := AllocateHWnd(HandleWndProc);
end;

procedure TIconTrayApplication.HandleWndProc(var Msg: TMessage);
begin
  with Msg do
    try
      if Msg = wm_DefaultIconMessage then
        begin
          DoOnMouse(lParam);
          case lParam of
            WM_LBUTTONDOWN:   DoOnClick;
            WM_LBUTTONDBLCLK: DoOnDblClick;
          end { case }
        end
      else if Msg = WM_DESTROY then
        FHandle := 0
      else
        Result := DefWindowProc(FHandle, Msg, wParam, lParam);
    except
      Application.HandleException(Self);
    end
end;

procedure TIconTrayApplication.DisableUpdateIcon;
begin
  Inc(FUpdateIconDisabled);
end;

procedure TIconTrayApplication.EnableUpdateIcon;
begin
  Dec(FUpdateIconDisabled);
  if (FUpdateIconDisabled = 0) and FUpdateIconRequired then
    UpdateIcon;
end;

procedure TIconTrayApplication.UpdateIcon;
var
  Cmd: DWord;
  MustUpdate: Boolean;
  ToolTip: String;
begin
  if csDesigning in ComponentState then Exit;
  if FUpdateIconDisabled > 0 then
    FUpdateIconRequired := True
  else
    begin
      FUpdateIconRequired := False;

      if not FVisible or (csDestroying in ComponentState) then
        Cmd := NIM_DELETE
      else if FHandle = 0 then
        Cmd := NIM_ADD
      else
        Cmd := NIM_MODIFY;

      if Cmd <> NIM_DELETE then
        with FTrayIcon do
          begin
            ToolTip := szTip;
            MustUpdate :=
              (FHandle = 0) or
              (wnd <> Handle) or
              (hIcon <> FIcon.Handle) or
              (ToolTip <> FToolTip);
            if csDestroying in ComponentState then
              MustUpdate := False;
            if not MustUpdate then Exit;
            wnd := Handle;
            uCallBackMessage := FIconMessage;
            hIcon := FIcon.Handle;
            FToolTip := szTip;
            uFlags := 0;
            if FIconMessage > 0 then
              uFlags := uFlags or NIF_MESSAGE;
            if hIcon <> 0 then
              uFlags := uFlags or NIF_ICON;
            if Length(FToolTip) > 0 then
              uFlags := uFlags or NIF_TIP;
          end
        else
          FTrayIcon.uFlags := 0;

        Shell_NotifyIcon(Cmd, @FTrayIcon);
        if Cmd = NIM_DELETE then
          DeleteHandle;
        DoOnUpdateIcon;
    end;
end;

procedure TIconTrayApplication.DoOnUpdateIcon;
begin
  if Assigned(FOnUpdateIcon) then FOnUpdateIcon(Self);
end;

procedure TIconTrayApplication.ShowOwnerForm;
var
  Frm: TCustomForm;
begin
  if Assigned(FOwnerForm) then
    Frm := FOwnerForm
  else
    Frm := Owner as TCustomForm;
  with Frm do
  begin
    ShowWindow(Handle, sw_ShowNormal);
    ShowWindow(Application.Handle, sw_ShowNormal);
    Show;
    SetForegroundWindow(Handle);
    Show; { enables the creation of window controls}
  end;
end;

procedure TIconTrayApplication.DoOnClick;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TIconTrayApplication.DoOnDblClick;
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

procedure TIconTrayApplication.SetToolTip(const Value: string);
begin
  if Value <> FToolTip then
  begin
    FToolTip := Value;
    UpdateIcon;
  end;
end;

procedure TIconTrayApplication.DoOnMouse(const MessageId: Longint);
var
  Mouse: TPoint;
begin
  if (MessageId = WM_RBUTTONUP) and (FPopupMenu <> nil) then
  begin
    GetCursorPos(Mouse);
    FPopupMenu.PopupComponent := Self;
    FPopupMenu.Popup(Mouse.X, Mouse.Y);
  end;
  if Assigned(FOnMouse) then FOnMouse(Self, MessageId);
end;

procedure TIconTrayApplication.Notification(AComponent: TComponent;
            Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FPopupMenu then
      FPopupMenu := nil
    else if AComponent = FOwnerForm then
      OwnerForm := nil;
  end;
end;

procedure TIconTrayApplication.SetApplicationIcon(const Value: Boolean);
begin
  if Value <> FApplicationIcon then
  begin
    if Value then
      FIcon.Assign(Application.Icon);
    FApplicationIcon := Value;
  end;
end;

procedure TIconTrayApplication.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    UpdateIcon;
  end;
end;

procedure TIconTrayApplication.Loaded;
begin
  inherited;
  UpdateIcon;
end;

procedure TIconTrayApplication.Hide;
begin
  Visible := False;
end;

procedure TIconTrayApplication.Show;
begin
  Visible := True;
end;

procedure TIconTrayApplication.SetPopupMenu(const Value: TPopupMenu);
begin
  if Value <> FPopupMenu then
  begin
    if Assigned(FPopupMenu) and (FPopupMenu.PopupComponent = Self) then
      FPopupMenu.PopupComponent := nil;
    FPopupMenu := Value;
  end;
end;

procedure TIconTrayApplication.SetOwnerForm(const Value: TCustomForm);
begin
  if Value <> FOwnerForm then
  begin
    if Assigned(FOwnerForm) then
      FOwnerForm.RemoveFreeNotification(Self);
    FOwnerForm := Value;
    if Assigned(FOwnerForm) then
      FOwnerForm.FreeNotification(Self);
  end;
end;

function TIconTrayApplication.GetIsMainIcon: Boolean;
begin
  Result := (IconTrayApplication = Self);
end;

procedure TIconTrayApplication.SetIsMainIcon(const Value: Boolean);
begin
  if Value then
    IconTrayApplication := Self
  else if IconTrayApplication = Self then
    IconTrayApplication := nil;
end;

initialization
  Application.ShowMainForm := False;
  ShowWindow(Application.Handle, SW_HIDE);
end.

