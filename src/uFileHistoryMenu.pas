unit uFileHistoryMenu;

interface

uses
  Classes, Menus, uFileHistory, SysUtils;

procedure UpdateHistoryMenu(BeforeMenuItem: TMenuItem; History: IFileHistory);

implementation

type
  THistoryMenuItem = class(TMenuItem)
  private
    FHistory: IFileHistory;
    FFileName: string;
  public
    constructor Create(Parent: TMenuItem; InsertAt: Integer; History: IFileHistory; const AFileName: string); reintroduce;
    procedure Click; override;
  end;

  THistoryStorage = class(TInterfacedObject, IFileHistoryStorage)
  private
    FItems: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    function ReadFileHistory(Items: TStrings): Boolean;
    function WriteFileHistory(Items: TStrings): Boolean;
    property Items: TStrings read FItems;
  end;

procedure UpdateHistoryMenu(BeforeMenuItem: TMenuItem; History: IFileHistory);
var
  ParentMenuItem: TMenuItem;
  I: Integer;
  Storage: THistoryStorage;
  StorageIntf: IFileHistoryStorage;
  Items: TStrings;
  InsertAt: Integer;
begin
  ParentMenuItem := BeforeMenuItem.Parent;
  for I := ParentMenuItem.Count - 1 downto 0 do
    if (ParentMenuItem[I] is THistoryMenuItem) then
      ParentMenuItem.Remove(ParentMenuItem[I]);
  Storage := THistoryStorage.Create;
  StorageIntf := Storage; // Ref count!!!
  History.Write(Storage);
  Items := Storage.Items;
  InsertAt := ParentMenuItem.IndexOf(BeforeMenuItem);
  for I := Items.Count - 1 downto 0 do
    THistoryMenuItem.Create(ParentMenuItem, InsertAt, History, Items[I]);
end;

{ THistoryStorage }

constructor THistoryStorage.Create;
begin
  inherited;
  FItems := TStringList.Create;
end;

destructor THistoryStorage.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function THistoryStorage.ReadFileHistory(Items: TStrings): Boolean;
begin
  Items.Assign(FItems);
  Result := True;
end;

function THistoryStorage.WriteFileHistory(Items: TStrings): Boolean;
begin
  FItems.Assign(Items);
  Result := True;
end;

{ THistoryMenuItem }

constructor THistoryMenuItem.Create(Parent: TMenuItem; InsertAt: Integer; History: IFileHistory; const AFileName: string);
begin
  inherited Create(Parent);
  FFileName := AFileName;
  FHistory := History;
  Caption := AFileName;
  Parent.Insert(InsertAt, Self);
end;

procedure THistoryMenuItem.Click;
begin
  FHistory.Use(FFileName);
end;

end.
