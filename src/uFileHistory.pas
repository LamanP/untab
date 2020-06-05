unit uFileHistory;

interface

uses
  Classes;

type
  IFileHistoryUse = interface;
  IFileHistoryStorage = interface;

  // Interface to represent a file history - see UpdateMenu function
  IFileHistory = interface(IUnknown)
    ['{DFDC29D6-CCC3-4ED0-9E8C-B19B1E999787}']
    function GetMaxItems: Integer;
    procedure SetMaxItems(Value: Integer);
    function GetUseHandler: IFileHistoryUse;
    procedure SetUseHandler(Value: IFileHistoryUse);

    // Method to use a history item
    procedure Use(const AFileName: string);
    procedure FileToFront(const AFileName: string);
    function CalcExistingFileCount: Integer;
    procedure RemoveNonExistentFiles;

    function Read(Storage: IFileHistoryStorage): Boolean;
    function Write(Storage: IFileHistoryStorage): Boolean;

    property MaxItems: Integer read GetMaxItems write SetMaxItems;

    // Assign this property to process uses of history items
    property UseHandler: IFileHistoryUse read GetUseHandler write SetUseHandler;
  end;

  // Interface to read/write file history
  IFileHistoryStorage = interface(IUnknown)
    ['{3111F41F-53C2-46F8-B687-F9025F5DA6CB}']

    function ReadFileHistory(Items: TStrings): Boolean;
    function WriteFileHistory(Items: TStrings): Boolean;
  end;

  // Callback to execute uses of files
  IFileHistoryUse = interface(IUnknown)
    ['{83A46DE7-1958-463E-8409-46B9FF39CB70}']
    procedure Using(const AFileName: string);
  end;

function CreateHistory: IFileHistory;

implementation

uses
  SysUtils;

type
  TFileHistory = class(TInterfacedObject, IFileHistory)
  private
    FMaxItems: Integer;
    FItems: TStrings;
    FUseHandler: IFileHistoryUse;
    procedure Prune;
    function CheckFileExistence(const RemoveNonExistent: Boolean): Integer;
  protected
    // IFileHistory
    function GetMaxItems: Integer;
    procedure SetMaxItems(Value: Integer);
    function GetUseHandler: IFileHistoryUse;
    procedure SetUseHandler(Value: IFileHistoryUse);
    procedure Use(const AFileName: string);
    procedure FileToFront(const AFileName: string);
    function Read(Storage: IFileHistoryStorage): Boolean;
    function Write(Storage: IFileHistoryStorage): Boolean;
    function CalcExistingFileCount: Integer;
    procedure RemoveNonExistentFiles;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function CreateHistory: IFileHistory;
begin
  Result := TFileHistory.Create;
end;

{ TFileHistory }

constructor TFileHistory.Create;
begin
  inherited;
  FMaxItems := 10;
  FItems := TStringList.Create;
end;

destructor TFileHistory.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TFileHistory.GetMaxItems: Integer;
begin
  Result := FMaxItems;
end;

procedure TFileHistory.SetMaxItems(Value: Integer);
begin
  if Value <> FMaxItems then
  begin
    FMaxItems := Value;
    Prune;
  end;
end;

procedure TFileHistory.Prune;
var
  Cnt: Integer;
begin
  Cnt := FItems.Count;
  while Cnt > FMaxItems do
  begin
    Dec(Cnt);
    FItems.Delete(Cnt);
  end;
end;

function TFileHistory.GetUseHandler: IFileHistoryUse;
begin
  Result := FUseHandler;
end;

procedure TFileHistory.SetUseHandler(Value: IFileHistoryUse);
begin
  FUseHandler := Value;
end;

procedure TFileHistory.Use(const AFileName: string);
begin
  FileToFront(AFileName);
  if Assigned(FUseHandler) then
    FUseHandler.Using(AFileName);
end;

procedure TFileHistory.FileToFront(const AFileName: string);
var
  I: Integer;
begin
  I := FItems.IndexOf(AFileName);
  if I < 0 then
    begin
      FItems.Insert(0, AFileName);
      Prune;
    end
  else
    FItems.Move(I, 0);
end;

function TFileHistory.Read(Storage: IFileHistoryStorage): Boolean;
begin
  FItems.BeginUpdate;
  try
    FItems.Clear;
    Result := Storage.ReadFileHistory(FItems);
  finally
    FItems.EndUpdate;
  end;
end;

function TFileHistory.CheckFileExistence(const RemoveNonExistent: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := FItems.Count - 1 downto 0 do
    if not FileExists(FItems[I]) then
    begin
      Inc(Result);
      if RemoveNonExistent then
        FItems.Delete(I);
    end;
end;

function TFileHistory.CalcExistingFileCount: Integer;
begin
  Result := CheckFileExistence(False);
end;

procedure TFileHistory.RemoveNonExistentFiles;
begin
  CheckFileExistence(True);
end;

function TFileHistory.Write(Storage: IFileHistoryStorage): Boolean;
begin
  Result := Storage.WriteFileHistory(FItems);
end;

end.
