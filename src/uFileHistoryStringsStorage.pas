unit uFileHistoryStringsStorage;

interface

uses
  uFileHistory, Classes;

type
  TFileHistoryStringsStorage = class(TInterfacedObject, IFileHistoryStorage)
  private
    FStrings: TStringList;
    function GetStrings: TStrings;
  protected
    // IFileHistoryStorage
    function ReadFileHistory(Items: TStrings): Boolean;
    function WriteFileHistory(Items: TStrings): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Strings: TStrings read GetStrings;
  end;

implementation

uses
  SysUtils;

{ TFileHistoryStringsStorage }

constructor TFileHistoryStringsStorage.Create;
begin
  inherited;
  FStrings := TStringList.Create;
end;

destructor TFileHistoryStringsStorage.Destroy;
begin
  FreeAndNil(FStrings);
  inherited;
end;

function TFileHistoryStringsStorage.GetStrings: TStrings;
begin
  Result := FStrings;
end;

function TFileHistoryStringsStorage.ReadFileHistory(Items: TStrings): Boolean;
begin
  Items.Assign(FStrings);
  Result := True;
end;

function TFileHistoryStringsStorage.WriteFileHistory(Items: TStrings): Boolean;
begin
  FStrings.Assign(Items);
  Result := True;
end;

end.
