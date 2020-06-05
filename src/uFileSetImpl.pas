unit uFileSetImpl;

interface

uses uFileSetApi;

function newFileSet: IFileSet;

implementation

uses
  Classes, SysUtils;

type
  TFileSetImpl = class(TInterfacedObject, IFileSet)
  private
    FFileSetName: string;
    FFileNames: TStringList;
    function GetCount: Integer;
    function GetFileName(Index: Integer): string;
    function GetFileSetName: string;

    function AddFileName(const AFileName: string): Integer;
    procedure Remove(const Index: Integer);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFileAs(const AFileName: string);
    procedure SaveToFile;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

function newFileSet: IFileSet;
begin
  Result := TFileSetImpl.Create;
end;

{ TFileSetImpl }

constructor TFileSetImpl.Create;
begin
  inherited;
  FFileNames := TStringList.Create;
  FFileNames.Sorted := True;
  FFileNames.Duplicates := dupIgnore;
  FFileNames.CaseSensitive := False;
end;

destructor TFileSetImpl.Destroy;
begin
  FreeAndNil(FFileNames);
  inherited;
end;

function TFileSetImpl.AddFileName(const AFileName: string): Integer;
begin
  Result := FFileNames.Add(AFileName);
end;

function TFileSetImpl.GetCount: Integer;
begin
  Result := FFileNames.Count;
end;

function TFileSetImpl.GetFileName(Index: Integer): string;
begin
  Result := FFileNames[Index];
end;

function TFileSetImpl.GetFileSetName: string;
begin
  Result := FFileSetName;
end;

procedure TFileSetImpl.LoadFromFile(const AFileName: string);
begin
  FFileNames.LoadFromFile(AFileName);
  FFileSetName := AFileName;
end;

procedure TFileSetImpl.Remove(const Index: Integer);
begin
  FFileNames.Delete(Index);
end;

procedure TFileSetImpl.SaveToFile;
begin
  SaveToFileAs(FFileSetName);
end;

procedure TFileSetImpl.SaveToFileAs(const AFileName: string);
begin
  FFileNames.SaveToFile(AFileName);
  FFileSetName := AFileName;
end;

end.
