unit uFileSetApi;

interface

type
  IFileSet = interface(IUnknown)
    ['{40B3185D-6C01-48C1-A204-955086DD1B8C}']
    function GetCount: Integer;
    function GetFileName(Index: Integer): string;
    function GetFileSetName: string;

    function AddFileName(const AFileName: string): Integer;
    procedure Remove(const Index: Integer);
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFileAs(const AFileName: string);
    procedure SaveToFile;

    property Count: Integer read GetCount;
    property FileName[Index: Integer]: string read GetFileName; default;
    property FileSetName: string read GetFileSetName;
  end;

implementation

end.
