unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uFileSetApi,
  uFileSetImpl, System.Actions, Vcl.ActnList, Vcl.StdActns, Vcl.Menus,
  System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, uFileHistory,
  uFileHistoryStringsStorage;

type
  TMainForm = class;

  TStats = record
    files: Integer;
    tabs: Integer;
    lines: Integer;
    SpacesTrimmed: Integer;
  end;

  TFileSetHistoryStorage = class(TInterfacedObject, IFileHistoryStorage)
  private
    FMainForm: TMainForm;
    function ReadFileHistory(Items: TStrings): Boolean;
    function WriteFileHistory(Items: TStrings): Boolean;
    function GetHistoryFileName: string;
  public
    constructor Create(const AMainForm: TMainForm); reintroduce;
  end;

  TMainForm = class(TForm, IFileHistoryUse)
    lbFiles: TListBox;
    Splitter1: TSplitter;
    memoLog: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    editTabsize: TEdit;
    btnGo: TButton;
    CBBackup: TCheckBox;
    CbTrim: TCheckBox;
    Panel2: TPanel;
    Label3: TLabel;
    LabelFileSet: TLabel;
    MainMenu: TMainMenu;
    ActionList: TActionList;
    ImageListActions: TImageList;
    FileMenu: TMenuItem;
    FileOpenFile: TFileOpen;
    Open1: TMenuItem;
    FileOpenFileSet: TFileOpen;
    Open2: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    FileSaveSet: TFileSaveAs;
    SaveAs1: TMenuItem;
    ToolButton3: TToolButton;
    FileExit1: TFileExit;
    MenuSeparatorAfterHistory: TMenuItem;
    Exit1: TMenuItem;
    N2: TMenuItem;
    procedure btnGoClick(Sender: TObject);
    procedure FileOpenFileAccept(Sender: TObject);
    procedure FileOpenFileSetBeforeExecute(Sender: TObject);
    procedure FileSaveSetBeforeExecute(Sender: TObject);
    procedure FileSaveSetAccept(Sender: TObject);
    procedure FileOpenFileSetAccept(Sender: TObject);
    procedure lbFilesData(Control: TWinControl; Index: Integer;
      var Data: string);
    procedure FileMenuClick(Sender: TObject);
  private
    FTabSize: Integer;
    FFileSet: IFileSet;
    FFileSetHistory: IFileHistory;
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
    function Convert(const Filename: string; var Stats: TStats): TStats; overload;
    procedure Convert(src, dst: TStrings; var Stats: TStats); overload;
    function ConvertLine(const Line: string; var Stats: TStats): string;
    procedure UpdateFileSetName;
    function EnsureDefaultFileSetFolder: string;
    function GetInitialFileSetFolder: string;
    procedure SetInitialFileSetFolder(const Value: string);
    procedure InvalidateListBox;
  protected
    procedure CreateWnd; override;
    property InitialFileSetFolder: string read
      GetInitialFileSetFolder write SetInitialFileSetFolder;
    procedure UsingFromFileSetHistory(const AFileName: string);
    procedure IFileHistoryUse.Using = UsingFromFileSetHistory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ShellAPI, uFileHistoryMenu;

{ TFileSetHistoryStorage }

constructor TFileSetHistoryStorage.Create(const AMainForm: TMainForm);
begin
  inherited Create;
  FMainForm := AMainForm;
end;

function TFileSetHistoryStorage.GetHistoryFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(FMainForm.EnsureDefaultFileSetFolder) +
    'FileSetHistory.txt';
end;

function TFileSetHistoryStorage.ReadFileHistory(Items: TStrings): Boolean;
var
  HistoryFile: string;
begin
  HistoryFile := GetHistoryFileName;
  if FileExists(HistoryFile) then
    Items.LoadFromFile(HistoryFile)
  else
    items.Clear;
  Result := True;
end;

function TFileSetHistoryStorage.WriteFileHistory(Items: TStrings): Boolean;
begin
  Items.SaveToFile(GetHistoryFileName);
  Result := True;
end;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FFileSet := newFileSet;
  UpdateFileSetName;

  // Load hister
  FFileSetHistory := CreateHistory;
  FFileSetHistory.Read(TFileSetHistoryStorage.Create(Self));
  FFileSetHistory.RemoveNonExistentFiles;
  FFileSetHistory.UseHandler := Self;
end;

destructor TMainForm.Destroy;
begin
  FFileSetHistory.Write(TFileSetHistoryStorage.Create(Self));
  inherited;
end;

procedure TMainForm.AcceptFiles(var msg: TMessage);
var
  I, FileDragCount: integer;
  acFileName : array [0..MAX_PATH] of char;
begin
  FileDragCount := DragQueryFile(msg.WParam, $FFFFFFFF, acFileName, MAX_PATH);

  for I := 0 to FileDragCount-1 do
  begin
    DragQueryFile( msg.WParam, I,acFileName, MAX_PATH );
    FFileSet.AddFileName(acFileName);
  end;
  DragFinish( msg.WParam );
  InvalidateListBox;
end;

procedure TMainForm.btnGoClick(Sender: TObject);
var
  I : Integer;
  deltaStats, stats: TStats;
resourcestring
  SFileDone = 'Lines: %8d, Tabs removed: %8d, Blanks trimmed: %8d. "%s"';
  SDone = 'All done, Files: %8d, Lines: %8d, Tabs removed: %8d, Blanks trimmed: %8d.';
begin
  if not TryStrToInt(editTabsize.Text, FTabSize) or (FTabSize <= 0) then
    raise Exception.Create('Invalid tab size');

  FillChar(stats, SizeOf(stats), 0);
  memoLog.Clear;
  for I := 0 to FFileSet.Count - 1 do
  begin
    deltaStats := Convert(FFileSet[I], stats);
    if (deltaStats.tabs > 0) or (deltaStats.SpacesTrimmed > 0) then
    memoLog.Lines.Add(Format(SFileDone, [
      deltaStats.lines,
      deltaStats.tabs,
      deltaStats.SpacesTrimmed,
      FFileSet[I]
    ]));
  end;
  memoLog.Lines.Add(Format(SDone, [stats.files, stats.lines, stats.tabs, stats.SpacesTrimmed]));
end;

// Returns the delta in stats
function TMainForm.Convert(const Filename: string; var Stats: TStats): TStats;
var
  src,dst: TStrings;
  BackupFile: string;
begin
  Result := Stats;
  Inc(Stats.files);
  src := TStringList.Create;
  try
    dst := TStringList.Create;
    try
      src.LoadFromFile(FileName);

      if CBBackup.Checked then
      begin
        // Make a backup file
        BackupFile := IncludeTrailingPathDelimiter(ExtractFilePath(Filename)) + 'backup';
        ForceDirectories(BackupFile);
        src.SaveToFile(BackupFile + '\' + ExtractFileName(Filename));
      end;
      Convert(src, dst, stats);
      dst.SaveToFile(Filename);
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
  Result.files := Stats.files - Result.files;
  Result.tabs := Stats.tabs - Result.tabs;
  Result.lines := Stats.lines - Result.lines;
  Result.SpacesTrimmed := Stats.SpacesTrimmed - Result.SpacesTrimmed;
end;

procedure TMainForm.Convert(src, dst: TStrings; var Stats: TStats);
var
  I: Integer;
  S: string;
begin
  dst.Clear;
  for I := 0 to src.Count - 1 do
  begin
    Inc(Stats.lines);
    S := ConvertLine(src[I], stats);
    if CbTrim.Checked then
    begin
      Inc(Stats.SpacesTrimmed, Length(S));
      S := TrimRight(S);
      Dec(Stats.SpacesTrimmed, Length(S));
    end;
    dst.Add(S);
  end;
end;

function TMainForm.ConvertLine(const Line: string; var Stats: TStats): string;
var
  I, J, Stop: Integer;
begin
  Result := '';
  J := 1;
  for I := 1 to Length(Line) do
  begin
    if Line[I] = #9 then
      begin
        Inc(Stats.tabs);
        Stop := ((J - 1) div FTabSize + 1) * FTabSize + 1;
        while (J < Stop) do
        begin
          Result := Result + ' ';
          Inc(J);
        end;
      end
    else
      begin
        Result := Result + Line[I];
        Inc(J);
      end;
  end;
end;

procedure TMainForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles( Handle, True );
end;

procedure TMainForm.FileMenuClick(Sender: TObject);
begin
  UpdateHistoryMenu(MenuSeparatorAfterHistory, FFileSetHistory);
end;

procedure TMainForm.FileOpenFileAccept(Sender: TObject);
var
  Files: TStrings;
  I: Integer;
begin
  Files := FileOpenFile.Dialog.Files;
  for I := 0 to Files.Count - 1 do
    FFileSet.AddFileName(Files[I]);
  InvalidateListBox;
end;

procedure TMainForm.InvalidateListBox;
begin
  lbFiles.Count := FFileSet.Count;
  lbFiles.Invalidate;
end;

procedure TMainForm.lbFilesData(Control: TWinControl; Index: Integer;
  var Data: string);
begin
  Data := FFileSet[Index];
end;

function BuildDefaultFileSetFolder: string;
begin
  with TStringBuilder.Create do
    try
      Append(IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')))
      .Append('Competer\Untab\FileSets');
      Result := ToString;
    finally
      Free;
    end;
end;

function TMainForm.EnsureDefaultFileSetFolder: string;
begin
  Result := BuildDefaultFileSetFolder;
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

procedure TMainForm.FileOpenFileSetAccept(Sender: TObject);
begin
  FFileSetHistory.Use(FileOpenFileSet.Dialog.FileName);
  InitialFileSetFolder := ExtractFilePath(FileOpenFileSet.Dialog.FileName);
end;

procedure TMainForm.FileOpenFileSetBeforeExecute(Sender: TObject);
begin
  FileOpenFileSet.Dialog.InitialDir := InitialFileSetFolder;
end;

procedure TMainForm.FileSaveSetAccept(Sender: TObject);
begin
  FFileSet.SaveToFileAs(FileSaveSet.Dialog.FileName);
  InitialFileSetFolder := ExtractFilePath(FileSaveSet.Dialog.FileName);
  FFileSetHistory.FileToFront(FFileSet.FileSetName);
  UpdateFileSetName;
end;

procedure TMainForm.FileSaveSetBeforeExecute(Sender: TObject);
begin
  FileSaveSet.Dialog.InitialDir := InitialFileSetFolder;
end;

function TMainForm.GetInitialFileSetFolder: string;
var
  LatestFolderFile: string;
  Utf8Value: UTF8String;
begin
  Result := EnsureDefaultFileSetFolder;
  LatestFolderFile := IncludeTrailingPathDelimiter(Result) + 'LatestFileSetFolder.txt';
  if FileExists(LatestFolderFile) then
  begin
    with TFileStream.Create(fmOpenRead or fmExclusive) do
      try
        SetLength(Utf8Value, Size);
        Read(Utf8Value[1], Size);
      finally
        Free;
      end;
    Result := UTF8ToString(Utf8Value);
  end;
end;

procedure TMainForm.SetInitialFileSetFolder(const Value: string);
var
  LatestFolderFile: string;
  Utf8Value: UTF8String;
begin
  LatestFolderFile := IncludeTrailingPathDelimiter(EnsureDefaultFileSetFolder) + 'LatestFileSetFolder.txt';
  Utf8Value := UTF8Encode(Value);
  with TFileStream.Create(LatestFolderFile, fmCreate) do
    try
      Write(utf8Value[1], Length(Utf8Value));
    finally
      Free;
    end;
end;

procedure TMainForm.UpdateFileSetName;
resourcestring
  SUnnamedFileSet = '<unnamed>';
begin
  if Length(FFileSet.FileSetName) = 0 then
    LabelFileSet.Caption := SUnnamedFileSet
  else
    LabelFileSet.Caption := FFileSet.FileSetName;
end;

procedure TMainForm.UsingFromFileSetHistory(const AFileName: string);
begin
  FFileSet.LoadFromFile(AFileName);
  UpdateFileSetName;
  InvalidateListBox;
end;

end.

