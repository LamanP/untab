unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uIconTray;

type
  TStats = record
    files: Integer;
    tabs: Integer;
    lines: Integer;
    SpacesTrimmed: Integer;
  end;

  TMainForm = class(TForm)
    lbFiles: TListBox;
    Splitter1: TSplitter;
    memoLog: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    editTabsize: TEdit;
    btnGo: TButton;
    CBBackup: TCheckBox;
    BtnBrowse: TButton;
    OpenDialog: TOpenDialog;
    CbTrim: TCheckBox;
    procedure btnGoClick(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FTabSize: Integer;
    FIconTray: TIconTrayApplication;
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
    function Convert(const Filename: string; var Stats: TStats): TStats; overload;
    procedure Convert(src, dst: TStrings; var Stats: TStats); overload;
    function ConvertLine(const Line: string; var Stats: TStats): string;
    procedure ShowFromIconTray(Sender: TObject);
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  ShellAPI;

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FIconTray := TIconTrayApplication.Create(Self);
  FIconTray.OnClick := ShowFromIconTray;
  FIconTray.OwnerForm := Self;
  FIconTray.Show;
  FIconTray.Icon := Application.Icon;
  Visible := True;
end;

procedure TMainForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles( Handle, True );
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FIconTray);
  inherited;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caNone;
end;

procedure TMainForm.ShowFromIconTray(Sender: TObject);
begin
  FIconTray.ShowOwnerForm;
end;

procedure TMainForm.AcceptFiles(var msg: TMessage);
var
  I, FileDragCount: integer;
  acFileName : array [0..MAX_PATH] of char;
begin
  FileDragCount := DragQueryFile(msg.WParam, $FFFFFFFF, acFileName, MAX_PATH);

  lbFiles.Items.BeginUpdate;
  try
    for I := 0 to FileDragCount-1 do
    begin
      DragQueryFile( msg.WParam, I,acFileName, MAX_PATH );
      lbFiles.Items.Add(acFileName);
    end;
  finally
    lbFiles.Items.EndUpdate;
  end;

  DragFinish( msg.WParam );
end;

procedure TMainForm.BtnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    lbFiles.Items.AddStrings(OpenDialog.Files);
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
  for I := 0 to lbFiles.Items.Count - 1 do
  begin
    deltaStats := Convert(lbFiles.Items[I], stats);
    if (deltaStats.tabs > 0) or (deltaStats.SpacesTrimmed > 0) then
    memoLog.Lines.Add(Format(SFileDone, [
      deltaStats.lines,
      deltaStats.tabs,
      deltaStats.SpacesTrimmed,
      lbFiles.Items[I]
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

end.

