unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uFileDropper;

type
  TMainForm = class(TForm, IFileReceiver)
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
  private
    FFileDropHandler: IFileDropHandler;
    FTabSize: Integer;
    procedure DropFiles(Sender: IFileDropHandler; Files: TStrings);
    procedure Convert(const Filename: string); overload;
    procedure Convert(src, dst: TStrings); overload;
    function ConvertLine(const Line: string): string;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.BtnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    lbFiles.Items.AddStrings(OpenDialog.Files);
end;

procedure TMainForm.btnGoClick(Sender: TObject);
var
  I : Integer;
resourcestring
  SDone = 'Done';
begin
  if not TryStrToInt(editTabsize.Text, FTabSize) or (FTabSize <= 0) then
    raise Exception.Create('Invalid tab size');
  for I := 0 to lbFiles.Items.Count - 1 do
    Convert(lbFiles.Items[I]);
  memoLog.Lines.Add(SDone);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  FFileDropHandler := GetFileDropHandler(Self);
  inherited;
end;

procedure TMainForm.DropFiles(Sender: IFileDropHandler; Files: TStrings);
begin
  lbFiles.Items.AddStrings(Files);
end;

procedure TMainForm.CreateWnd;
begin
  inherited;
  FFileDropHandler.SetWindowHandle(Handle);
end;

procedure TMainForm.Convert(const Filename: string);
var
  src,dst: TStrings;
  BackupFile: string;
begin
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
      Convert(src, dst);
      dst.SaveToFile(Filename);
    finally
      dst.Free;
    end;
  finally
    src.Free;
  end;
end;

procedure TMainForm.Convert(src, dst: TStrings);
var
  I: Integer;
  S: string;
begin
  dst.Clear;
  for I := 0 to src.Count - 1 do
  begin
    S := ConvertLine(src[I]);
    if CbTrim.Checked then
      S := Trim(S);
    dst.Add(S);
  end;
end;

function TMainForm.ConvertLine(const Line: string): string;
var
  I, J, Stop: Integer;
begin
  Result := '';
  J := 1;
  for I := 1 to Length(Line) do
  begin
    if Line[I] = #9 then
      begin
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
