program untab;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uUntabEngine in 'uUntabEngine.pas',
  uFileSetApi in 'uFileSetApi.pas',
  uFileSetImpl in 'uFileSetImpl.pas',
  uFileHistory in 'uFileHistory.pas',
  uFileHistoryStringsStorage in 'uFileHistoryStringsStorage.pas',
  uFileHistoryMenu in 'uFileHistoryMenu.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
