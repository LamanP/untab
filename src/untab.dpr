program untab;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uUntabEngine in 'uUntabEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
