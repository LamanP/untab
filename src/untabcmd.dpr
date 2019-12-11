program untabcmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Classes,
  uCmdLine in '..\..\easybook\Source\Lib\uCmdLine.pas',
  uDmCommandParser in 'uDmCommandParser.pas' {dmCommandParser: TDataModule};

begin
  try
    // Parse the command line
    with TDmCommandParser.create(nil) do
      try

      finally
        Free;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
