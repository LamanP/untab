unit uDmCommandParser;

interface

uses
  System.SysUtils, System.Classes, uCmdLine;

type
  TdmCommandParser = class(TDataModule)
    CommandLine: TCommandLine;
    prmInputfile: TCommandLineParameter;
    swTabSize: TCommandLineSwitch;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmCommandParser: TdmCommandParser;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
