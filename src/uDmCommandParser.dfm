object dmCommandParser: TdmCommandParser
  OldCreateOrder = False
  Height = 150
  Width = 215
  object CommandLine: TCommandLine
    UsageParameters.Introduction = 'Command line syntax error. Usage:'
    UsageParameters.Parameters = 'Parameters:'
    UsageParameters.Options = 'Options:'
    UsageParameters.Switches = 'Switches:'
    UsageParameters.DateTimeFormatUsage = ' (%s)'
    UsageParameters.MaxWidth = 80
    Left = 88
    Top = 56
    object prmInputfile: TCommandLineParameter
      Required = True
      ElementName = 'inputfiles'
      AllowList = True
      ValueDescription = 'List of input files to be untabbed'
    end
    object swTabSize: TCommandLineSwitch
      ValueType = cevInteger
      ElementName = 'tabsize'
      ValueRequired = True
      ValueDescription = 'Size of a single tab'
    end
  end
end
