object VBServerMethods: TVBServerMethods
  OldCreateOrder = False
  OnCreate = DSServerModuleCreate
  OnDestroy = DSServerModuleDestroy
  Height = 389
  Width = 716
  object conFB: TFDConnection
    FetchOptions.AssignedValues = [evDetailDelay]
    FetchOptions.DetailDelay = 400
    FormatOptions.AssignedValues = [fvMapRules, fvDataSnapCompatibility]
    FormatOptions.OwnMapRules = True
    FormatOptions.MapRules = <
      item
        SourceDataType = dtWideString
        TargetDataType = dtAnsiString
      end
      item
        SourceDataType = dtFmtBCD
        TargetDataType = dtCurrency
      end
      item
        SourceDataType = dtSingle
        TargetDataType = dtDouble
      end>
    FormatOptions.DataSnapCompatibility = True
    ResourceOptions.AssignedValues = [rvCmdExecMode, rvAutoReconnect]
    ResourceOptions.AutoReconnect = True
    UpdateOptions.AssignedValues = [uvAutoCommitUpdates]
    LoginPrompt = False
    Transaction = trnFB
    OnError = conFBError
    BeforeConnect = conFBBeforeConnect
    Left = 30
    Top = 20
  end
  object FDGUIxWaitCursor: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 525
    Top = 20
  end
  object FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink
    Left = 525
    Top = 75
  end
  object FDStanStorageJSONLink: TFDStanStorageJSONLink
    Left = 525
    Top = 140
  end
  object FDStanStorageBinLink: TFDStanStorageBinLink
    Left = 525
    Top = 200
  end
  object qrySQL: TFDQuery
    ActiveStoredUsage = [auDesignTime]
    FilterOptions = [foCaseInsensitive]
    Connection = conFB
    FormatOptions.AssignedValues = [fvDataSnapCompatibility]
    FormatOptions.DataSnapCompatibility = True
    Left = 95
    Top = 20
  end
  object cmdGeneric: TFDCommand
    Connection = conFB
    ActiveStoredUsage = [auDesignTime]
    Left = 160
    Top = 20
  end
  object sprGeneric: TFDStoredProc
    ActiveStoredUsage = [auDesignTime]
    Connection = conFB
    ResourceOptions.AssignedValues = [rvStorePrettyPrint]
    ResourceOptions.StorePrettyPrint = True
    StoredProcName = 'SP_GEN_BILLABLE_SUMMARY_TABLE'
    Left = 240
    Top = 20
  end
  object trnFB: TFDTransaction
    Connection = conFB
    Left = 30
    Top = 75
  end
  object qryInsert: TFDQuery
    Connection = conFB
    Left = 95
    Top = 75
  end
end
