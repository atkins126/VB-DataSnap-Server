object VBDSService: TVBDSService
  OldCreateOrder = False
  DisplayName = 'VB Service'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  Height = 271
  Width = 415
  object DSServer: TDSServer
    Left = 96
    Top = 11
  end
  object DSTCPServerTransport: TDSTCPServerTransport
    Port = 20270
    Server = DSServer
    Filters = <
      item
        FilterId = 'PC1'
        Properties.Strings = (
          'Key=V!Zamwe2Rh0fbprL')
      end
      item
        FilterId = 'RSA'
        Properties.Strings = (
          'UseGlobalKey=true'
          'KeyLength=1024'
          'KeyExponent=3')
      end
      item
        FilterId = 'ZLibCompression'
        Properties.Strings = (
          'CompressMoreThan=1024')
      end>
    AuthenticationManager = DSAuthenticationManager
    Left = 96
    Top = 73
  end
  object DSHTTPService: TDSHTTPService
    HttpPort = 20275
    Server = DSServer
    DSPort = 20270
    Filters = <
      item
        FilterId = 'PC1'
        Properties.Strings = (
          'Key=ab7cAWYuRB0BlvbJ')
      end
      item
        FilterId = 'RSA'
        Properties.Strings = (
          'UseGlobalKey=true'
          'KeyLength=1024'
          'KeyExponent=3')
      end
      item
        FilterId = 'ZLibCompression'
        Properties.Strings = (
          'CompressMoreThan=1024')
      end>
    AuthenticationManager = DSAuthenticationManager
    Left = 96
    Top = 135
  end
  object DSAuthenticationManager: TDSAuthenticationManager
    OnUserAuthenticate = DSAuthenticationManagerUserAuthenticate
    OnUserAuthorize = DSAuthenticationManagerUserAuthorize
    Roles = <>
    Left = 96
    Top = 197
  end
  object DSServerClass: TDSServerClass
    OnGetClass = DSServerClassGetClass
    Server = DSServer
    Left = 200
    Top = 11
  end
end
