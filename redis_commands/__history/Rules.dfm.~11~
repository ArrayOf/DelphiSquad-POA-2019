object DataModule1: TDataModule1
  OldCreateOrder = False
  Height = 677
  Width = 810
  object RESTClient1: TRESTClient
    BaseURL = 'https://maps.googleapis.com/maps/api/staticmap'
    Params = <
      item
        Kind = pkQUERY
        Name = 'key'
        Value = 'AIzaSyDxZv3XIiP6Yb8rSfxOfhWlgyETb0AZ15g'
      end
      item
        Kind = pkQUERY
        Name = 'size'
        Value = '500x400'
      end
      item
        Name = 'format'
        Value = 'jpg'
      end
      item
        Name = 'scale'
        Value = '2'
      end>
    Left = 80
    Top = 32
  end
  object RESTRequest1: TRESTRequest
    Client = RESTClient1
    Params = <>
    Response = RESTResponse1
    SynchronizedEvents = False
    Left = 80
    Top = 104
  end
  object RESTResponse1: TRESTResponse
    Left = 80
    Top = 184
  end
  object RESTRequest2: TRESTRequest
    Client = RESTClient1
    Params = <
      item
        Name = 'center'
        Value = '-23.5193955,-46.7018535'
      end
      item
        Name = 'zoom'
        Value = '16'
      end>
    Response = RESTResponse2
    SynchronizedEvents = False
    Left = 232
    Top = 104
  end
  object RESTResponse2: TRESTResponse
    Left = 232
    Top = 184
  end
end
