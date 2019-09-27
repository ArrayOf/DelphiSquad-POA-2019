unit QueryEPTC;

interface

uses
  System.Types,

  Redis.Client,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

  TQueryEPTC = class(TObject)
  private
    FRedis: IRedisClient;
  public
    function NearbyBusStop(const ACodeLine: string; const ACoordinates: TPoint): TPoint;
  end;

implementation

{ TQueryEPTC }

function TQueryEPTC.NearbyBusStop(const ACodeLine: string; const ACoordinates: TPoint): TPoint;
begin

end;

end.
