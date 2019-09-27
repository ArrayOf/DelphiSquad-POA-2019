unit QueryEPTC;

interface

uses
  System.Types,

  Redis.Client,
  Redis.Values,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

  TQueryEPTC = class(TObject)
  private
    FRedis: IRedisClient;
  public
    constructor Create(const ARedisHost: string; const ARedisPort: Integer); reintroduce;
    function NearbyBusStop(const ACodeLine: string; const ALatitude: Extended; const ALongitude: Extended): TArray<string>;
  end;

implementation

uses
  System.SysUtils;

{ TQueryEPTC }

constructor TQueryEPTC.Create(const ARedisHost: string; const ARedisPort: Integer);
begin
  inherited Create;

  Self.FRedis := TRedisClient.Create(ARedisHost, ARedisPort);
  Self.FRedis.Connect;
end;

function TQueryEPTC.NearbyBusStop(const ACodeLine: string; const ALatitude: Extended; const ALongitude: Extended): TArray<string>;
var
  sKeyName    : string;
  aResult     : TRedisArray;
  aCoordinates: TRedisMatrix;
begin
  sKeyName := Format('DELPHISQUAD:2019:POA:LINHA:%s#', [ACodeLine]);
  aResult  := Self.FRedis.GEORADIUS(sKeyName, ALongitude, ALatitude, 10000000, TRedisGeoUnit.Meters, TRedisSorting.Asc, 1);

  if not aResult.IsNull then
  begin
    aCoordinates := Self.FRedis.GEOPOS(sKeyName, [aResult.Value[0].Value]);

    SetLength(Result, 2);
    Result[0] := aCoordinates.Value[0].Value[0].Value;
    Result[1] := aCoordinates.Value[0].Value[1].Value;
  end;
end;

end.
