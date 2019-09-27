unit Rules;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.Types,
  System.JSON,
  Data.DbxHTTPLayer,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  REST.Types,
  REST.Client,
  IPPeerServer,
  Datasnap.DSCommonServer,
  Datasnap.DSHTTP,
  Datasnap.DSServer,

  LoadEPTCData,

  Redis.Client,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type

{$METHODINFO ON}
  TStopBus = class(TComponent)
  public
    function HelloWorld: string;
    function BusHunter(const ALine: string; const ALatitude: string; const ALongitude: string): TJSONObject;
  end;
{$METHODINFO OFF}

  TDataModule1 = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTRequest2: TRESTRequest;
    RESTResponse2: TRESTResponse;
    DSServer1: TDSServer;
    DSHTTPService1: TDSHTTPService;
    DSServerClass1: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
  private const
    FDelphiSquadLat = -30.0428357;
    FDelphiSquadLng = -51.2188929;
  private
    FRedisHost    : string;
    FRedisPort    : Integer;
    FRedisHostPort: string;
    function GetGoogleAPIKey: string;
    procedure SetGoogleAPIKey(const Value: string);
    procedure SetRedisHostPort(const Value: string);
  public
    function TestRedis: Boolean;
    function GetMap: TStringStream;
    function WhereAreWe: TStringStream;
    function LoadEPTC(const APath: string; ALog: TProcessLine): ITask;
    function GetNearbyBusStop(const ALine: string): TStringStream;
    procedure ActiveServer;
    property GoogleAPIKey: string read GetGoogleAPIKey write SetGoogleAPIKey;
    property RedisHostPort: string read FRedisHostPort write SetRedisHostPort;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  Winapi.Windows,
  Main,
  QueryEPTC;

{$R *.dfm}
{ TDataModule1 }

procedure TDataModule1.ActiveServer;
begin
  Self.DSServer1.Start;
end;

procedure TDataModule1.DSServerClass1GetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := TStopBus;
end;

function TDataModule1.GetGoogleAPIKey: string;
begin
  Result := Self.RESTClient1.Params.ParameterByName('key').Value;
end;

function TDataModule1.GetMap: TStringStream;
begin
  Self.RESTRequest1.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse1.RawBytes, Self.RESTResponse1.ContentLength);
  Result.Seek(0, 0);
end;

function TDataModule1.GetNearbyBusStop(const ALine: string): TStringStream;
var
  oQuery      : TQueryEPTC;
  aCoordinates: TArray<string>;
  sCoordinates: string;
begin
  oQuery := TQueryEPTC.Create(Self.FRedisHost, Self.FRedisPort);
  try
    aCoordinates := oQuery.NearbyBusStop(ALine, Self.FDelphiSquadLat, Self.FDelphiSquadLng);
    sCoordinates := Format('color:green|%s,%s;color:red|%g,%g', [aCoordinates[0], aCoordinates[1], Self.FDelphiSquadLat, Self.FDelphiSquadLng]);

    Self.RESTRequest2.Params.ParameterByName('markers').Value := sCoordinates;

    OutputDebugString(PChar(Self.RESTRequest2.GetFullRequestURL()));

    Self.RESTRequest2.Execute;

    Result := TStringStream.Create;
    Result.WriteData(Self.RESTResponse2.RawBytes, Self.RESTResponse2.ContentLength);
    Result.Seek(0, 0);
  finally
    oQuery.Free;
  end;
end;

function TDataModule1.LoadEPTC(const APath: string; ALog: TProcessLine): ITask;
var
  maEPTCLoad: TProc;
begin
  maEPTCLoad := procedure
    var
      oLoader: TLoadEPTC;
    begin
      oLoader := TLoadEPTC.Create(APath, ALog);
      try
        oLoader.Work(Self.FRedisHost, Self.FRedisPort);
      finally
        oLoader.Free;
      end;
    end;

  Result := TTask.Run(maEPTCLoad);
end;

procedure TDataModule1.SetGoogleAPIKey(const Value: string);
begin
  Self.RESTClient1.Params.ParameterByName('key').Value := Value;
end;

procedure TDataModule1.SetRedisHostPort(const Value: string);
var
  slParts: TStringList;
  sHost  : string;
  sPort  : string;
begin
  slParts := TStringList.Create;
  try
    slParts.Delimiter       := ':';
    slParts.StrictDelimiter := True;
    slParts.DelimitedText   := Value;

    sHost := slParts[0];
    if slParts.Count > 1 then
    begin
      sPort := slParts[1];
    end else begin
      sPort := '6379';
    end;
  finally
    slParts.Free;
  end;

  Self.FRedisHostPort := Value;
  Self.FRedisHost     := sHost;
  Self.FRedisPort     := StrToInt(sPort);
end;

function TDataModule1.TestRedis: Boolean;
var
  oRedis: IRedisClient;
begin
  try
    oRedis := TRedisClient.Create(Self.FRedisHost, Self.FRedisPort);
    oRedis.Connect;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

function TDataModule1.WhereAreWe: TStringStream;
var
  sCoordinates: string;
begin
  sCoordinates := Format('%g,%g', [Self.FDelphiSquadLat, Self.FDelphiSquadLng]);

  Self.RESTRequest2.Params.ParameterByName('markers').Value := Format('color:red|%s', [sCoordinates]);

  Self.RESTRequest2.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse2.RawBytes, Self.RESTResponse2.ContentLength);
  Result.Seek(0, 0);
end;

{ TStopBus }

function TStopBus.BusHunter(const ALine: string; const ALatitude, ALongitude: string): TJSONObject;
var
  oQuery : TQueryEPTC;
  aBuffer: TArray<string>;
begin
  oQuery := TQueryEPTC.Create(DataModule1.FRedisHost, DataModule1.FRedisPort);
  try
    aBuffer := oQuery.NearbyBusStop(ALine, StrToFloat(ALatitude), StrToFloat(ALongitude));

    Result := TJSONObject.Create;
    Result.AddPair('lat', aBuffer[0]);
    Result.AddPair('lng', aBuffer[1]);
  finally
    oQuery.Free;
  end;
end;

function TStopBus.HelloWorld: string;
begin
  Result := 'Olá mundo!';
end;

end.
