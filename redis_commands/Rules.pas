unit Rules;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  REST.Types,
  Data.DbxHTTPLayer,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  REST.Client,

  Redis.Client,
  Redis.Commons,
  Redis.NetLib.INDY

    ;

type
  TDataModule1 = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    RESTRequest2: TRESTRequest;
    RESTResponse2: TRESTResponse;
  private
    { Private declarations }
  public
    { Public declarations }
    function GetMap: TStringStream;
    function WhereAreWe: TStringStream;
    function LoadListTUP(const AFileName: string): IFuture<Integer>;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses Main;
{$R *.dfm}
{ TDataModule1 }

{ TDataModule1 }

function TDataModule1.GetMap: TStringStream;
begin
  Self.RESTRequest1.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse1.RawBytes, Self.RESTResponse1.ContentLength);
  Result.Seek(0, 0);
end;

function TDataModule1.LoadListTUP(const AFileName: string): IFuture<Integer>;
var
  maTUPLoad: TFunc<Integer>;
begin
  if not FileExists(AFileName) then
  begin
    raise Exception.CreateFmt('O arquivo [%s] n�o existe!', [AFileName]);
  end;

  maTUPLoad := function: Integer
    var
      hFileTUP   : TextFile;
      sLine      : string;
      iLinesCount: Integer;
      oRedis     : IRedisClient;
      slFields   : TStringList;
      eLat       : Extended;
      eLng       : Extended;
      sNumber    : string;
    begin
      slFields                 := TStringList.Create;
      slFields.StrictDelimiter := True;
      slFields.Delimiter       := ',';
      slFields.QuoteChar       := '"';
      iLinesCount              := 0;

      oRedis := TRedisClient.Create('localhost', 6379);
      oRedis.Connect;
      Form1.Memo1.Lines.Add('Conectado com o Redis!');

      oRedis.FLUSHDB;

      AssignFile(hFileTUP, AFileName);
      Reset(hFileTUP);
      try
        Readln(hFileTUP, sLine);
        while not Eof(hFileTUP) do
        begin
          Readln(hFileTUP, sLine);
          if (sLine = EmptyStr) then
          begin
            Break;
          end;

          Inc(iLinesCount);

          slFields.DelimitedText := sLine;

          eLat    := StrToFloat(slFields[19]);
          eLng    := StrToFloat(slFields[20]);
          sNumber := slFields[6];

          oRedis.GEOADD('DELPHISQUAD:2019:POA:ORELHAO#', eLat, eLng, sNumber);
        end;
      finally
        CloseFile(hFileTUP);
        slFields.Free;
      end;

      Result := iLinesCount;

    end;

  Result := TTask.Future<Integer>(maTUPLoad)
end;

function TDataModule1.WhereAreWe: TStringStream;
begin
  Self.RESTRequest2.Execute;

  Result := TStringStream.Create;
  Result.WriteData(Self.RESTResponse2.RawBytes, Self.RESTResponse2.ContentLength);
  Result.Seek(0, 0);
end;

end.
