unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Maps, FMX.Controls.Presentation, System.Sensors, FMX.Objects, FMX.Edit,
  System.Sensors.Components, System.Permissions, FMX.Layouts, FMX.DialogService,
  REST.Types,
  REST.Client,
  Web.HTTPApp,
  System.JSON,
  System.Threading,

{$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  Androidapi.JNI.Telephony, Androidapi.JNI.Provider, Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.App, Data.Bind.Components, Data.Bind.ObjectScope,
  System.ImageList, FMX.ImgList
{$ENDIF};

type
  TMainForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    MapView: TMapView;
    FooterLabel: TLabel;
    LocationSensor: TLocationSensor;
    PlSearch: TPanel;
    EdtBus: TEdit;
    ImgSearch: TImage;
    RtlSearch: TRectangle;
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    Rectangle1: TRectangle;
    ImageList1: TImageList;
    procedure RtlSearchClick(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fCoordinate: TMapCoordinate;
    fUserPin: TMapMarker;
    // fBusStopPin: TMapMarker;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;

  PermissionsService.RequestPermissions
    ([JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
    procedure(const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and
        (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        LocationSensor.Active := True;
        FooterLabel.Text := 'Show!!!';
      end
      else
      begin
        LocationSensor.Active := False;
        FooterLabel.Text := 'GPS não habilitado';
      end;
    end)
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  LocationSensor.Active := False;
end;

procedure TMainForm.LocationSensorLocationChanged(Sender: TObject;
const OldLocation, NewLocation: TLocationCoord2D);
var
  oMarker: TMapMarkerDescriptor;
begin
  fCoordinate := TMapCoordinate.Create(NewLocation.Latitude,
    NewLocation.Longitude);
  MapView.Location := fCoordinate;

  // Pin
  if Assigned(fUserPin) then
    fUserPin.Remove;

  oMarker := TMapMarkerDescriptor.Create(fCoordinate, 'Sua Localização');
  oMarker.Icon := ImageList1.Source.Items[0].MultiResBitmap.Items[0].Bitmap;
  fUserPin := MapView.AddMarker(oMarker);
  FooterLabel.Text := Format('Latitude: %s | Longitude: %s',
    [fCoordinate.Latitude.ToString, fCoordinate.Longitude.ToString]);

  // Zoom map
  MapView.Zoom := 15;
end;

// Envia o codigo da linha para pesquisa
procedure TMainForm.RtlSearchClick(Sender: TObject);
var
  // oFooter: String;
  aTask: ITask;

  I: Integer;
  oResponse: TJSONObject;
  oPoints: TJSONArray;
  oPoint: TJSONObject;
  oMarker: TMapMarkerDescriptor;
  fLat: Double;
  fLng: Double;
begin

  if EdtBus.Text <> EmptyStr then
  begin
    FooterLabel.Text := 'Buscando o ponto de ônibus...';
    try
      RESTRequest.Method := TRESTRequestMethod.rmGET;
      RESTRequest.Params.ParameterByName('code_bus').Value := EdtBus.Text;
      RESTRequest.Params.ParameterByName('lat').Value :=
        fCoordinate.Latitude.ToString.Replace(',', '.');
      RESTRequest.Params.ParameterByName('lng').Value :=
        fCoordinate.Longitude.ToString.Replace(',', '.');

      RESTRequest.Execute;
      oResponse := RESTRequest.Response.JSONValue as TJSONObject;

      oPoints := oResponse.GetValue('result') as TJSONArray;
      oPoint := oPoints.Items[0] as TJSONObject;

      // Replace existe por conta do Locale
      fLat := StrToFloat(oPoint.GetValue('lat').Value.Replace('.', ','));
      fLng := StrToFloat(oPoint.GetValue('lng').Value.Replace('.', ','));

      oMarker := TMapMarkerDescriptor.Create(TMapCoordinate.Create(fLat,
        fLng), 'Busao');
      oMarker.Icon := ImageList1.Source.Items[1].MultiResBitmap.Items[0].Bitmap;
      MapView.AddMarker(oMarker);

      MapView.Zoom := 20;
      // end
    finally
      oResponse.Free;
      oPoints.Free;
      oPoint.Free;
    end
  end;

end;

end.
