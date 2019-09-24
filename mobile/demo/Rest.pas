unit Rest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Sensors,
  System.Sensors.Components,
  FMX.Maps,
  System.Permissions,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.DialogService,
  REST.Types,
  REST.Client,
  Web.HTTPApp,
  System.JSON, Data.Bind.Components, Data.Bind.ObjectScope,
  {$IFDEF ANDROID}
    Androidapi.JNI.GraphicsContentViewText, AndroidApi.Helpers, Androidapi.JNI.Telephony,Androidapi.JNI.Provider, Androidapi.JNIBridge,
    Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
    Androidapi.jni.App
  {$ENDIF}
  ;

type
  TFormRest = class(TForm)
    MapView: TMapView;
    LocationSensor: TLocationSensor;
    Layout1: TLayout;
    Label2: TLabel;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Switch1: TSwitch;
    procedure FormCreate(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure Switch1Switch(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRest: TFormRest;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TFormRest.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;
  LocationSensor.Active := True;

 //Zoom map
  MapView.Zoom := 15;
end;

procedure TFormRest.LocationSensorLocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
    coordinate : TMapCoordinate;
    oBody: TJSONObject;
    //result : TJSONObject;
begin
  Label2.Text := Format('Latitude: %s | Longitude: %s', [NewLocation.Latitude.ToString, NewLocation.Longitude.ToString]);

  //Center
  coordinate := TMapCoordinate.Create(NewLocation.Latitude, NewLocation.Longitude);
  MapView.Location := coordinate;

  //Pin
  MapView.AddMarker(TMapMarkerDescriptor.Create(coordinate, 'Eu'));

  oBody := nil;
  try
    RESTRequest1.Method := TRESTRequestMethod.rmPOST;

    oBody := TJSONObject.Create;
    oBody.AddPair('lat', NewLocation.Latitude.ToString);
    oBody.AddPair('lng', NewLocation.Longitude.ToString);

    if RadioButton1.IsChecked then
      oBody.AddPair('person', 'big_phone')
    else if RadioButton2.IsChecked then
      oBody.AddPair('person', 'user');

    RESTRequest1.AddBody(oBody.ToString);

    RESTRequest1.Execute;
    //result := RESTRequest1.Response.JSONValue as TJSONObject;
  finally
    oBody.Free;
  end;

end;

procedure TFormRest.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.IsChecked then
    begin
      RadioButton2.IsChecked := False;
    end;
end;

procedure TFormRest.RadioButton2Change(Sender: TObject);
begin
  if RadioButton2.IsChecked then
    begin
      RadioButton1.IsChecked := False;
    end;
end;

procedure TFormRest.Switch1Switch(Sender: TObject);
begin
  LocationSensor.Active := Switch1.IsChecked;
   if Switch1.IsChecked then
    begin
       PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
      procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
      begin
        if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
            LocationSensor.Active := True
        else
          begin
            Switch1.IsChecked := False;
            TDialogService.ShowMessage('Location permission not granted');
          end;
      end)
    end
end;

end.
