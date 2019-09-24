unit RestTimer;

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
  TFormRestTimer = class(TForm)
    MapView: TMapView;
    LocationSensor: TLocationSensor;
    Layout1: TLayout;
    Label2: TLabel;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Switch1: TSwitch;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure Switch1Switch(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRestTimer: TFormRestTimer;
  oCoordinate: TMapMarker;
  oPins: array of TMapMarker;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TFormRestTimer.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;
  LocationSensor.Active := True;

 //Zoom map
  MapView.Zoom := 15;
end;

procedure TFormRestTimer.LocationSensorLocationChanged(Sender: TObject;
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
  if oCoordinate is not Null then
    oCoordinate.Remove;

  oCoordinate := MapView.AddMarker(TMapMarkerDescriptor.Create(coordinate, 'Eu'));

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
  finally
    oBody.Free;
  end;

end;

procedure TFormRestTimer.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.IsChecked then
    begin
      RadioButton2.IsChecked := False;
    end;
end;

procedure TFormRestTimer.RadioButton2Change(Sender: TObject);
begin
  if RadioButton2.IsChecked then
    begin
      RadioButton1.IsChecked := False;
    end;
end;

procedure TFormRestTimer.Switch1Switch(Sender: TObject);
begin
  LocationSensor.Active := Switch1.IsChecked;
   if Switch1.IsChecked then
    begin
       PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
      procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
      begin
        if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
            begin
              LocationSensor.Active := True;
              Timer1.Enabled := True;
            end
        else
          begin
            Switch1.IsChecked := False;
            TDialogService.ShowMessage('Location permission not granted');
          end;
      end)
    end
    else
      Timer1.Enabled := False;
end;

procedure TFormRestTimer.Timer1Timer(Sender: TObject);
var
    oBody: TJSONObject;
    response: TJSONObject;
    coordinate : TMapCoordinate;

    I: integer;
    phones: TJSONArray;

begin
  oBody := nil;
  try
    RESTRequest1.Method := TRESTRequestMethod.rmGET;

    RESTRequest1.Resource := 'lat={lat}&lng={lng}&radius={radius}&place={place}';
    RESTRequest1.Params.AddUrlSegment('lat', oCoordinate.Latitude.ToString);
    RESTRequest1.Params.AddUrlSegment('lng', oCoordinate.Longitude.ToString);
    RESTRequest1.Params.AddUrlSegment('radius', 6);

    if RadioButton1.IsChecked then
      RESTRequest1.Params.AddUrlSegment('place', 'user')
    else
      RESTRequest1.Params.AddUrlSegment('place', 'big_phone');

    RESTRequest1.Execute;
    response := RESTRequest1.Response.JSONValue as TJSONObject;

    //Clear
    for I := 0 to High(oPins) do
      oPins[I].Remove;

    setLength(oPins, 0);

    //Adiciona novamente
    phones := response.Get('listing').JsonValue as TJSONArray;
    setLength(oPins, length(phones));

    for I := 0 to length(phones) do
    begin
      coordinate := TMapCoordinate.Create(0, 0);
      oPins[I] := MapView.AddMarker(TMapMarkerDescriptor.Create(coordinate, 'Eu'));
    end;

  finally
    oBody.Free;
  end;
end;

end.
