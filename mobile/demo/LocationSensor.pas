unit LocationSensor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Sensors,
  System.Sensors.Components,
  FMX.Maps,
  System.Permissions,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  FMX.DialogService,
  {$IFDEF ANDROID}
   Androidapi.JNI.GraphicsContentViewText, AndroidApi.Helpers, Androidapi.JNI.Telephony,Androidapi.JNI.Provider, Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.jni.App
  {$ENDIF}
  ;

type
  TFormLocationSensor = class(TForm)
    MapView: TMapView;
    LocationSensor: TLocationSensor;
    Layout1: TLayout;
    Switch1: TSwitch;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure Switch1Switch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLocationSensor: TFormLocationSensor;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TFormLocationSensor.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;

 //Zoom map
  MapView.Zoom := 15;
end;

procedure TFormLocationSensor.LocationSensorLocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
    coordinate : TMapCoordinate;
begin

  Label2.Text := Format('Latitude: %s | Longitude: %s', [NewLocation.Latitude.ToString, NewLocation.Longitude.ToString]);

  //Center
  coordinate := TMapCoordinate.Create(NewLocation.Latitude, NewLocation.Longitude);
  MapView.Location := coordinate;

  //Pin
  MapView.AddMarker(TMapMarkerDescriptor.Create(coordinate, 'Eu'));

end;

procedure TFormLocationSensor.Switch1Switch(Sender: TObject);
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
            Label1.Text := 'GPS Ligado';
            Label2.Text := 'Aguardando localizacao';
          end
        else
          begin
            Switch1.IsChecked := False;
            TDialogService.ShowMessage('Location permission not granted');
          end;
      end)
    end
   else
     Label1.Text := 'Ligue a localização';

end;

end.
