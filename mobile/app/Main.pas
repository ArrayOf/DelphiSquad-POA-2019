unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Maps, FMX.Controls.Presentation, System.Sensors, FMX.Objects, FMX.Edit,
  System.Sensors.Components, System.Permissions, FMX.Layouts,  FMX.DialogService,

  {$IFDEF ANDROID}
   Androidapi.JNI.GraphicsContentViewText, AndroidApi.Helpers, Androidapi.JNI.Telephony,Androidapi.JNI.Provider, Androidapi.JNIBridge,
   Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
   Androidapi.jni.App
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
    procedure RtlSearchClick(Sender: TObject);
    procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  oCoordinate: TMapCoordinate;

  oUserPin: TMapMarker;
  oBusStopPin: TMapMarker;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;

  //Zoom map
  MapView.Zoom := 11;


 PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        begin
          LocationSensor.Active := True;
        end
      else
        begin
          LocationSensor.Active := False;
          //TDialogService.ShowMessage('Location permission not granted');
          FooterLabel.Text = 'GPS não habilitado';
        end;
    end)
end;

procedure TMainForm.LocationSensorLocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  oCoordinate := TMapCoordinate.Create(NewLocation.Latitude, NewLocation.Longitude);
  MapView.Location := oCoordinate;

  //Pin
  if not Assigned(oUserPin) then
    oUserPin.Remove;

  oUserPin = MapView.AddMarker(TMapMarkerDescriptor.Create(oCoordinate, 'Sua Localização'));
  FooterLabel.Text := Format('Latitude: %s | Longitude: %s', [oCoordinate.Latitude.ToString, oCoordinate.Longitude.ToString]);
end;

//Envia o codigo da linha para pesquisa
procedure TMainForm.RtlSearchClick(Sender: TObject);
begin

  if not EdtBus.Text.IsEmpty then
    begin

    end;

end;

end.
