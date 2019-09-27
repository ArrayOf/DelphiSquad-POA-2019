unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Threading,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ImageMapa: TImage;
    Memo1: TMemo;
    Timer1: TTimer;
    Button5: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Button3: TButton;
    Button4: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure LabeledEdit2Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    FLoadTask: ITask;
    procedure ShowMap(AContent: TStringStream);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Rules,
  Vcl.Imaging.jpeg,
  LoadEPTCData;

procedure TForm1.Button1Click(Sender: TObject);
var
  oResponse: TStringStream;

begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;

  try
    oResponse := DataModule1.GetMap;
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  oResponse: TStringStream;
begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;

  try
    oResponse := DataModule1.WhereAreWe;
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  bRet: Boolean;
begin
  Screen.Cursor := crHourGlass;

  bRet := DataModule1.TestRedis;
  if bRet then
  begin
    MessageBox(Self.Handle, 'O Redis está acessível!', 'Atenção!', MB_ICONINFORMATION + MB_OK);
  end else begin
    MessageBox(Self.Handle, 'O Redis *NÃO* está acessível!', 'Atenção!', MB_ICONERROR + MB_OK);
  end;

  Screen.Cursor := crDefault;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  DataModule1.ActiveServer;
  Self.Button4.Enabled := False;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  sDirName: string;
  maLog   : TProcessLine;
begin
  sDirName := 'E:\ArrayOf_DelphiSquad-POA-2019\dataset';

  maLog := procedure(const ALine: string)
    begin
      Self.Memo1.Lines.Add(ALine);
    end;

  Self.FLoadTask := DataModule1.LoadEPTC(sDirName, maLog);

  Self.Button5.Enabled := False;
  Self.Timer1.Enabled  := True;

  Application.ProcessMessages;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  oResponse: TStringStream;
begin
  Screen.Cursor := crHourGlass;
  oResponse     := nil;
  try
    oResponse := DataModule1.GetNearbyBusStop('244-1');
    Self.ShowMap(oResponse);
  finally
    oResponse.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown     := True;
  FormatSettings.DecimalSeparator := '.';

  Self.FLoadTask            := nil;
  DataModule1.GoogleAPIKey  := Self.LabeledEdit1.Text;
  DataModule1.RedisHostPort := Self.LabeledEdit2.Text;

  Self.PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.LabeledEdit1Change(Sender: TObject);
begin
  DataModule1.GoogleAPIKey := Self.LabeledEdit1.Text;
end;

procedure TForm1.LabeledEdit2Change(Sender: TObject);
begin
  DataModule1.RedisHostPort := Self.LabeledEdit2.Text;
end;

procedure TForm1.ShowMap(AContent: TStringStream);
var
  oJPG: TJPEGImage;
  oBMP: TBitmap;
begin
  oJPG := TJPEGImage.Create;
  oBMP := TBitmap.Create;

  oJPG.LoadFromStream(AContent);
  oBMP.Assign(oJPG);

  Self.ImageMapa.Picture.Bitmap.Assign(oBMP);

  oJPG.Free;
  oBMP.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Self.Timer1.Enabled := False;

  if Assigned(Self.FLoadTask) then
  begin
    case Self.FLoadTask.Status of
      TTaskStatus.Created:
        ;
      TTaskStatus.WaitingToRun:
        ;
      TTaskStatus.Running:
        begin
          Self.Memo1.Lines.Add('Tarefa em andamento!');
        end;
      TTaskStatus.Completed:
        begin
          Self.Memo1.Lines.Add('Tarefa completa!');
          Self.FLoadTask := nil;
        end;
      TTaskStatus.WaitingForChildren:
        ;
      TTaskStatus.Canceled:
        ;
      TTaskStatus.Exception:
        begin
          Self.Memo1.Lines.Add('Deu exception!');
          Self.FLoadTask := nil;
        end;
    end;

    if Assigned(Self.FLoadTask) then
    begin
      Self.Timer1.Enabled := True;
    end else begin
      Self.Button5.Enabled := True;
      Application.ProcessMessages;
    end;
  end;

end;

end.
