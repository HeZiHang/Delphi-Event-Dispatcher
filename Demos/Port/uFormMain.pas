unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uPort, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FPort:IPort;

  end;

var
  FormMain: TFormMain;

implementation

uses uFormMemo, uFormStatus;
{$R *.dfm}

procedure TFormMain.Button1Click(Sender: TObject);
begin
  FPort.Start;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  FPort.Stop
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Memo:TFormMemo;
  Status:TFormStatus;
begin
  FPort:=TPort.Create;
  Memo:=TFormMemo.Create(Self);
  FPort.Users.AddObserver(Memo);
  Memo.Align:=alClient;
  Memo.BorderStyle:=bsNone;
  Memo.Parent:=Self;
  Memo.Show;

  Status:=TFormStatus.Create(Self);
  FPort.Users.AddObserver(Status);
  Status.Align:=alBottom;
  Status.BorderStyle:=bsNone;
  Status.Parent:=Self;
  Status.Show;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FPort.Users.RemoveAllObserver;
  FPort:=nil;
end;

end.
