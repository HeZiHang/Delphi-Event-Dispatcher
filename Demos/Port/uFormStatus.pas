unit uFormStatus;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uPort;

type
  TFormStatus = class(TForm, IPortUser)
    Label1: TLabel;
  private
    { Private declarations }
    FCount:Integer;
    FLineCount:Integer;
    FCharCount:Integer;
    procedure RefreshStatus;
  public
    { Public declarations }
    procedure OnOpen(aPort:IPort);
    procedure OnClose(aPort:IPort);
    procedure OnReceive(aPort:IPort; L:String);
  end;

var
  FormStatus: TFormStatus;

implementation

{$R *.dfm}

{ TFormStatus }

procedure TFormStatus.OnClose(aPort: IPort);
begin
  Dec(FCount);
  RefreshStatus;
end;

procedure TFormStatus.OnOpen(aPort: IPort);
begin
  if FCount=0 then
  begin
    FLineCount:=0;
    FCharCount:=0;
  end;
  Inc(FCount);
  RefreshStatus;
end;

procedure TFormStatus.OnReceive(aPort: IPort; L: String);
begin
  Inc(FLineCount);
  Inc(FCharCount, Length(L));
  RefreshStatus
end;

procedure TFormStatus.RefreshStatus;
begin
  Label1.Caption:=Format('Port Open Count:%d, Line Count:%d, Char Count:%d', [FCount, FLineCount, FCharCount]);
end;

end.
