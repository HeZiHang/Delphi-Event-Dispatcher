unit uFormMemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uPort;

type
  TFormMemo = class(TForm, IPortUser)
    Memo1: TMemo;
  private
    { Private declarations }
    FCount:Integer;
    procedure OnOpen(aPort:IPort);
    procedure OnClose(aPort:IPort);
    procedure OnReceive(aPort:IPort; L:String);
  public
    { Public declarations }
  end;

var
  FormMemo: TFormMemo;

implementation

{$R *.dfm}

{ TFormMemo }

procedure TFormMemo.OnClose(aPort: IPort);
begin
  Dec(FCount);
  if FCount=0 then
    Memo1.Enabled:=False;
end;

procedure TFormMemo.OnOpen(aPort: IPort);
begin
  if FCount=0 then
    Memo1.Clear;
  Inc(FCount);
  Memo1.Enabled:=True;
end;

procedure TFormMemo.OnReceive(aPort: IPort; L: String);
begin
  Memo1.Lines.Add(L);
end;

end.
