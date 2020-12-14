unit uPort;

interface

uses uInterfaceDispatcher, System.Classes, System.SyncObjs;
type
  IPort=interface;
{$M+}
  IPortUser=interface
    ['{9F1AFD7E-F483-430D-B227-BAEC4070E792}']
    procedure OnOpen(aPort:IPort);
    procedure OnClose(aPort:IPort);
    procedure OnReceive(aPort:IPort; L:String);
  end;
{$M-}

  IPort=interface
    ['{569B25F1-2FF1-4507-8050-62B2F030866B}']
    procedure Start;
    procedure Stop;
    function GetUsers: IInterfaceObservable<IPortUser>;
    property Users:IInterfaceObservable<IPortUser> read GetUsers;
  end;

  TPort=class(TInterfacedObject, IPort)
  private
    function GetUsers: IInterfaceObservable<IPortUser>;
  protected
    FUsers:IInterfaceObservable<IPortUser>;
    FThreadCount:Int64;
    FThreadTerminated:Boolean;
  public
    procedure Start;
    procedure Stop;
    constructor Create;
  end;
implementation

{ TPort }

constructor TPort.Create;
begin
  inherited Create;
  FUsers:=TDioInterfaceDispatcher<IPortUser>.Create;
end;

function TPort.GetUsers: IInterfaceObservable<IPortUser>;
begin
  Result:=FUsers;
end;

procedure TPort.Start;
begin
  FThreadTerminated:=False;
  TInterLocked.Increment(FThreadCount);
  TThread.CreateAnonymousThread(
    procedure
    var
      i:Integer;
      S:String;
    begin
      FUsers.Source.OnOpen(Self);
      while not FThreadTerminated do
      begin
        S:='';
        for i := 10 to Random(100)+10 do
          S:=S+Char(Random(26)+Ord('A'));
        FUsers.Source.OnReceive(Self, S);

        for i := 0 to 200 do
          if FThreadTerminated then break
            else TThread.Sleep(1);
      end;
      FUsers.Source.OnClose(Self);
      TInterLocked.Decrement(FThreadCount);
    end).Start;

end;

procedure TPort.Stop;
begin
  FThreadTerminated:=True;
  while TInterLocked.Read(FThreadCount)<>0 do CheckSynchronize; //Wait for All Thread terminated
end;

end.
