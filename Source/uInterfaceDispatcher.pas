{*******************************************************************************
  Copyright 2015-2021 HeZiHang

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ********************************************************************************}

/// <summary>
///   This unit provides public interfaces and classes for the Delphi Event Dispatcher (DED) framework.
/// </summary>
unit uInterfaceDispatcher;

interface

uses System.Generics.Collections, System.TypInfo, System.Rtti, System.SyncObjs;

type
  IInterfaceObservable < T: IInterface >= interface
    procedure AddObserver(const aListener: T; ListenerDescription:String='');
    procedure AddObservers(const aListener: TArray<T>; ListenerDescription:TArray<String>);
    function GetObservers:TArray<T>;
    function GetDescriptions:TArray<String>;
    procedure RemoveObserver(const aListener: T);
    function IndexOf(const aListener:T):Integer;
    procedure RemoveAllObserver;
    procedure Lock;
    procedure UnLock;
    function GetSource: T;
    function GetCount:Integer;
    property Source: T read GetSource;
    property Count:Integer read GetCount;
  end;

  TDioInterfaceDispatcher<T: IInterface> = class(TInterfacedObject, IInterfaceObservable<T>)
  protected
    class var LType: TRttiType;
    class var FContext: TRttiContext;
    class var FTypeInfo: PTypeInfo;
    class var FMethods: TArray<TRttiMethod>;
    class var FIID: TGUID;
    class constructor Create;
    class destructor Destroy;
  private
    procedure DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
  protected
    FCallInUIThread:Boolean;
    FList: TList<T>;
    FDescriptions:TList<String>;
    FVirtualSource, FSource: T;
    FVirtualInterface: TVirtualInterface;
    FEvents: TObjectList<TList<TMethod>>;
    FLocker:TCriticalSection;
    procedure MethodInvoke(Method: TRttiMethod; const Args: TArray<TValue>;
      out Result: TValue);
  public
    procedure AddObserver(const aListener: T; ListenerDescription:String='');
    procedure AddObservers(const aListener: TArray<T>; ListenerDescription:TArray<String>);
    function GetObservers:TArray<T>;
    function GetDescriptions:TArray<String>;
    function GetCount:Integer;
    procedure RemoveAllObserver;
    function IndexOf(const aListener:T):Integer;
    procedure RemoveObserver(const aListener: T);
    procedure Lock;
    procedure UnLock;
    function GetSource: T;
    constructor Create(CallInUIThread:Boolean=True);
    destructor Destroy;override;
    property Source: T read FSource;
    property Count:Integer read GetCount;
  end;

implementation

uses System.SysUtils, System.Classes;

{ TDioInterfaceDispatcher<T> }

procedure TDioInterfaceDispatcher<T>.AddObserver(const aListener: T; ListenerDescription:String='');

type
  TVtable = array [0 .. 3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
var
  i: Integer;
  M: TMethod;
  P: Pointer;
begin
  Lock;
  if FList.IndexOf(aListener)<0 then
  begin
    FList.Add(aListener);
    FDescriptions.Add(ListenerDescription);
    P := IInterface(aListener);
    // P := IInterfaceGetObject(aListener).GetObject;
    for i := 0 to FEvents.Count - 1 do
    begin
      // 3 is offset of Invoke, after QI, AddRef, Release
      M.Code := PPVtable(P)^^[3 + i];
      M.Data := P;
      FEvents[i].Add(M);
    end;
    // if FList.Count=1 then
    // FSource:=aListener
    // else
    // FSource:=FVirtualSource;
  end;
  UnLock;
end;

class destructor TDioInterfaceDispatcher<T>.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FMethods) do
    FMethods[i].DisposeOf;
  SetLength(FMethods, 0);
  LType.DisposeOf;
  FContext.Free;
end;

procedure TDioInterfaceDispatcher<T>.DoInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  L: TList<TMethod>;
  i: Integer;
  M: TMethod;
begin
  Lock;
  L := FEvents[Method.VirtualIndex - 3];
  i := 0;
  while i < L.Count do
  begin
    M := L[i];

    Args[0] := M.Data;
    if Assigned(M.Code) then
      System.Rtti.Invoke(M.Code, Args, Method.CallingConvention, nil);
    if (i < L.Count) and (M = L[i]) then
      Inc(i);
  end;
  UnLock;
end;

procedure TDioInterfaceDispatcher<T>.MethodInvoke(Method: TRttiMethod;
  const Args: TArray<TValue>; out Result: TValue);
var
  R:TValue;
begin
  if FCallInUIThread then
     TThread.Synchronize(nil, procedure
     begin
       DoInvoke(Method, Args, R)
     end)
  else
    DoInvoke(Method, Args, R);
  Result:=R;
end;

procedure TDioInterfaceDispatcher<T>.AddObservers(const aListener: TArray<T>;
  ListenerDescription: TArray<String>);
var
  i: Integer;
begin
  Lock;
  for i := 0 to High(aListener) do
    Self.AddObserver(aListener[i], ListenerDescription[i]);
  UnLock;
end;

constructor TDioInterfaceDispatcher<T>.Create(CallInUIThread:Boolean=True);
var
  i: Integer;
  LMethod: TRttiMethod;
  E: TList<TMethod>;
  S: String;
begin
  inherited Create;
  FCallInUIThread:=CallInUIThread;
  FLocker:=TCriticalSection.Create;
  FEvents := TObjectList < TList < TMethod >>.Create(True);
  FList := TList<T>.Create;
  FDescriptions:=TList<String>.Create;

  FVirtualInterface := TVirtualInterface.Create(FTypeInfo);
  FVirtualInterface.OnInvoke := Self.MethodInvoke;
  FVirtualInterface.QueryInterface(FIID, FVirtualSource);
//  Assert(Assigned(FVirtualSource), '未找到接口' + GUIDToString(FIID));
  Assert(Assigned(FVirtualSource), 'Interface not found:' + GUIDToString(FIID));
  FSource := FVirtualSource;
  for i := 0 to High(FMethods) do
  begin
    E := TList<TMethod>.Create; // TEvent.Create(LMethod, FTypeInfo, i);
    FEvents.Add(E);
  end;
end;

class constructor TDioInterfaceDispatcher<T>.Create;
begin
  FTypeInfo := TypeInfo(T);
  LType := FContext.GetType(FTypeInfo);
  FIID := TRttiInterfaceType(LType).GUID;
  FMethods := LType.GetMethods();
  //Assert(Length(FMethods)>0, '接口'+ LType.Name+'未找到函数定义，请在其定义中加入{$M+}编译开关');
  Assert(Length(FMethods)>0, 'No procedures found in '+ LType.Name+', Please add compile direcive{$M+} and{$M-} to its declaration.');
  // Assert(Length(FMethods) <= 30, '只能分发30个以内函数的接口！');
end;

destructor TDioInterfaceDispatcher<T>.Destroy;
var
  i: Integer;
begin
  FSource := nil;
  FVirtualSource := nil;
  FVirtualInterface := nil;
  FDescriptions.DisposeOf;
  FList.DisposeOf;
  FEvents.DisposeOf;
  FLocker.DisposeOf;
  inherited;
end;

function TDioInterfaceDispatcher<T>.GetCount: Integer;
begin
  Lock;
  Result:=FList.Count;
  UnLock;
end;

function TDioInterfaceDispatcher<T>.GetDescriptions: TArray<String>;
begin
  Lock;
  Result:=FDescriptions.ToArray;
  UnLock;
end;

function TDioInterfaceDispatcher<T>.GetObservers: TArray<T>;
begin
  Lock;
  Result:=FList.ToArray;
  UnLock;
end;

function TDioInterfaceDispatcher<T>.GetSource: T;
begin
  Result := FSource;
end;

function TDioInterfaceDispatcher<T>.IndexOf(const aListener: T): Integer;
begin
  Lock;
  Result:=FList.IndexOf(aListener);
  UnLock;
end;

procedure TDioInterfaceDispatcher<T>.Lock;
begin
  FLocker.Enter;
end;

procedure TDioInterfaceDispatcher<T>.RemoveAllObserver;
var
  i: Integer;
begin
  Lock;
  FList.Clear;
  for i := 0 to FEvents.Count - 1 do
    FEvents[i].Clear;
  FDescriptions.Clear;
  UnLock;
end;

procedure TDioInterfaceDispatcher<T>.RemoveObserver(const aListener: T);
var
  N, i: Integer;
begin
  Lock;
  N := FList.IndexOf(aListener);
  if N >= 0 then
  begin
    for i := 0 to FEvents.Count - 1 do
      FEvents[i].Delete(N);
    FDescriptions.Delete(N);
  end;
  FList.Remove(aListener);
  UnLock;
end;

procedure TDioInterfaceDispatcher<T>.UnLock;
begin
  FLocker.leave;
end;

end.
