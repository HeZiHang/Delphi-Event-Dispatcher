![alt text]

# DED an interfaced publish/subscribe framework for Delphi
Delphi Event Dispatcher (for short DED) is a publish/subscribe and multicast Event framework for Cross platforms.


## Features
* __Easy :__ DelphiEventDispatch is easy to learn and use. it can override an interface of Events to be a subscribers, and publisher only call procedures of an virtual Event interface to invoke all subscribers.
* __Decouple : based interface, decouple different parts/layers of your application.
* __Event Driven__
* __Support different delivery mode:__ Specifying the TThreadMode in Subscribe attribute, you can choose to delivery the event in the Main Thread or in a Background ones.
* __Thread Safe__

## Show me the code

### Events

1.Define Interface events:

```delphi
{$M+}
IDataEvent = inteface(IInterface)
['{3522E1C5-547F-4AB6-A799-5B3D3574D2FA}']
// additional information here
// procedure OnDataReceived(Sender:IDataSender; const DataBuffer:Pointer; const DataBufferSize:Integer);
// procdure OnOpen(Sender:IDataSender);
// procedure OnClose(Sender:IDataSender);
end;
...
{$M-}

```
2.Prepare publisher
```
type
  TDataModule1=class(TDataModule, IInterfaceObservable<IDataEvent>)
	...
	FDataSubscribers:IInterfaceObservable<IDataEven>;
	...
  published
    ...
    DataSubscribers:IInterfaceObservable<IDataEven> read FDataSubscribers;
	...
  end;

procedure TDataModule1.DataModule1Create(Sender:TObject);
begin
  ...
  FDataSubscribers:=TDioInterfaceDispatcher<IDataEvent>.Create;
  ...
end;
``` 
3.Prepare subscribers:

 * Declare your subscribing methods:
```delphi
{Subscribe}
type
  TForm1=class(TForm, IDataEvent)
    procedure OnDataReceived(Sender:IDataSender; const DataBuffer:Pointer; const DataBufferSize:Integer);
    procdure OnOpen(Sender:IDataSender);
	procedure OnClose(Sender:IDataSender);
	...
  end;
...
procedure TForm1.OnDataReceived(Sender:IDataSender; const DataBuffer:Pointer; const DataBufferSize:Integer);
begin
	//process data
end;

procedure TForm1.OnOpen(Sender:IDataSender);
begin
end;

procedure TForm1.OnClose(Sender:IDataSender);
begin
end;

```
 * Register your subscriber:
```delphi
DataModule1.DataSubscribers.AddObserver(Form1);
```

3.Post events:
```delphi
DataModule1.DataSubscribers.Source.OnDataReceived(aSender, aDataBuffer, aDataBufferSize);
or
DataModule1.DataSubscribers.Source.OnOpen(aSender);
or
DataModule1.DataSubscribers.Source.OnClose(aSender);

```


---

## Support
* DED is a 100% ObjectPascal framework so it works on VCL and Firemonkey
* It works with Delphi2010 and major

## Release Notes

2020-12-11 Initial Release.

## License
  Copyright 2016-2020 Hezihang

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
