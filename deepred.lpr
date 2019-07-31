program deepred;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, DaemonApp, inifiles,
  dr_server, pascalscriptfcl, dr_api, dr_script;

type
    TCustomDeepRedServer =
    class (TDeepRedServer)
          protected
                procedure WriteLog(evType : TEventType; const msg : string); override;
    end;


    TDeepRedDaemon =
    class (TCustomDaemon)
         private
               _server   : TCustomDeepRedServer;
               _log      : TFileStream;

               procedure AWriteln(evtype : TEventType; const msg : string);

         public
               //Daemon control
               function Start : boolean; override;
               function Stop : boolean; override;
               function Shutdown : boolean; override;
               function Install : boolean; override;
               function Execute : boolean; override;
    end;


    TDeepRedDaemonMapper =
    class (TCustomDaemonMapper)
          constructor Create(AOwner : TComponent); override;
    end;


//==============================================================================
// TDeepRedDaemonMapper
//==============================================================================
constructor TDeepRedDaemonMapper.Create(AOwner : TComponent);
var d : TDaemonDef;
begin
     inherited Create(AOwner);
     d:=DaemonDefs.Add as TDaemonDef;
     d.Options:=[doAllowStop];
     d.DisplayName:='DeepRed Server';
     d.Name:='DeepRedDaemon';
     d.DaemonClassName:='TDeepRedDaemon';
     d.WinBindings.ServiceType:=stWin32;
end;

//==============================================================================
// TDeepRedDaemon
//==============================================================================

function TDeepRedDaemon.Start : boolean;
var
  iniFile  : TIniFile;
  //bindings : TStringList;
  i        : cardinal;
  str      : string;
begin
     //****************************************************
     //Daemon configuration section
     //****************************************************
     //_log := TFileStream.Create('deepred.log',fmOpenWrite);

     if not FileExists(ChangeFileExt(Application.ExeName,'.conf')) then
     begin
          AWriteln(etError,'DeepRed Daemon : no configuration file found! you must have a '+
                   ChangeFileExt(ExtractFileName(Application.ExeName),'.conf')+
                   ' inside the daemon''s executable directory ...');
          exit(false);
     end;

     Result := inherited Start;
     if not Result then
     begin
          AWriteln(etError,'DeepRed Daemon : cannot instanciate daemon!');
          exit(false);
     end;
     AWriteln(etInfo,'DeepRed Daemon : starting ...');

     iniFile := TIniFile.Create(ChangeFileExt(Application.ExeName,'.conf'));
     (*
     _server := TDeepRedServer.Create(iniFile.ReadInteger('Repository','Port',61993),
                                    iniFile.ReadInteger('Repository','TerminateWaitTime',5000),
                                    iniFile.ReadInteger('Repository','MaxConnections',20),
                                    iniFile.ReadString('SQLite','Host','xswag.db'));
     if _server.Error <> '' then
          AWriteln(etError,'DeepRed Daemon : '+_server.Error);

     _server.SetMOTD(iniFile.ReadString('Repository','MOTD',''));
     _server.SetPathToArchives(iniFile.ReadString('Repository','PathToArchives','.'));
     _server.ClearBindings;*)

     //iniFile.ReadSectionRaw('Bindings',bindings);
     iniFile.Free;
     //****************************************************
     _server := TCustomDeepRedServer.Create('brookapi.cfg','');

     //_server.StartServer;
     AWriteln(etInfo,'DeepRed Daemon : started.');
end;


function TDeepRedDaemon.Stop : boolean;
var
   serverr: string;
begin
     try
        _server.StopServer;
        serverr := _server.Error;
        _server.Free;
        //_log.Free;

     except
        if serverr <> '' then
            AWriteln(etError,'DeepRed Daemon : Server reported the following error : '+serverr);
     end;
     AWriteln(etInfo,'DeepRed Daemon : stopped.');
    {$IFDEF WINDOWS}
    //    _log.Free;
    {$ELSE}
    {$ENDIF}
     result := inherited Stop;
end;


function TDeepRedDaemon.Shutdown : boolean;
begin
     AWriteln(etInfo,'DeepRed Daemon : shutdown required.');
     try
        //_server.Free;
        Result := inherited Shutdown;
     finally
        Application.Terminate;
     end;
end;


function TDeepRedDaemon.Install : boolean;
begin
     result := inherited Install;
     AWriteln(etInfo,'DeepRed Daemon : installed.');
end;


function TDeepRedDaemon.Execute : boolean;
begin
     result := inherited Execute;
     //AWriteln(etInfo,'DeepRed Daemon : running.');
end;


procedure TDeepRedDaemon.AWriteln(evType : TEventType; const msg : string);
var str : string;
begin
     case evType of
          etInfo    : str := '[INFO] '+msg;
          etWarning : str := '[WARN] '+msg;
          etError   : str := '[ERROR] '+msg;
          etDebug   : str := '[DEBUG] '+msg;
     end;
  (*
     _log.Write(str,length(str));
     _log.WriteByte(10);
     _log.WriteByte(13);      *)
     Application.Log(evType,msg);
end;

//==============================================================================
// TCustomDeepRedServer
//==============================================================================

procedure TCustomDeepRedServer.WriteLog(evType : TEventType; const msg : string);
var str : string;
begin
     case evType of
          etInfo    : str := '[INFO] [DeepRedServer] '+msg;
          etWarning : str := '[WARN] [DeepRedServer] '+msg;
          etError   : str := '[ERROR] [DeepRedServer] '+msg;
          etDebug   : str := '[DEBUG] [DeepRedServer] '+msg;
     end;
     Application.Log(evType,msg);
end;


//==============================================================================
//                              M  A  I  N
//==============================================================================

{$R *.res}

begin
     RegisterDaemonClass(TDeepRedDaemon);
     RegisterDaemonMapper(TDeepRedDaemonMapper);
     Application.Title:='Deep Red';
     Application.Run;
end.

