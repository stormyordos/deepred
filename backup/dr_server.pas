unit dr_server;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dr_api,
    synapseServer, hexsqlite, hextools, md5, fileinfo
;

type
    TDeepRedServer =
    class (TComponent)
          private
                _brookServer       : THTTPServer;
                _scriptServer      : TPascalScriptHelper;
                _synapseServer     : TTCP_NetworkServer;
                _sqliteClient      : THexSQLiteClient;

                _running           : boolean;
                _errorStr          : string;
                _motd              : string;

                function DisplayError : PChar;
          protected
                procedure WriteLog(evType : TEventType; const msg : string); virtual; abstract;
          public
                constructor Create(brookfile : string; dbfile : string);
                destructor Destroy; override;

                procedure AddBinding(const address : ansistring);
                procedure ClearBindings;
                function StartServer : boolean;
                function StopServer : boolean;
                procedure OnNetworkEvent(const netState : TNetworkState);
                procedure OnError(ASender: TObject; AException: Exception);

                property Running : boolean read _running;
                property Error : PChar read DisplayError;
    end;


resourcestring
    rs_ACK           = 'ACK';
    rs_NAK           = 'NAK';
    rs_MOTD          = 'MOTD';
    rs_WHOAMI        = 'WHOAMI';
    rs_VERSION       = 'VERSION';
    rs_HELP          = 'HELP';
    rs_STATUS        = 'STATUS';
    rs_KTHXBAI       = 'KTHXBAI';

function MD5HashFromString(const inStr : string) : PChar;
function MD5HashFromFile(const inFile : string) : PChar;

implementation

//==============================================================================
// Miscellaneous
//==============================================================================

function MD5HashFromString(const inStr : string) : PChar;
begin
     Result := PChar(MD5Print(MD5String(inStr)));
end;


function MD5HashFromFile(const inFile : string) : PChar;
begin
     Result := PChar(MD5Print(MD5File(inFile)));
end;

//==============================================================================
// TDeepRedServer
//==============================================================================

function TDeepRedServer.DisplayError : PChar;
begin
     Result := PChar(_errorStr);
end;


procedure TDeepRedServer.OnNetworkEvent(const netState : TNetworkState);
var
   i,j,k   : cardinal;
   size    : cardinal;
   f       : TFileStream;
   p       : pointer;
   str     : ansistring;
   FileVerInfo: TFileVersionInfo;
begin
     with netState do
     begin
          if not tcp then exit; //for future evolutions of synapseserver
          case msgType of
               CONNECT_MSG :
               begin
               end;
               DISCONNECT_MSG :
               begin
               end;
               SENDSTRING_MSG :
               begin
                    if StringMatch(rs_HELP,msg,PERFECT_MATCH,i) then
                    begin
                         _synapseServer.Server_SendMessage(@id,'-- Available commands : ');
                         _synapseServer.Server_SendMessage(@id,'-- HELP/WHOAMI/STATUS/VERSION/MOTD/KTHXBAI');
                         exit;
                    end;

                    if StringMatch(rs_WHOAMI,msg,PERFECT_MATCH,i) then
                    begin
                         _synapseServer.Server_SendMessage(@id,'-- You are peer '+
                                                           peerIp+' on port '+
                                                           SysUtils.IntToStr(peerPort));
                         exit;
                    end;

                    if StringMatch(rs_STATUS,msg,PERFECT_MATCH,i) then
                    begin
                         _synapseServer.Server_SendMessage(@id,'-- XSWAG server running | '+
                                                           SysUtils.IntToStr(_synapseServer.ClientCount)
                                                           +'/'+
                                                           SysUtils.IntToStr(_synapseServer.MaxConnectedClients));
                         exit;
                    end;

                    if StringMatch(rs_VERSION,msg,PERFECT_MATCH,i) then
                    begin
                         FileVerInfo := TFileVersionInfo.Create(nil);
                         try
                            FileVerInfo.ReadFileInfo;
                            _synapseServer.Server_SendMessage(@id,FileVerInfo.VersionStrings.Values['ProductVersion']);
                         finally
                            FileVerInfo.Free;
                         end;
                         exit;
                    end;

                    if StringMatch(rs_MOTD,msg,PERFECT_MATCH,i) then
                    begin
                         _synapseServer.Server_SendMessage(@id,_motd);
                         exit;
                    end;

                    if StringMatch(rs_KTHXBAI,msg,PERFECT_MATCH,i) then
                    begin
                         _synapseServer.Server_SendMessage(@id,'BAI!');
                         _synapseServer.Server_Kick(@id);
                         exit;
                    end;

                    _synapseServer.Server_Kick(@id);
               end;
          end;
     end;
end;


procedure TDeepRedServer.OnError(ASender: TObject; AException: Exception);
begin
     Self.WriteLog(etError, AException.Message);
end;


constructor TDeepRedServer.Create(brookfile : string; dbfile : string);
begin
     _running  := false;
     _errorStr := '';
     _motd     := '';
     //_sqliteClient := THexSQLiteClient.Create;
     //if not(_sqliteClient.OpenSQLiteDB(dbfile)) then _errorStr := _sqliteClient.Error;

     //_synapseServer := TTCP_NetworkServer.Create(timeout,port,maxConnections);
     //_synapseServer.OnNetworkEvent := @OnNetworkEvent;
     //BrookSettings.Configuration := brookfile;
     try
           _errorStr := 'Error while loading '+BROOKLIBNAME;
           LoadBrookLib('/usr/local/lib64/libsagui.so.2.4.0');
           _brookServer := THTTPServer.Create(nil);
           //_brookServer.OnError:= @OnError;
     except
           WriteLog(etError, _errorStr);
           halt(1);
     end;

     with _brookServer do
     begin
          Port:= 8080;
          ConnectionLimit := 100;
          Threaded := True;
          NoFavicon := True;
     end;
     _brookServer.Open;
     if not _brookServer.Active then
     begin
          WriteLog(etError, 'HTTP Server not active.');
          halt(1);
     end;
end;


destructor TDeepRedServer.Destroy;
begin
     //_sqliteClient.CloseSQLiteDB;
     //_sqliteClient.Free;
     //_synapseServer.Terminate;
     try
        _brookServer.Close;
        _brookServer.Free;
        UnloadBrookLib;
     finally
     end;
end;


procedure TDeepRedServer.AddBinding(const address : ansistring);
begin
     //_synapseServer.AddNewBinding(address);
end;


procedure TDeepRedServer.ClearBindings;
begin
     //_synapseServer.ClearBindings;
end;


function TDeepRedServer.StartServer : boolean;
begin
     Result := true;
     (*if not(_synapseServer.Server_Activate) then
     begin
          _errorStr := 'DeepRed server already up!';
          Result := false;
     end;     *)
end;


function TDeepRedServer.StopServer : boolean;
begin
     Result := true;
     (*if not(_synapseServer.Server_Deactivate) then
     begin
          _errorStr := 'DeepRed server already down!';
          Result := false;
     end;   *)
end;


end.

