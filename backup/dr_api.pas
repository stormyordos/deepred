unit dr_api;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, BrookLibraryLoader, BrookHTTPRequest, BrookHTTPAuthentication, BrookHTTPResponse,
    BrookHTTPServer, BrookHTTPRouter;


type
    //TPSOnUses = function( ; const data: string): boolean;

    //list listeners
    TRouteListListeners =
    class (TBrookHTTPRoute)
          protected
                test :
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //create listener
    TRouteCreateListener =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show loaded listener types
    TRouteShowListenerTypes =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show listener type information
    TRouteShowListenerTypeInfo =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show specific listener details
    TRouteShowListenerDetails =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //start listener
    TRouteStartListener =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //stop listener
    TRouteStopListener =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //list drones
    TRouteListDrones =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show loaded launcher types
    TRouteShowLauncherTypes =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show launcher type information
    TRouteShowLauncherTypeInfo =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //generate launcher
    TRouteGenLauncher =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show drone information
    TRouteShowDroneInfo =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //show drone loaded modules
    TRouteShowDroneModules =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //load module into drone
    TRouteLoadDroneModule =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //load script into drone
    TRouteLoadDroneScript =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //run script from drone
    TRouteRunDroneScript =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //run shell command from drone
    TRouteRunDroneShell =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    //debug info
    TRouteHome =
    class (TBrookHTTPRoute)
          protected
                procedure DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                                       ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
          public
                procedure AfterConstruction; override;
    end;

    TRouter =
    class (TBrookHTTPRouter)
          protected
                procedure DoNotFound(ASender: TObject; const ARoute: string;
                                     ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse); override;
    end;

    THTTPServer =
    class (TBrookHTTPServer)
          private
                _router : TRouter;
          protected
                function DoAuthenticate(ASender: TObject; AAuthentication: TBrookHTTPAuthentication;
                                        ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): boolean; override;
                procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
                                    AResponse: TBrookHTTPResponse); override;
          public
                constructor Create(AOwner : TComponent); override;
    end;


const
     BROOKLIBNAME = TBrookLibraryLoader.LIB_NAME;


procedure LoadBrookLib(const libName: string);
procedure UnloadBrookLib;


implementation


procedure LoadBrookLib(const libName: string);
begin
     TBrookLibraryLoader.Load(libName);
end;


procedure UnloadBrookLib;
begin
     TBrookLibraryLoader.Unload;
end;


//==============================================================================
// TRouteHome
//==============================================================================

procedure TRouteHome.AfterConstruction;
begin
     Methods := [rmGET];
     Pattern := '/home';
     Default := True;
end;


procedure TRouteHome.DoRequest(ASender: TObject; ARoute: TBrookHTTPRoute;
                               ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
var
   response: string;
begin
     response := '<html><head><title>Hello world</title></head><body><h1>Deep Red Server Info:</h1>';
     response := response + '<br/><br/>';
     response := response + '<h2>Headers:</h2><br/>'+ARequest.Headers.ToString;
     response := response + '<br/><br/>';
     response := response + '<h2>IP:</h2><br/>'+ARequest.IP;
     response := response + '<br/><br/>';
     response := response + '<h2>Fields:</h2><br/>'+ARequest.Fields.ToString;
     response := response + '<br/><br/>';
     response := response + '<h2>Cookies:</h2><br/>'+ARequest.Cookies.ToString;
     response := response + '<br/><br/>';
     response := response + '<h2>Path:</h2><br/>'+ARequest.Path;
     response := response + '<br/><br/>';
     response := response + '<h2>Params:</h2><br/>'+ARequest.Params.ToString;
     response := response + '<br/><br/>';
     response := response + '<h2>Method:</h2><br/>'+ARequest.Method;
     response := response + '<br/><br/>';
     response := response + '</body></html>';

     AResponse.Send(response, 'text/html; charset=utf-8', 200);
end;

//==============================================================================
// TRouter
//==============================================================================

procedure TRouter.DoNotFound(ASender: TObject; const ARoute: string;
                             ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse);
begin
     AResponse.Send('Page not found', 'text/plain', 404);
end;

//==============================================================================
// THTTPServer
//==============================================================================

constructor THTTPServer.Create(AOwner : TComponent);
begin
     inherited Create(AOwner);
     _router := TRouter.Create(Self);
     TRouteHome.Create(_router.Routes);
     //TRouteDownload.Create(FRouter.Routes);
     //TRoutePage.Create(FRouter.Routes);
     _router.Active := True;
end;


function THTTPServer.DoAuthenticate(ASender: TObject; AAuthentication: TBrookHTTPAuthentication;
                                    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse): boolean;
begin

end;


procedure THTTPServer.DoRequest(ASender: TObject;
                                ARequest: TBrookHTTPRequest;
                                AResponse: TBrookHTTPResponse);
begin
     //AResponse.Send('Hello,world!', 'text/plain', 200);
     _router.Route(ASender, ARequest, AResponse);
end;



end.

