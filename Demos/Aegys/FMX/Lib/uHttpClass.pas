unit uHttpClass;

interface

uses
  System.Net.URLClient, System.Net.HTTPClient;

type
  TRDHttpProgress = procedure(AStartPosition, AEndPosition, AReadCount: Int64)
    of object;

  TRDHttp = class
  private
    FCancelar: Boolean;
    FEndPosition: Int64;
    FHTTPClient: THTTPClient;
    FOnProgress: TRDHttpProgress;
    FStartPosition: Int64;
  private
    procedure OnReceiveDataEvent(const Sender: TObject; AContentLength: Int64;
      AReadCount: Int64; var Abort: Boolean);
    procedure HTTPClientValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const Certificate: TCertificate;
      var Accepted: Boolean);
  public
    class function DataArquivo(AUrl: string): TDateTime;
    class function Download(AFile, AUrl: string;
      AProgressEvent: TRDHttpProgress): Boolean; overload;
    constructor Create;
    destructor Destroy; override;
    function Download(AFile, AUrl: string): Boolean; overload;
    property Cancelar: Boolean read FCancelar write FCancelar;
    property OnProgress: TRDHttpProgress read FOnProgress write FOnProgress;
  end;

  THttpClass = class of TRDHttp;

implementation

uses
  System.Classes, System.SysUtils, Winapi.Windows, IdGlobalProtocols,
  uLocaleFunctions, uConstants;

constructor TRDHttp.Create;
begin
  inherited Create;

  FHTTPClient := THTTPClient.Create;
  FHTTPClient.OnValidateServerCertificate :=
    HTTPClientValidateServerCertificate;
  // FHTTPClient.SecureProtocols := [
  // THTTPSecureProtocol.SSL2,
  // THTTPSecureProtocol.SSL3,
  // THTTPSecureProtocol.TLS1,
  // THTTPSecureProtocol.TLS11,
  // THTTPSecureProtocol.TLS12,
  // THTTPSecureProtocol.TLS13];
  FHTTPClient.OnReceiveData := OnReceiveDataEvent;

  FHTTPClient.ConnectionTimeout := 5000;
  FHTTPClient.ResponseTimeout := 15000;

  FCancelar := False;
  FOnProgress := nil;
  FEndPosition := -1;
  FStartPosition := -1;
end;

class function TRDHttp.DataArquivo(AUrl: string): TDateTime;
var
  httpResponse: IHTTPResponse;
  hClient: THTTPClient;
begin
  try
    Result := 0;
    hClient := THTTPClient.Create;
    hClient.ConnectionTimeout := 5000;
    hClient.ResponseTimeout := 15000;

    httpResponse := nil;

    httpResponse := hClient.Head(AUrl);
    if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode > 299) then
      raise Exception.Create(Format(Locale.GetLocale(MSGS, 'ServerError'),
        [httpResponse.StatusCode, httpResponse.StatusText]));

    Result := GMTToLocalDateTime(httpResponse.LastModified);
  finally
    httpResponse := nil;
    if Assigned(hClient) then
      FreeAndNil(hClient);
  end;
end;

destructor TRDHttp.Destroy;
begin
  if Assigned(FHTTPClient) then
    FreeAndNil(FHTTPClient);
  inherited Destroy;
end;

class function TRDHttp.Download(AFile, AUrl: string;
  AProgressEvent: TRDHttpProgress): Boolean;
var
  FHttp: TRDHttp;
begin
  try
    Result := False;
    FHttp := TRDHttp.Create;
    FHttp.OnProgress := AProgressEvent;
    Result := FHttp.Download(AFile, AUrl);
  finally
    FreeAndNil(FHttp);
  end;
end;

procedure TRDHttp.OnReceiveDataEvent(const Sender: TObject;
  AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
begin
  Abort := Cancelar;

  if Assigned(OnProgress) then
    OnProgress(FStartPosition, FEndPosition, AReadCount);
end;

function TRDHttp.Download(AFile, AUrl: string): Boolean;
var
  httpResponse: IHTTPResponse;
  fsFile: TFileStream;
begin
  try
    try
      Result := True;
      Cancelar := False;
      FEndPosition := -1;
      FStartPosition := -1;
      httpResponse := nil;
      fsFile := nil;

      httpResponse := FHTTPClient.Head(AUrl);
      if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode > 299) then
        raise Exception.Create(Format(Locale.GetLocale(MSGS, 'ServerError'),
          [httpResponse.StatusCode, httpResponse.StatusText]));

      fsFile := TFileStream.Create(AFile, fmCreate);
      fsFile.Seek(0, TSeekOrigin.soBeginning);

      FStartPosition := fsFile.Position;
      FEndPosition := httpResponse.ContentLength;

      httpResponse := FHTTPClient.Get(AUrl, fsFile);
      if (httpResponse.StatusCode < 200) or (httpResponse.StatusCode > 299) then
        raise Exception.Create(Format(Locale.GetLocale(MSGS, 'ServerError'),
          [httpResponse.StatusCode, httpResponse.StatusText]));
    except
      Result := False;
    end;
  finally
    if fsFile <> nil then
      fsFile.Free;
    httpResponse := nil;
  end;
end;

procedure TRDHttp.HTTPClientValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  Accepted := True;
end;

end.
