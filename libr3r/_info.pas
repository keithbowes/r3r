unit Info;

interface

uses
{$IFDEF UNIX}
  BaseUnix
{$ELSE}
  {$IFDEF WIN32}
    SysUtils, Windows
  {$ELSE}
    Dos, SysUtils
  {$ENDIF}
{$ENDIF};

const
  Version = '@VERSION@';

function Os: String;

implementation

function Os: String;
var
  Name, Version: String;
  Data: Pointer;
begin
{$IFDEF UNIX}
  Data := GetMem(SizeOf(UtsName));
  FpUname(UtsName(Data^));
  Name := UtsName(Data^).SysName;
  Version := UtsName(Data^).Release;
  FreeMem(Data);
{$ELSE}
  {$IFDEF WIN32}
    New(LPOSVERSIONINFO(Data));
    with OSVERSIONINFO(Data^) do
    begin
      dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
      GetVersionEx(Data);
      case dwPlatformId of
        VER_PLATFORM_WIN32_NT:
        begin
          Name := 'Windows NT';
        end;
        VER_PLATFORM_WIN32_WINDOWS:
        begin
          Name := 'Windows';
        end;
        VER_PLATFORM_WIN32s:
        begin
          Name := 'Win32s';
        end;
      end;
      Version := IntToStr(dwMajorVersion) + '.' + IntToStr(dwMinorVersion);
    end;
    Dispose(LPOSVERSIONINFO(Data));
  {$ELSE}
    Name := 'DOS';
    Version := IntToStr(Lo(DosVersion)) + '.' + IntToStr(Hi(DosVersion));
  {$ENDIF}
{$ENDIF}

  Result := Name + ' ' + Version;
end;

end.
