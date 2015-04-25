unit DNSResolver;

(*
  DeuSu - An OpenSource Internet-Search-Engine
  Copyright (C) 1999-2015 Michael Schoebel & Acoon GmbH

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License version 2 as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
*)

interface

uses
  GlobalTypes,
  robotglobal;


function GetIP4ByHostName(const AHostName: AnsiString): TIP4;



implementation

uses
  {$IfDef MSWINDOWS}
  WinSock,
  Windows,
  {$Else}
  NetDb,
  {$EndIf}
  OSWrapper,
  Logging,
  Hash,
  Classes,
  SyncObjs,
  SysUtils;

const
  MaxIP4s = 10;
  MaxHash = 10024*1024-1;

  // TODO: Make configurable in deusu.config[.default] !!!!
  DNSCache = 'dnscache.dat';


type
  PDNSData = ^TDNSData;
  TDNSData = record
    Next: PDNSData;
    IP: TIP4;
    HostName: shortstring;
  end;


var
  CritSec: TCriticalSection;
  HashTable: array [0..MaxHash] of PDNSData;



procedure AddToMemoryCache(s: string);
var
  HostName: AnsiString;
  IP: tIP4;
  Data: pDNSData;
  i: integer;
  i64: int64;
  HashCode: integer;
begin
  HostName := AnsiString(LowerCase(copy(s, 1, i-1)));
  if Length(HostName) > 255 then
    exit;

  Delete(s, 1, i);
  if TryStrToInt64(s, i64) then
  begin
    IP.IP := i64;
    HashCode := CalcCRC(HostName) and MaxHash;
    GetMem(Data, SizeOf(tDNSData)-255+Length(HostName));
    Data^.IP := IP;
    Data^.HostName := HostName;
    Data^.Next := HashTable[HashCode];
    HashTable[HashCode] := Data;
  end;
end;



procedure LoadCacheData;
var
  f: TextFile;
  s: string;
  i: integer;
begin
  FillChar(HashTable, SizeOf(HashTable), 0);

  if not FileExists(DNSCache) then
    exit;


  AssignFile(f, DNSCache);
  Reset(f);

  while not eof(f) do
  begin
    ReadLn(f, s);

    i := Pos('=', s);
    if i > 0 then
      AddToMemoryCache(s);
  end;

  CloseFile(f);
end;



procedure AddToCache(const AHostName: AnsiString; const IP: tIP4);
var
  f: TextFile;
begin
  // Should never happen, but better safe than sorry
  if Pos('=', AHostName) > 0 then
    exit;

  CritSec.Acquire;
  try
    AssignFile(f, DNSCache);
    if FileExists(DNSCache) then
      Append(f)
    else
      ReWrite(f);
    WriteLn(f, AHostName, '=', IP.IP);
    CloseFile(f);
  finally
    CritSec.Release;
  end;
end;



function ResolveHostByNameIPv4(const AHostName: AnsiString): tIP4;
var
  {$IfDef MSWINDOWS}
  HostName: array [0 .. 255] of AnsiChar;
  HostEnt: pHostEnt;
    IPCount: integer;
    IP: array [0 .. MaxIP4s - 1] of tIP4;
    p: pAnsiChar;
    p2: pointer;
  {$Else}
  IPv4Arr: array of tHostAddr;
  retVal: integer;
  {$EndIf}
  i: integer;
begin
  {$IfDef MSWINDOWS}
  StrPCopy(HostName, AHostName);
  HostEnt := GetHostByName(HostName);
  if HostEnt <> nil then
  begin
    if (HostEnt^.h_addrtype = AF_INET) and (HostEnt^.h_length = 4) then
    begin
      IpCount := 0;
      p2 := pointer(HostEnt^.h_addr_list);
      while (p2 <> nil) and (IpCount < MaxIP4s) do
      begin
        p := PAnsiChar(p2^);
        if p = nil then
          break;

        Move(p^, IP[IpCount], 4);
        Inc(IpCount);
        Inc(NativeInt(p2), SizeOf(p));
      end;
      Result := IP[Random(IpCount)];
    end;
  end;
  {$Else}
  SetLength(IPv4Arr, 10);
  retVal := ResolveName(AHostName, IPv4Arr);

  (*
  WriteLn('High(Ipv4Array)=',High(IPv4Arr));
  for i:=0 to High(IPv4Arr) do
  begin
    Result.IP:=IPv4Arr[ i ].s_addr;
    WriteLn('IP#',i,'=',Ip2Str(Result));
  end;
  *)

  if retVal = 0 then
    Result.IP := 0
  else
    Result.IP := IPv4Arr[ 0 ].s_addr;
  {$EndIf}
  // WriteLn('Host=',aHostName,'   IP=',Ip2Str(Result));
end;



function GetIP4ByHostName(const AHostName: AnsiString): TIP4;
// Note: AHostName is assumed to always be lower-case!
var
  Ti: cardinal;
  HashCode: integer;
  Data: pDNSData;
begin
  Result.IP := 0;
  if Length(AHostName) > 255 then
    exit;

  HashCode := CalcCRC(AHostName) and MaxHash;
  Data := HashTable[HashCode];
  while Data <> nil do
  begin
    if Data^.HostName = AHostName then
      exit(Data^.IP)
    else
      Data := Data^.Next;
  end;
  // DebugLogMsg('robot.log','Couldn''t find DNS cache-entry for "'+AHostName+'".');

  Ti := GetTickCount;
  Result := ResolveHostByNameIPv4(AHostName);
  Ti := GetTickCount - Ti;
  if Ti > 20000 then
    LogMsg('robot.log', 'DNS lookup for "' + AHostName + '" took ' + IntToStr(Ti) + 'ms.');

  if Result.IP <> 0 then
    AddToCache(AHostName, Result);
end;



{$IfDef MSWINDOWS}
procedure Startup;
var
  ErrorCode: integer;
  WSAData: TWSAData;
begin
  ErrorCode := WSAStartup($0101, WSAData);
end;
{$EndIf}



begin
  CritSec := tCriticalSection.Create;
  LoadCacheData;
  {$IfDef MSWINDOWS}
  Startup;
  {$EndIf}
end.
