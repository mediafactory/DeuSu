unit OSWrapper;

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
    Types;

{$IfNDef DCC}
type
    RawByteString = AnsiString;
{$EndIf}

function GetTickCount:int64;
{$ifdef DCC} function GetTickCount64:int64; {$endif}

implementation

uses
{$IfDef UNIX}
    SysUtils;
{$Else}
    Windows;
{$EndIf}


function GetTickCount:int64;
begin
    {$Ifdef UNIX}
    Result:=Trunc(Now*24*3600*1000);
    {$Else}
    Result:=Windows.GetTickCount;
    {$Endif}
end;


{$ifdef DCC}
function GetTickCount64:int64;
begin
    Result := GetTickCount;
end;
{$endif}



end.
