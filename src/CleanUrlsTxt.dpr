program CleanUrlsTxt;

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

{$IfNDef LINUX}
{$APPTYPE CONSOLE}
{$EndIf}

uses
  SysUtils,
  Hash,
  DbTypes,
  GlobalTypes,
  UrlDatabase;

const
  cMaxHash = 1024*1024-1;

type
  pEntry = ^tEntry;

  tEntry = record
    Next: pEntry;
    s: shortstring;
  end;

var
  Entries: array [0..cMaxHash] of pEntry;
  DuplicatesCount: int64;
  deOnly: boolean;
  LinesIn, LinesOut: int64;



function NodeExists(HashCode: integer; s: shortstring): boolean;
var
  p: pEntry;
begin
  s := LowerCase(s);

  p := Entries[HashCode];
  while p <> nil do
  begin
    if LowerCase(p^.s) = s then
    begin
      Inc(DuplicatesCount);
      exit(true);
    end;
    p := p^.Next;
  end;

  Result := false;
end;


procedure ReadUrlsFromFile(const FileName: string);
var
  f: TextFile;
  s, Url: string;
  i: integer;
  p: pEntry;
  DbNr: integer;
  HashCode: uint32;
begin
  LinesIn := 0;

  AssignFile(f, FileName);
  Reset(f);

  while not eof(f) do
  begin
    ReadLn(f, Url);
    Inc(LinesIn);
    if (LinesIn and 16383) = 0 then
      Write(#13'Reading... ', LinesIn);

    i := Pos(#255, Url);
    if i > 0 then
      Url := copy(Url, 1, i - 1);

    i := Pos(#8, Url);
    if i > 0 then
      Delete(Url, 1, i);

    if Length(Url) > 0 then
      if (Url[1] = ' ') or
        (Url[Length(Url)] = ' ') then
        Url := Trim(Url);

    while LowerCase(copy(Url, 1, 7)) = 'http://' do
      Delete(Url, 1, 7);

    if Length(Url) > 0 then
    begin
      if Pos('/', Url) = 0 then Url := Url + '/';

      if copy(Url, Length(Url) - 1, 2) = '//' then
        Url := copy(Url, 1, Length(Url) - 1);

      s := Url;

      if Pos('?utm_source=feedburner&', LowerCase(s)) > 0 then
          s := copy(s, 1, Pos('?', s) - 1);

      if Pos('?utm_source=rss&', LowerCase(s)) > 0 then
          s := copy(s, 1, Pos('?', s) - 1);

      if Pos('?partner=rss&', LowerCase(s)) > 0 then
          s := copy(s, 1, Pos('?', s) - 1);

      if (s <> '') and
        (Pos(#39, s) = 0) and
        (Length(s) < 255) then
      begin
        if (not deOnly) or
          (deOnly and (Pos('.de/', s) > 0)) then
        begin
          i := Pos('&amp;', LowerCase(s));
          if i > 0 then
            repeat
              Delete(s, i + 1, 4);
              i := Pos('&amp;', LowerCase(s));
            until i <= 0;


          DbNr := DbNrOfUrl(LowerCase(s));
          HashCode := CalcCRC(LowerCase(s)) and cMaxHash;
          HashCode := ((HashCode shl cDbBits) or DbNr) and cMaxHash;

          // if not NodeExists(HashCode, s) then
          begin
            GetMem(p, SizeOf(tEntry) - 255 + Length(s));
            p^.Next := Entries[HashCode];
            p^.s := s;
            Entries[HashCode] := p;
          end;
        end;
      end;
    end;
  end;


  CloseFile(f);
  Write(#13'Reading... ', LinesIn, ' ');
end;



procedure WriteUrlsToFiles(const FileName: string);
var
  DbNr,i: integer;
  f: TextFile;
  TextBuf: array[1..16384] of char;
  p: pEntry;
begin
  Write('Writing...');
  LinesOut := 0;

  for DbNr := 0 to cDbCount - 1 do
  begin
    AssignFile(f, FileName + IntToStr(DbNr));
    SetTextBuf(f, TextBuf);
    if FileExists(FileName + IntToStr(DbNr)) then
      Append(f)
    else
      ReWrite(f);

    for i := 0 to cMaxHash do
    begin

      if (i and (cDbCount - 1)) = DbNr then
      begin
        p := Entries[i];

        while p <> nil do
        begin
          WriteLn(f, p^.s);
          Inc(LinesOut);
          p := p^.Next;
        end;

      end;

    end;
    CloseFile(f);
  end;

  WriteLn;
end;



var
  i: integer;

begin
  WriteLn('CleanUrlsTxt ', cVersionCopy);
  WriteLn(cGPLNotice);
  WriteLn;


  deOnly := LowerCase(ParamStr(3)) = 'deonly';

  for i := 0 to cMaxHash do
    Entries[i] := nil;
  DuplicatesCount := 0;

  ReadUrlsFromFile(ParamStr(1));
  WriteUrlsToFiles(ParamStr(2));

  WriteLn('LinesIn=', LinesIn);
  WriteLn('LinesOut=', LinesOut);
  WriteLn('Duplicates=', DuplicatesCount);

end.
