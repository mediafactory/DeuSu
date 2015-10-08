program ImportAlexa;

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

{$APPTYPE CONSOLE}


uses
    SysUtils;

var
    s: string;
    fIn, fOut: TextFile;
    i: integer;
    LinesWritten: integer;

begin
    WriteLn('Usage: ImportAlexa [InFile] [OutFile]');
    if ParamCount <> 2 then
    begin
        WriteLn('Incorrect number of arguments.');
        WriteLn('In most cases it should be:');
        WriteLn('ImportAlexa top-1m.csv domainrank.txt');
        halt;
    end;

    AssignFile(fIn, ParamStr(1));
    Reset(fIn);

    AssignFile(fOut, ParamStr(2));
    ReWrite(fOut);

    LinesWritten := 0;

    while not eof(fIn) do
    begin
        ReadLn(fIn, s);
        i := Pos(',', s);
        if i > 0 then Delete(s, 1, i);
        if Pos('/', s) = 0 then s := s + '/';
        if copy(s, 1, 4) <> 'www.' then s := 'www.' + s;

        if LowerCase(s) = 'www.wikipedia.org/' then
        begin
            WriteLn(fOut, 'en.wikipedia.org/');
            Inc(LinesWritten);
            if LinesWritten >= 1000000 then break;
        end;

        if LowerCase(s) = 'www.wikipedia.org/' then
        begin
            WriteLn(fOut, 'de.wikipedia.org/');
            Inc(LinesWritten);
            if LinesWritten >= 1000000 then break;
        end;

        WriteLn(fOut, s);
        Inc(LinesWritten);
        if LinesWritten >= 1000000 then break;
    end;

    CloseFile(fIn);
    CloseFile(fOut);

end.
