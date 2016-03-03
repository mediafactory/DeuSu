program DeuSuCompressSnippets;

(*
    DeuSu - An OpenSource Internet-Search-Engine
    This file Copyright (C) 2016 Michael Schoebel

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License version 2 as
    published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
*)

uses
    SysUtils,
    dsSnippets;


procedure DoFiles(Filename: UTF8String);
var
    Sr: TSearchRec;
    Res: integer;
    s: UTF8String;
begin
    Res := FindFirst(FileName, faAnyFile, Sr);
    while Res = 0 do
    begin
        s := ExtractFilePath(FileName) + Sr.Name;
        if copy(Sr.Name, 1, 11) <> 'compressed.' then
        begin
            Write(s, '...');
            CompressSnippets(s);
            WriteLn(' Done.');
        end;
        Res := FindNext(Sr);
    end;
    FindClose(Sr);
end;



var
    i: integer;
begin
    for i := 1 to ParamCount do
        DoFiles(ParamStr(i));
end.
