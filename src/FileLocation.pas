unit FileLocation;

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

var
  cUrls,cAddUrl,cIgnorePages,cUrlList,
  cIgnoreHosts,cUrlDb,cInfDb,cKeyDb,
  cInPath,cParsedPath,cImportPath,cSpeedTrap,
  cSDataPath,cTempPath,
  cSearchTempDir,cSearchLogFile,
  cSearchFirstPath,cSearchSecondPath: string;



implementation

uses
  Config;



procedure CorrectPathSeparators(var Path: string);
{$ifdef UNIX}
var
  i: integer;
{$endif}
begin
  {$ifdef UNIX}
  for i:=1 to length(Path) do
    if Path[i]='\' then Path[i]:='/';
  {$endif}
end;



begin
  (*
    I used ConfigReadString and manual checking for empty results
    because ConfigReadStringDefault will return empty strings despite
    having a different default if the setting exists, but has no value.
    The file- and pathnames here MUST NOT be empty, so extra checking
    is needed.
    TODO: There is need for additional checking here. Pathnames should
    already exist or otherwise be created.
  *)

  cUrls:=Config.ReadString('file-system.Urls');
  if cUrls='' then
    cUrls:='data\txt\urls.txt';
  CorrectPathSeparators(cUrls);

  cAddUrl:=Config.ReadString('file-system.AddUrl');
  if cAddUrl='' then
    cAddUrl:='data\txt\addurl.txt';
  CorrectPathSeparators(cAddUrl);

  cUrlList:=Config.ReadString('file-system.UrlList');
  if cUrlList='' then
    cUrlList:='data\txt\robot.url';
  CorrectPathSeparators(cUrlList);

  cIgnorePages:=Config.ReadString('file-system.IgnorePages');
  if cIgnorePages='' then
    cIgnorePages:='data\txt\ignorepages.txt';
  CorrectPathSeparators(cIgnorePages);

  cIgnoreHosts:=Config.ReadString('file-system.IgnoreHosts');
  if cIgnoreHosts='' then
    cIgnoreHosts:='data\txt\ignorehosts.txt';
  CorrectPathSeparators(cIgnoreHosts);



  cUrlDb:=Config.ReadString('file-system.UrlDb');
  if cUrlDb='' then
    cUrlDb:='data\db2.url';
  CorrectPathSeparators(cUrlDb);

  cInfDb:=Config.ReadString('file-system.InfDb');
  if cInfDb='' then
    cInfDb:='data\db2.inf';
  CorrectPathSeparators(cInfDb);

  cKeyDb:=Config.ReadString('file-system.KeyDb');
  if cKeyDb='' then
    cKeyDb:='data\db2.key';
  CorrectPathSeparators(cKeyDb);




  cInPath:=Config.ReadString('file-system.InPath');
  if cInPath='' then
    cInPath:='data\crawler\in\';
  CorrectPathSeparators(cInPath);

  cParsedPath:=Config.ReadString('file-system.ParsedPath');
  if cParsedPath='' then
    cParsedPath:='data\crawler\parsed\';
  CorrectPathSeparators(cParsedPath);

  cImportPath:=Config.ReadString('file-system.ImportPath');
  if cImportPath='' then
    cImportPath:='data\crawler\import\';
  CorrectPathSeparators(cImportPath);

  cSpeedTrap:=Config.ReadString('file-system.SpeedTrap');
  if cSpeedTrap='' then
    cSpeedTrap:='data\crawler\speedtrap.dat';
  CorrectPathSeparators(cSpeedTrap);



  cSDataPath:=Config.ReadString('file-system.SDataPath');
  if cSDataPath='' then
    cSDataPath:='data\sdata1\';
  CorrectPathSeparators(cSDataPath);

  cTempPath:=Config.ReadString('file-system.TempPath');
  if cTempPath='' then
    cTempPath:='data\tmp\';
  CorrectPathSeparators(cTempPath);



  cSearchTempDir:=Config.ReadString('file-system.Search.TempDir');
  if cSearchTempDir='' then
    cSearchTempDir:='data\search\';
  CorrectPathSeparators(cSearchTempDir);

  cSearchLogFile:=Config.ReadString('file-system.Search.LogFile');
  if cSearchLogFile='' then
    cSearchLogFile:='data\search.log';
  CorrectPathSeparators(cSearchLogFile);

  cSearchFirstPath:=Config.ReadString('file-system.Search.FirstPath');
  if cSearchFirstPath='' then
    cSearchFirstPath:='data\sdata1\';
  CorrectPathSeparators(cSearchFirstPath);

  cSearchSecondPath:=Config.ReadString('file-system.Search.SecondPath');
  if cSearchSecondPath='' then
    cSearchSecondPath:='data\sdata2\';
  CorrectPathSeparators(cSearchSecondPath);

end.
