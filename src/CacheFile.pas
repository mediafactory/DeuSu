unit CacheFile;

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
  Classes;

type
  TCacheFile = class
  strict protected
    FBuf: array [0..64*1024-1] of byte;
    FBufLen: int64;
    FBufStart: int64;
    FFile: TFileStream;
    FFileName: string;
    FPosition: int64;
    FSize: int64;
  public
    procedure Assign(FileName: string);
    procedure Close;
    function Eof: boolean;
    function FilePos: int64;
    function FileSize: int64;
    procedure Read(var Data; Len: int64);
    procedure Reset;
    procedure Seek(NewPosition: int64);
  end;

  TBufWriteFile = class
  strict protected
    FBuf: array [0..16*1024*1024-1] of byte;
    FBufLen: int64;
    FFile: TFileStream;
    FFileName: string;
    FSize: int64;

    procedure FlushBuffer;
  public
    procedure Assign(FileName: string);
    procedure Close;
    function FileSize: int64;
    procedure Reset;
    procedure ReWrite;
    procedure Write(var Data; Len: int64);
  end;

  TPreloadedFile = class
  strict protected
    FCacheData: array of byte;
    FCacheSize: int64;
    FFile: TFileStream;
    FFileName: string;
    FPosition: int64;

    procedure Open(Mode: word);
  public
    procedure Assign(FileName: string);
    procedure Close;
    function Eof: boolean;
    function FilePos: int64;
    function FileSize: int64;
    function IsPreloaded: boolean;
    procedure OpenRead;
    procedure OpenReadWrite;
    procedure Preload;
    procedure Read(var Data; Len: int64);
    procedure Seek(NewPosition: int64);
    procedure UnloadCache;
    procedure Write(var Data; Len: int64);
  end;



implementation

uses
  SysUtils;

type
  tBufArr = array [0..1000*1000*1000] of byte;
  pBufArr = ^tBufArr;



procedure TBufWriteFile.Assign(FileName: string);
begin
  FFileName := FileName;
end;



procedure TBufWriteFile.ReWrite;
begin
  FFile := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
  FBufLen := 0;
  FSize := 0;
end;



procedure TBufWriteFile.Reset;
begin
  FFile := TFileStream.Create(FFileName,
    fmOpenReadWrite or fmShareDenyNone);
  FBufLen := 0;
  FSize := 0;
end;



procedure TBufWriteFile.FlushBuffer;
begin
  if FBufLen > 0 then
  begin
    FFile.Write(FBuf, FBufLen);
    FBufLen := 0;
  end;
end;



procedure TBufWriteFile.Close;
begin
  FlushBuffer;
  FFile.Free;
end;



procedure TBufWriteFile.Write(var Data; Len: int64);
begin
  if ((FBufLen + Len) > SizeOf(FBuf)) and
    (FBufLen > 0) then
    FlushBuffer;

  if Len <= SizeOf(FBuf) then
  begin
    Move(Data, FBuf[FBufLen], Len);
    Inc(FBufLen, Len);
  end
  else
    FFile.Write(Data, Len);

  Inc(FSize, Len);
end;



function TBufWriteFile.FileSize:int64;
begin
  Result := FSize;
end;



{ ----------------------------------------------- }



procedure TCacheFile.Assign(FileName: string);
begin
  FFileName := FileName;
end;



procedure TCacheFile.Reset;
begin
  FFile := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  FSize := FFile.Size;

  FPosition := 0;
  FBufStart := -1;
  FBufLen := -1;
end;



procedure TCacheFile.Close;
begin
  FFile.Free;
end;



procedure TCacheFile.Read(var Data; Len: int64);
var
  pBuffer: pBufArr;
  startAt: integer;
  Bytes: int64;
begin
  if (Len = 1) and
    (FPosition >= FBufStart) and
    (FPosition < FBufStart + FBufLen) then
  begin
    Move(FBuf[FPosition - FBufStart], Data, 1);
    Inc(FPosition);
    exit;
  end;

  pBuffer := @Data;
  startAt := 0;

  while Len > 0 do
  begin
    if (FBufStart = -1) or
      (FPosition < FBufStart) or
      (FPosition >= (FBufStart + FBufLen)) then
    begin
      FBufStart := FPosition;
      FBufLen := FSize - FBufStart;
      if FBufLen > SizeOf(FBuf) then
        FBufLen := SizeOf(FBuf);

      FFile.Position := FBufStart;
      FFile.Read(FBuf, FBufLen);
    end;

    Bytes := FBufLen + FBufStart - FPosition;
    if Bytes > Len then Bytes := Len;

    Move(FBuf[FPosition - FBufStart], pBuffer^[startAt], Bytes);
    Inc(FPosition, Bytes);
    Inc(startAt, Bytes);
    Dec(Len, Bytes);
  end;
end;



function TCacheFile.FileSize:int64;
begin
  Result := FSize;
end;



function TCacheFile.FilePos:int64;
begin
  Result := FPosition;
end;



procedure TCacheFile.Seek(NewPosition: int64);
begin
  FPosition := NewPosition;
end;



function TCacheFile.Eof:boolean;
begin
  if FSize > 0 then
    Result := FPosition >= FSize
  else
    Result := FPosition = FSize;
end;



{ -------------------------------------------- }



procedure TPreloadedFile.Assign(FileName: string);
begin
  FFileName := FileName;
  FCacheSize := 0;
  SetLength(FCacheData, 1);
end;



procedure TPreloadedFile.Open(Mode: word);
begin
  FCacheSize := 0;
  SetLength(FCacheData, 1);
  FPosition := 0;

  if not FileExists(FFileName) then
  begin
    (*
      Create the file and close it again. This is necessary as
      Windows would ignore fmShareDenyNone if fmCreate is also used.
      So we need to *always* open an already existing file with
      fmShareDenyNone.
    *)
    FFile := TFileStream.Create(FFileName, fmCreate or Mode);
    FFile.Free;
  end;

  FFile := TFileStream.Create(FFileName, Mode, fmShareDenyNone);
end;



procedure TPreloadedFile.OpenRead;
begin
  Open(fmOpenRead);
end;



procedure TPreloadedFile.OpenReadWrite;
begin
  Open(fmOpenReadWrite);
end;



procedure TPreloadedFile.Close;
begin
  UnloadCache;
  FFile.Free;
end;



procedure TPreloadedFile.UnloadCache;
begin
  if IsPreloaded then
  begin
    if FCacheSize <> 0 then
    begin
      FCacheSize := 0;
      SetLength(FCacheData, 1);
    end;
  end;
end;



procedure TPreloadedFile.Preload;
var
  OldFilePos: int64;
  Po, Len, Bytes: int64;
begin
  // Make sure we have NO cache before we create a new one
  UnloadCache;


  FCacheSize := FileSize;
  try
    SetLength(FCacheData, FCacheSize);
  except
    FCacheSize := 0;
    SetLength(FCacheData, 1);
    WriteLn(#13'Memory allocation in TPreloadedFile.Preload failed.');
    exit;
  end;

  if (High(FCacheData) + 1) <> FCacheSize then
  begin
    FCacheSize := 0;
    SetLength(FCacheData, 1);
    WriteLn(#13'Memory allocation in TPreloadedFile.Preload failed.');
    exit;
  end;


  OldFilePos := FilePos; // Remember the current file-position
  try
    FFile.Position := 0;
    Po := 0;
    Bytes := FCacheSize;
    while Bytes > 0 do
    begin
      Len := Bytes;
      if Len > 1048576 then Len := 1048576;

      try
        FCacheData[Po] := 0;
      except
        WriteLn('Oopsie... #1  High(CacheData)=', High(FCacheData));
      end;

      try
        FFile.Read(FCacheData[Po], Len);
      except
        WriteLn('Oopsie... #2');
      end;

      Inc(Po, Len);
      Dec(Bytes, Len);
    end;
  except
    WriteLn(#13'Cache Preload in TPreloadedFile.Preload failed.');
    WriteLn('ReadFile caused an exception.');
    UnloadCache;
  end;

  Seek(OldFilePos); // Back to previous file-position
end;



procedure TPreloadedFile.Read(var Data; Len: int64);
begin
  // Is the data to be read completely within the cache?
  if IsPreloaded and ((FPosition + Len) <= FCacheSize) then
    Move(FCacheData[FPosition], Data, Len) // Read data from cache
  else
    if FFile.Read(Data, Len) <> Len then
      System.Write('Uh-oh... Read failed...');

  Inc(FPosition, Len);
end;



procedure TPreloadedFile.Write(var Data; Len: int64);
begin
  // Will the data to be written be completely within the cache?
  if IsPreloaded and ((FPosition + Len) <= FCacheSize) then
    Move(Data, FCacheData[FPosition], Len); // Write into cache

  FFile.Write(Data, Len); // Also write to disk
  Inc(FPosition, Len);
end;



function TPreloadedFile.FileSize:int64;
begin
  Result := FFile.Size;
end;



function TPreloadedFile.IsPreloaded: boolean;
begin
  Result := High(FCacheData) > 0;
end;



function TPreloadedFile.FilePos:int64;
begin
  Result := FPosition;
end;



procedure TPreloadedFile.Seek(NewPosition: int64);
begin
  FFile.Position := NewPosition;
  FPosition := NewPosition;
end;



function TPreloadedFile.Eof:boolean;
begin
  Result := FilePos >= FileSize;
end;



end.
