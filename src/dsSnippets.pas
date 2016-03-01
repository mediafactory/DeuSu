unit dsSnippets;

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

interface

uses
    CacheFile;



type
    TSnippet = record
        Url, Title, Description: UTF8String;
    end;



procedure CompressSnippets(
    InFile: UTF8String);

procedure GetUncompressedSnippet(
    FileName: UTF8String;
    Nr: int64;
    out Snippet: TSnippet);

procedure GetCompressedSnippet(
    FileName: UTF8String;
    Nr: int64;
    out Snippet: TSnippet);




implementation

uses
    SysUtils,
    Classes,
    DbTypes,
    paszlib;



type
    TUrlIndex = packed record
        FilePos: uint32;
        CompressedSize: uint16;
    end;

    TOldSnippet = packed record
        Url: string[cMaxUrlLength];
        Title: string[cMaxTitleLength];
        Description: string[255]; // Yes, this is a shortstring, but I wanted
                                  // to make it more clearly how long it is.
    end;



procedure CopyRawString(const s: shortstring; out OutStr: UTF8String);
begin
    SetLength(OutStr, Length(s));
    Move(s[1], OutStr[1], Length(s));
end;



procedure GetUncompressedSnippet(
    FileName: UTF8String;
    Nr: int64;
    out Snippet: TSnippet);
var
    f: TFileStream;
    OldSnippet: TOldSnippet;
begin
    OldSnippet.Url := '';
    OldSnippet.Title := '';
    OldSnippet.Description := '';

    try
        f := tFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
        f.Position := Nr * SizeOf(OldSnippet);
        f.Read(OldSnippet, SizeOf(OldSnippet));
        CopyRawString(OldSnippet.Url, Snippet.Url);
        CopyRawString(OldSnippet.Title, Snippet.Title);
        CopyRawString(OldSnippet.Description, Snippet.Description);
    finally
        f.Free;
    end;
end;



procedure ReadFromBuf(var s; InBuf: array of AnsiChar; var BufPos: int32);
var
    Bytes: integer;
begin
    Bytes := Ord(InBuf[BufPos]);
    Inc(Bytes);
    Move(InBuf[BufPos], s, Bytes);
    Inc(BufPos, Bytes );
end;



procedure GetCompressedSnippet(
    FileName: UTF8String;
    Nr: int64;
    out Snippet: TSnippet);
var
    f: TFileStream;
    OldSnippet: TOldSnippet;
    InBuf, OutBuf: array of AnsiChar;
    UrlIndex: TUrlIndex;
    OutLen: cardinal;
    BufPos: int32;
    ZLibResult: longint;
begin
    OldSnippet.Url := '';
    OldSnippet.Title := '';
    OldSnippet.Description := '';


    try
        f := tFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
        f.Position := Nr * SizeOf(UrlIndex);
        f.Read(UrlIndex, SizeOf(UrlIndex));

        f.Position := UrlIndex.FilePos;
        SetLength(InBuf, UrlIndex.CompressedSize);
        f.Read(InBuf[0], UrlIndex.CompressedSize);

        if UrlIndex.CompressedSize < SizeOf(OldSnippet) then
        begin
            SetLength(OutBuf, SizeOf(OldSnippet));
            OutLen := Length(OutBuf);
            ZLibResult := uncompress(@OutBuf[0], OutLen,
                @InBuf[0], UrlIndex.CompressedSize);
            if ZLibResult = Z_OK then
            begin
                BufPos := 0;
                ReadFromBuf(OldSnippet.Url, OutBuf, BufPos);
                ReadFromBuf(OldSnippet.Title, OutBuf, BufPos);
                ReadFromBuf(OldSnippet.Description, OutBuf, BufPos);
            end;
        end;

        CopyRawString(OldSnippet.Url, Snippet.Url);
        CopyRawString(OldSnippet.Title, Snippet.Title);
        CopyRawString(OldSnippet.Description, Snippet.Description);
    finally
        f.Free;
    end;
end;



procedure CompressSnippets(
    InFile: UTF8String);
var
    fIn: TCacheFile;
    fOut: TBufWriteFile;
    OldSnippet: TOldSnippet;
    InBuf, OutBuf: array of AnsiChar;
    UrlBytes, TitleBytes, DescriptionBytes, SourceBytes: int32;
    DestBytes: cardinal;
    InTotal, OutTotal: int64;
    UrlIndex: array of TUrlIndex;
    UrlNr: int32;
begin
    // Initialize Url, Title and Snippet so that the compiler doesn't
    // complain about them not being initialized.
    OldSnippet.Url := '';
    OldSnippet.Title := '';
    OldSnippet.Description := '';

    // We need at most SizeOf(OldSnippet) bytes for the input buffer
    SetLength(InBuf, SizeOf(OldSnippet));

    SetLength(OutBuf, 16384); // Probably WAY more than necessary

    InTotal := 0;
    OutTotal := 0;

    fIn := TCacheFile.Create;
    fIn.Assign(InFile);
    fIn.Reset;

    fOut := TBufWriteFile.Create;
    fOut.Assign(
        ExtractFilePath(InFile) + 'compressed.' + ExtractFileName(InFile));
    fOut.ReWrite;

    // Calculate how many URLs are in this file
    // and allocate UrlIndex accordingly.
    SetLength(UrlIndex, fIn.FileSize div SizeOf(OldSnippet));
    fOut.Write(UrlIndex[0], Length(UrlIndex) * SizeOf(TUrlIndex));

    UrlNr := 0;

    while not fIn.eof do
    begin
        fIn.Read(OldSnippet.Url, SizeOf(OldSnippet));

        UrlBytes := Length(OldSnippet.Url) + 1;
        TitleBytes := Length(OldSnippet.Title) + 1;
        DescriptionBytes := Length(OldSnippet.Description) + 1;

        Move(OldSnippet.Url, InBuf[0], UrlBytes);
        SourceBytes := UrlBytes;

        Move(OldSnippet.Title, InBuf[SourceBytes], TitleBytes);
        Inc(SourceBytes, TitleBytes);

        Move(OldSnippet.Description, InBuf[SourceBytes], DescriptionBytes);
        Inc(SourceBytes, DescriptionBytes);

        DestBytes := Length(OutBuf);
        if compress2(@OutBuf[0], DestBytes, @InBuf[0], SourceBytes, 9) <> Z_OK then DestBytes := 0;

        // In testing it never happened that the compressed snippet was
        // larger than - or of equal size as - the original OldSnippet.
        // This code will catch that eventuality though as it cannot be
        // ruled-out completely.
        if DestBytes >= SizeOf(OldSnippet) then
        begin
            DestBytes := SizeOf(OldSnippet);
            Move(InBuf[0], OutBuf[0], DestBytes);
        end;


        Inc(InTotal, SourceBytes);
        Inc(OutTotal, DestBytes + SizeOf(TUrlIndex));

        UrlIndex[UrlNr].FilePos := fOut.FileSize;
        UrlIndex[UrlNr].CompressedSize := DestBytes;
        fOut.Write(OutBuf[0], DestBytes);
        Inc(UrlNr);
    end;

    fOut.Seek(0);
    fOut.Write(UrlIndex[0], Length(UrlIndex) * SizeOf(TUrlIndex));
    fOut.Close;
    fOut.Free;
    fIn.Close;
    fIn.Free;
end;



end.

