unit rwi;

(*
    DeuSu - An OpenSource Internet-Search-Engine
    This file Copyright (C) 2015 Michael Schoebel

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



procedure CompressRWI(InFile, OutFile: string); overload;
function ReadCompressedDocumentID(
    f: tCacheFile;
    PreviousDocumentId: uint32):int32;





implementation

uses
    DbTypes,
    Hash;


const
    cMaxTempPages = 32768;





function ReadCompressedDocumentID(
    f: tCacheFile;
    PreviousDocumentId: uint32):int32;
var
    Data: int32;
    Offset: int32;
    ShiftAmount: integer;
    b: byte;
begin
    f.Read(b, 1);
    Data := b and 7;
    Offset := (b shr 3) and 15;

    ShiftAmount := 4;

    while (b and 128) > 0 do
    begin
        f.Read(b, 1);
        Offset := Offset or ((b and 127) shl ShiftAmount);
        Inc(ShiftAmount, 7);
    end;

    Result := Data or ((PreviousDocumentId + Offset) shl 3);
end;





procedure CompressKeyword(
    fIn: tPreloadedFile;
    fOut: tBufWriteFile;
    var HashOut: array of uint32;
    Keyword: shortstring;
    DocumentCount: uint32;
    TopHitsPointer: int64);
var
    HashCode: uint32;
    PreviousPosition: uint32;
    Buf: array[1..cMaxTempPages] of uint32;
    ThisCount: uint32;
    b: byte;
    Data, DocumentID, Offset, PreviousDocumentId: uint32;
    i: int32;
    THP32: int32;
begin
    HashCode := (CalcCRC(Keyword) shr 6) and cCompressedMaxIndexHash;
    PreviousPosition := HashOut[HashCode];
    HashOut[HashCode] := fOut.FileSize;

    fOut.Write(Keyword, Length(Keyword)+1);

    fOut.Write(DocumentCount, SizeOf(DocumentCount));

    THP32 := TopHitsPointer;
    fOut.Write(THP32, SizeOf(THP32));

    fOut.Write(PreviousPosition, SizeOf(PreviousPosition));

    PreviousDocumentId := 0;
    while DocumentCount <> 0 do
    begin
        ThisCount := DocumentCount;
        if ThisCount > cMaxTempPages then ThisCount := cMaxTempPages;

        fIn.Read(Buf, ThisCount*4);
        for i := 1 to ThisCount do
        begin
            Data := Buf[i];

            // Store the ranking-information in bits 0-2
            b := Data and 7;

            DocumentID := Data shr 3;
            Offset := DocumentID - PreviousDocumentId;
            PreviousDocumentId := DocumentID;

            // Extract the lowest 4 bits of the DocumentID-Offset
            // and put them in bits 3-6
            b := b or ((Offset and 15) shl 3);
            Offset := Offset shr 4;

            // Set bit 7 if we need more bits to store the
            // DocumentID-Offset
            if Offset > 0 then b := b or 128;

            fOut.Write(b, 1);

            while Offset > 0 do
            begin
                // Put the next 7 bits of the DocumentID-Offset
                // in bits 0-6
                b := Offset and 127;
                Offset := Offset shr 7;

                // Set bit 7 if we need more bits to store the
                // DocumentID-Offset
                if Offset > 0 then b := b or 128;

                fOut.Write(b, 1);
            end;

        end;

        Dec(DocumentCount, ThisCount);
    end;
end;





procedure CompressRWI(
    fIn: tPreloadedFile;
    fOut: tBufWriteFile); overload;
var
    HashIn: array of uint64;
    HashOut: array of uint32;
    i: integer;
    Keyword: shortstring;
    DocumentCount: uint32;
    TopHitsPointer: int64;
    StartPo, Po, i64: int64;
begin
    SetLength(HashIn, cMaxIndexHash+1);
    fIn.Seek(0);
    fIn.Read(HashIn[0], Length(HashIn)*8);

    SetLength(HashOut, cCompressedMaxIndexHash+1);
    for i := 0 to High(HashOut) do
        HashOut[i] := 0;
    fOut.Seek(0);
    fOut.Write(HashOut[0], Length(HashOut)*4);

    // Without this the compiler would later complain
    // that "Keyword" is not initialized
    Keyword := '';


    for i := 0 to High(HashIn) do
    begin
        Po := HashIn[i];

        while Po <> 0 do
        begin
            fIn.Seek(Po);
            fIn.Read(Keyword[0], 1);
            if Length(Keyword) > 0 then
            begin
                fIn.Read(Keyword[1], Length(Keyword));
                fIn.Read(DocumentCount, SizeOf(DocumentCount));
                fIn.Read(TopHitsPointer, SizeOf(TopHitsPointer));
                StartPo := fIn.FilePos;

                CompressKeyword(
                    fIn, fOut, HashOut,
                    Keyword, DocumentCount, TopHitsPointer);

                i64 := DocumentCount;
                Po := StartPo + i64*4;

            end
            else break;
        end;
    end;

    fOut.Seek(0);
    fOut.Write(HashOut[0], Length(HashOut)*4);
end;





procedure CompressRWI(InFile, OutFile: string);
var
    fIn: tPreloadedFile;
    fOut: tBufWriteFile;
begin
    fIn := tPreloadedFile.Create;
    fIn.Assign(InFile);
    fIn.OpenRead;
    fIn.Preload;

    fOut := tBufWriteFile.Create;
    fOut.Assign(OutFile);
    fOut.ReWrite;


    CompressRWI(fIn, fOut);


    fIn.Close;
    fIn.Free;

    fOut.Close;
    fOut.Free;
end;



end.
