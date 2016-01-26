program searchservernew;

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

{$UNDEF DISABLE_RESULT_CACHING}
{$UNDEF DISABLE_FANCY_HITS}

uses
    {$ifdef Unix}
    cthreads,
    cmem,
    {$endif}
    OSWrapper,
    SysUtils,
    Classes,
    IdSocketHandle,
    IdCustomHTTPServer,
    IdHTTPServer,
    IdContext,
    //DomainRank,
    Hash,
    CacheFile,
    GlobalTypes,
    FileLocation,
    DbTypes,
    rwi,
    math,
    SyncObjs,
    Words;



const
    cMaxKeywords = 10; // This defines the maximum number of keywords per query
    cMaxTempPages = 32768; // Size of read-buffer used during query-processing

    cMaxPagesPerShard = 10 * 1000 * 1000;
    // Number of shards is defined in dbtypes.pas -> cDbCount
    // 10 million is enough for 1.28 billion pages when used with the default
    // number of 128 shards. You can set this pretty high without adverse
    // consequences, as the actual memory-allocation is only the amount that
    // is really needed.

    cMaxCachedResults = 2 * 1024 - 1;



type
    tAction = (acSet, acAnd, acNot);

    tCachedResult = record
        Pages: array [1 .. 1000] of integer;
        Values: array [1 .. 1000] of uint16;
        MaxValue, Count: integer;
        Query: shortstring;
    end;

    tRWIWorkChunk = record
        RWIData: array of uint32;
        Action: tAction;
    end;

    pRWIWorkChunk = ^tRWIWorkChunk;

    tServerObject = class
	public
	    procedure IdHTTPServer1CommandGet(
		AContext: TIdContext;
		ARequestInfo: TIdHTTPRequestInfo;
		AResponseInfo: TIdHTTPResponseInfo);
    end;

    tFeatureFlags = record
        EnableCompressedIndex: boolean;
    end;

    tValueArray = array of int32;



var
    ServerObject: tServerObject;
    RankData: array of int32;
    BackLinkData: array of int32;
    MaxBackLinkCount: int64;
    UrlData: array of byte;
    OrgRbs: RawByteString;
    CachedResults: array [0 .. cMaxCachedResults] of tCachedResult;
    CacheUsed: array [0 .. cMaxCachedResults] of boolean;
    ThisQuery: AnsiString;
    ThisIsCachedResult: boolean;
    Ti4, Ti5, Ti6: int64;
    cSData: string;
    MainQS: array [0 .. 2048] of AnsiChar;
    f: TextFile;
    Begriff: AnsiString;
    StartWithNr: integer;
    PreferDe, PreferEn, GerOnly: boolean;
    KeyWordCount: integer;
    KeyWords: array [1 .. cMaxKeywords] of shortstring;
    KeyWordIsHostName: array [1 .. cMaxKeywords] of boolean;
    KeyWordResultCount: array [1 .. cMaxKeywords] of int32;
    KeyWordAction: array [1 .. cMaxKeywords] of tAction;
    BitField, TempBitField: array of uint32;
    Values: array of uint16;
    Html: array [0 .. cDbCount - 1] of tFileStream;
    TempBuf: array [1 .. cMaxTempPages] of int32;
    FilterData: array of byte;
    ShowCount: integer;
    MaxValue: integer;
    ResultCount: integer;
    FilterMask: integer;
    b1, b2, b3, b4, b5, b6, b7: integer;
    Searchs: integer;
    NoResults: integer;
    ValueTable: array [0 .. 65535] of int32;
    KeyDbs, FancyDbs: array [0 .. 63] of tCacheFile;
    BitFieldInitialized: boolean;
    Counter: integer;
    Count: integer;
    RefreshCachesCountdown: integer;
    Ticks: int64;
    MinTicks, MaxTicks: integer;
    CritSec: tCriticalSection;
    QueryPass: integer;
    EarlyAbort: boolean;
    IdHTTPServer1: TIdHTTPServer;
    FirstPath, SecondPath: string;
    FeatureFlags: tFeatureFlags;
    CorpusSize: int64;
    InverseDocumentFrequency: double;
    ValueData: array[0..65535] of tValueArray;
    BackLinkValueArray: array of double;





procedure ResizeValueArray(Value, MinSize: int32);
var
    CurrentSize: int32;
    NewSize: int32;
begin
    CurrentSize := Length(ValueData[Value]);
    NewSize := CurrentSize + 10;
    if NewSize > 1024 then NewSize := 1024;
    SetLength(ValueData[Value], NewSize);
end;



procedure SetValueData(Value, Index, DocId: int32); inline;
begin
    Dec(Index);
    if Index > High(ValueData[Value]) then
        ResizeValueArray(Value, Index+1);

    ValueData[Value][Index] := DocId;
end;



function GetValueData(Value, Index: int32):int32; inline;
begin
    Dec(Index);
    if Index > High(ValueData[Value]) then
        ResizeValueArray(Value, Index+1);

    Result := ValueData[Value][Index];
end;



procedure InitValueArray;
var
    i: int32;
begin
    for i := 0 to 65535 do
    begin
        SetLength(ValueData[i], 10);
    end;
end;



procedure InitBackLinkValueArray;
var
    i: int32;
    ln_mbl: double;
begin
    SetLength(BackLinkValueArray, MaxBackLinkCount+1);
    if MaxBackLinkCount>0 then ln_mbl := ln(MaxBackLinkCount);
    for i := 0 to High(BackLinkValueArray) do
    begin
        if i = 0 then BackLinkValueArray[i] := 1.0
        else BackLinkValueArray[i] := 2500.0 * ln(i) / ln_mbl;
    end;
end;



function GetBackLinkValue(DocID: int32): double; inline;
var
    BackLinkCount: int64;
begin
    BackLinkCount := BackLinkData[DocID];
    Result := BackLinkValueArray[BackLinkCount];
end;



procedure AppendToLog(Txt: string);
var
    f: TextFile;
begin
    FileMode := 2;
    try
        AssignFile(f, cSearchLogFile);
        if FileExists(cSearchLogFile) then
            Append(f)
        else
            ReWrite(f);

        WriteLn(f, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ' + Txt);
        CloseFile(f);
    except
    end;
end;



procedure ResetStatistics;
begin
    Searchs := 0;
    Counter := 0;
    Ticks := 0;
    MinTicks := 3600000;
    MaxTicks := 0;
    NoResults := 0;
end;



procedure AddToMemo(Txt: string);
begin
    exit;
    WriteLn(Txt);
end;



function FindParam(const Name: AnsiString): string;
const
    MaxLen = 255;
var
    ThisName: AnsiString;
    i, j: integer;
    Code1, Code3: integer;
    qs: array [0 .. 2048] of AnsiChar;
    Data, Data2: AnsiString;
begin
    StrCopy(qs, MainQS);
    StrLower(qs);

    i := 0;
    Data := '';
    while qs[i] <> #0 do
    begin
        ThisName := '';
        if qs[i] = '&' then Inc(i);
        while ((qs[i] <> #0) and (qs[i] <> '=')) do
        begin
            ThisName := ThisName + qs[i];
            Inc(i);
        end;

        if Name = ThisName then
        begin
            j := 0;
            if (qs[i] = '=') then Inc(i);
            while ((qs[i] <> #0) and (qs[i] <> '&') and (j < MaxLen)) do
            begin
                Data := Data + qs[i];
                Inc(i);
                Inc(j);
            end;


            Data2 := '';
            while Data <> '' do
            begin
                case Data[1] of
                    'ä', 'Ä':
                        begin
                            Data2 := Data2 + 'ae';
                            Delete(Data, 1, 1);
                        end;
                    'ö', 'Ö':
                        begin
                            Data2 := Data2 + 'oe';
                            Delete(Data, 1, 1);
                        end;
                    'ü', 'Ü':
                        begin
                            Data2 := Data2 + 'ue';
                            Delete(Data, 1, 1);
                        end;
                    'ß':
                        begin
                            Data2 := Data2 + 'ss';
                            Delete(Data, 1, 1);
                        end;
                    '%': if Length(Data) >= 3 then
                        begin
                            Code1 := StrToIntDef('$' + copy(Data, 2, 2), 0);
                            Delete(Data, 1, 3);
                            case Code1 of
                                $C3:
                                    begin
                                        Code3 := StrToIntDef('$' + copy(Data, 2, 2), 0);
                                        Delete(Data, 1, 2);
                                        case Code3 of
                                            $A4, $84: Data2 := Data2 + 'ae';
                                            $B6, $96: Data2 := Data2 + 'oe';
                                            $AC, $9C: Data2 := Data2 + 'ue';
                                            $9F: Data2 := Data2 + 'ss';
                                        else Data2 := Data2 + Chr(Code1) + Chr(Code3);
                                        end;
                                    end;
                                $C4, $E4: Data2 := Data2 + 'ae';
                                $D6, $F6: Data2 := Data2 + 'oe';
                                $DC, $FC: Data2 := Data2 + 'ue';
                                $DF: Data2 := Data2 + 'ss';
                            else Data2 := Data2 + Chr(Code1);
                            end;
                        end
                        else
                        begin
                            Data2 := Data2 + Data[1];
                            Delete(Data, 1, 1);
                        end;
                else
                    begin
                        Data2 := Data2 + Data[1];
                        Delete(Data, 1, 1);
                    end;
                end;
            end;

            Result := Data2;
            exit;
        end;

        while ((qs[i] <> #0) and (qs[i] <> '&')) do
            Inc(i);
    end;
end;



procedure ExtractKeywords;
var
    i: integer;
begin
    for i := 1 to Length(Begriff) do
    begin
        if Begriff[i] in ['"', '/', '&', '*', ',', '+'] then Begriff[i] := ' ';
    end;

    while Pos('  ', Begriff) > 0 do
        Delete(Begriff, Pos('  ', Begriff), 1);
    Begriff := Trim(Begriff);

    KeyWordCount := 0;
    i := 1;

    while i <= Length(Begriff) do
    begin
        while ((i <= Length(Begriff)) and (Begriff[i] = ' ')) do
            Inc(i);

        Inc(KeyWordCount);
        KeyWords[KeyWordCount] := '';

        while ((i <= Length(Begriff)) and (Begriff[i] <> ' ')) do
        begin
            if ((Begriff[i] <> '+') or (KeyWordCount > 1) or
            (KeyWords[KeyWordCount] <> '')) then
                KeyWords[KeyWordCount] := KeyWords[KeyWordCount] + Begriff[i];
            Inc(i);
        end;
        if KeyWordCount >= cMaxKeywords then exit;
    end;
end;



procedure ReadString(var Fi: tFileStream; var s: shortstring);
begin
    Fi.Read(s, 1);
    if Length(s) > 0 then
        Fi.Read(s[1], Length(s));
end;



procedure FindThisHost(const ThisHost: shortstring; Action: tAction);
var
    Hosts: tFileStream;
    HashCode: int32;
    i, Data, ThisValue, NewValue: int32;
    s: shortstring;
    Po, An: int32;
    ThisAn, fd: integer;
begin
    case Action of
        acSet:
            begin
                BitFieldInitialized := true;
                FillChar(BitField[0], Length(BitField)*4, 0);
                FillChar(ValueTable, SizeOf(ValueTable), 0);
            end;
        acAnd:
            begin
                FillChar(TempBitField[0], Length(TempBitField)*4, 0);
            end;
        acNot:
            begin
                FillChar(TempBitField[0], Length(TempBitField)*4, 255);
            end;
    end;

    HashCode := CalcCRC(ThisHost) and cMaxIndexHash;
    Hosts := tFileStream.Create(cSData + 'hosts.idx', fmOpenRead or fmShareDenyNone);

    Hosts.Position := HashCode * 8;
    Hosts.Read(Po, 4);

    while Po <> -1 do
    begin
        Hosts.Position := Po;
        Hosts.Read(Po, 4);
        ReadString(Hosts, s);
        if s = ThisHost then
        begin
            Hosts.Read(An, 4);

            while An > 0 do
            begin
                ThisAn := An;
                if ThisAn > cMaxTempPages then ThisAn := cMaxTempPages;
                Hosts.Read(TempBuf, 4 * ThisAn);
                for i := 1 to ThisAn do
                begin
                    Data := TempBuf[i];
                    fd := FilterData[Data shr 3];
                    ThisValue := b1;
                    Inc(ThisValue, (31 - (fd and 31)) * b7);
                    if Action = acSet then
                        Inc(ThisValue, Round(GetBackLinkValue(Data shr 3)));
                    if ThisValue < 1 then ThisValue := 1;
                    if ThisValue > 65535 then ThisValue := 65535;

                    case Action of
                        acSet:
                            begin
                                Inc(ValueTable[ThisValue]);
                                Values[Data shr 3] := ThisValue;
                                Inc(BitField[Data shr 8], 1 shl ((Data shr 3) and 31));
                            end;
                        acAnd:
                            begin
                                if (BitField[Data shr 8] and (1 shl ((Data shr 3) and 31))) > 0 then
                                    NewValue := Values[Data shr 3]
                                else NewValue := 0;

                                Dec(ValueTable[NewValue]);
                                Inc(NewValue, ThisValue);

                                if NewValue > 65535 then NewValue := 65535;


                                Inc(ValueTable[NewValue]);
                                Values[Data shr 3] := NewValue;
                                Inc(TempBitField[Data shr 8], 1 shl ((Data shr 3) and 31));
                            end;
                        acNot:
                            begin
                                Dec(TempBitField[Data shr 8], 1 shl ((Data shr 3) and 31));
                            end;
                    end; { case Action of }
                end; { For-Schleife }

                Dec(An, ThisAn);
            end; { while An>0 }

            case Action of
                acAnd, acNot:
                    begin
                        for i := 0 to High(BitField) do
                            BitField[i] := BitField[i] and TempBitField[i];
                    end; { acAnd, acNot }
            end;

            Hosts.Free;
            exit;
        end
        else
        begin
            if Po = -1 then
            begin
                Hosts.Free;
                if Action = acAnd then
                    FillChar(BitField[0], Length(BitField) * 4, 0);
                exit;
            end;
        end;
    end;

    Hosts.Free;
    if Action = acAnd then
        FillChar(BitField[0], Length(BitField) * 4, 0);
end;



procedure ProcessThisKeyViaAnd(ThisAn: integer);
var
    i: integer;
    Data: integer;
    IndexShl: array [0 .. 31] of integer;
    ThisValue, NewValue, ThisRankValue: integer;
    fd: integer;
    TempBit: integer;
    ThisUrlData: byte;
    HostElements: byte;
    BaseValue: array [0 .. 31] of integer;
begin
    for i := 0 to 31 do
        IndexShl[i] := 1 shl i;

    for i := 0 to 31 do
    begin
        ThisValue := b1;
        if (i and 1) <> 0 then Inc(ThisValue, b2);
        if (i and 2) <> 0 then Inc(ThisValue, b3);
        if (i and 4) <> 0 then Inc(ThisValue, b4);
        if (i and 16) <> 0 then Inc(ThisValue, b5);
        if (i and 8) <> 0 then Inc(ThisValue, b6);
        BaseValue[i] := ThisValue;
    end;

    for i := 1 to ThisAn do
    begin
        Data := TempBuf[i];
        if ((BitField[Data shr 8] and IndexShl[(Data shr 3) and 31]) > 0) then
        begin
            fd := FilterData[Data shr 3];
            ThisValue := BaseValue[(Data and 7) or ((fd shr 3) and 24)];

            if PreferDe and ((fd and 32) <> 0) then Inc(ThisValue, 10);
            if PreferEn and ((fd and 32) = 0) then Inc(ThisValue, 10);
            Inc(ThisValue, (31 - (fd and 31)) * b7);


            ThisRankValue := RankData[Data shr 3] + 1;

            ThisUrlData := UrlData[Data shr 3];
            HostElements := (ThisUrlData and 15) + 1;

            if ThisRankValue = 0 then ThisRankValue := 1000001;

            //Inc(ThisValue, Round(GetBackLinkValue(Data shr 3)));
            ThisValue :=
                Round((1.0 - ThisRankValue * 0.000000027) *
                    ThisValue / HostElements *
                    InverseDocumentFrequency);


            if ThisValue > 65535 then ThisValue := 65535;

            TempBit := TempBitField[Data shr 8];
            NewValue := Values[Data shr 3] + ThisValue;
            if NewValue > 65535 then NewValue := 65535;
            Values[Data shr 3] := NewValue;
            TempBitField[Data shr 8] := TempBit + IndexShl[(Data shr 3) and 31];
        end;

    end; { For }
end;



function FindKeyWordResultCountForHost(ThisKey: shortstring): integer;
begin
    Result := 0;
end;



function FindKeyWordResultCount(ThisKey: shortstring; KeywordNr: integer): integer;
var
    Keys: ^tCacheFile;
    HashCode: integer;
    s: shortstring;
    Po: int64;
    Po32: uint32;
    An: int32;
    TopHitsPointer: int64;
    THP32: int32;
begin
    Result := 0;

    if (LowerCase(copy(ThisKey, 1, 5)) = 'host:') or
    (LowerCase(copy(ThisKey, 1, 5)) = 'site:') then
    begin
        Result := FindKeyWordResultCountForHost(copy(ThisKey, 6, 255));
        exit;
    end;

    if LowerCase(copy(ThisKey, 1, 4)) = 'www.' then
    begin
        Result := FindKeyWordResultCountForHost(ThisKey);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = '.de' then
    begin
        Result := FindKeyWordResultCountForHost('www.' + ThisKey);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 3, 4)) = '.com' then
    begin
        Result := FindKeyWordResultCountForHost('www.' + ThisKey);
        exit;
    end;

    HashCode := CalcCRC(ThisKey);
    Str(HashCode and 63, s);
    Keys := @KeyDbs[HashCode and 63];
    // AppendToLog('Optimizer: Searching for keyword "' + ThisKey + '" in database #' + IntToStr(HashCode and 63));


    if FeatureFlags.EnableCompressedIndex then
        HashCode := (HashCode shr 6) and cCompressedMaxIndexHash
    else
        HashCode := (HashCode shr 6) and cMaxIndexHash;


    if Length(s) < 2 then s := '0' + s;
    FileMode := 0;

    if FeatureFlags.EnableCompressedIndex then
    begin
        Keys.Seek(HashCode * 4);
        Keys.Read(Po32, 4);
        Po := Po32;
    end
    else
    begin
        Keys.Seek(HashCode * 8);
        Keys.Read(Po, 8);
    end;

    Keys.Seek(Po);

    while Po <> 0 do
    begin
        Keys.Read(s, 1);
        if s = '' then
        begin
            break;
        end;
        Keys.Read(s[1], Length(s));
        Keys.Read(An, 4);

        if FeatureFlags.EnableCompressedIndex then
        begin
            Keys.Read(THP32, 4);
            TopHitsPointer := THP32;
        end
        else
            Keys.Read(TopHitsPointer, SizeOf(TopHitsPointer));

        if FeatureFlags.EnableCompressedIndex then Keys.Read(Po32, 4);

        if s = ThisKey then
        begin
            Result := An;
            exit;
        end;

        if FeatureFlags.EnableCompressedIndex then
        begin
            if Po32 = 0 then break;
            Keys.Seek(Po32)
        end
        else Keys.Seek(Keys.FilePos + uint32(An) * 4);
    end;

    Result := 0;
    if Pos(':', ThisKey) > 0 then exit;

    if (LowerCase(copy(ThisKey, 1, 3)) = 'www') or
    (LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = 'com') or
    (LowerCase(copy(ThisKey, Length(ThisKey) - 1, 2)) = 'de') then
    begin
        if LowerCase(copy(ThisKey, 1, 3)) = 'www' then Insert('.', ThisKey, 4);
        if LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = 'com' then Insert('.', ThisKey, Length(ThisKey) - 2);
        if LowerCase(copy(ThisKey, Length(ThisKey) - 1, 2)) = 'de' then Insert('.', ThisKey, Length(ThisKey) - 1);
        begin
            KeyWordIsHostName[KeywordNr] := true;
            KeyWords[KeywordNr] := 'host:' + ThisKey;
        end;
    end;
end;



procedure FindThisKey(ThisKey: shortstring; Action: tAction);
var
    Keys, Fancy: ^tCacheFile;
    UseFancyHits: boolean;
    HashCode: integer;
    s: shortstring;
    An: int32;
    Po: int64;
    Po32: uint32;
    i, ThisAn: integer;
    ThisValue, NewValue: integer;
    Data: integer;
    IndexShl: array [0 .. 31] of integer;
    TempBit: integer;
    fd: integer;
    AllLocations, TitleOnly, UrlOnly: boolean;
    ThisRankValue: integer;
    ThisUrlData: byte;
    HostElements: byte;
    TopHitsPointer: int64;
    THP32, PreviousDocumentID: int32;
    ValueFlags: array[0..255] of integer;
begin
    Ti4 := GetTickCount;
    Ti5 := 0;
    Ti6 := 0;

    for i := 0 to 255 do
    begin
        ThisValue := b1;
        if (i and 1) <> 0 then Inc(ThisValue, b2); // InSnippet
        if (i and 2) <> 0 then Inc(ThisValue, b3); // InTitle
        if (i and 4) <> 0 then Inc(ThisValue, b4); // InUrl
        if (i and 128) <> 0 then Inc(ThisValue, b5); // IsDomainRoot
        if (i and 64) <> 0 then Inc(ThisValue, b6); // Starts with "www."
        ValueFlags[i] := ThisValue;
    end;


    if (LowerCase(copy(ThisKey, 1, 5)) = 'host:') or
    (LowerCase(copy(ThisKey, 1, 5)) = 'site:') then
    begin
        FindThisHost(copy(ThisKey, 6, 255), Action);
        exit;
    end;

    if LowerCase(copy(ThisKey, 1, 4)) = 'www.' then
    begin
        FindThisHost(ThisKey, Action);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 2, 3)) = '.de' then
    begin
        FindThisHost('www.' + ThisKey, Action);
        exit;
    end;

    if LowerCase(copy(ThisKey, Length(ThisKey) - 3, 4)) = '.com' then
    begin
        FindThisHost('www.' + ThisKey, Action);
        exit;
    end;

    AllLocations := true;
    TitleOnly := false;
    UrlOnly := false;
    if LowerCase(copy(ThisKey, 1, 6)) = 'inurl:' then
    begin
        AllLocations := false;
        UrlOnly := true;
        Delete(ThisKey, 1, 6);
    end
    else
    if LowerCase(copy(ThisKey, 1, 8)) = 'intitle:' then
    begin
        AllLocations := false;
        TitleOnly := true;
        Delete(ThisKey, 1, 8);
    end;


    for i := 0 to 31 do
        IndexShl[i] := 1 shl i;

    case Action of
        acSet:
            begin
                BitFieldInitialized := false;
                FillChar(ValueTable, SizeOf(ValueTable), 0);
                if KeyWordCount > 1 then
                begin
                    FillChar(BitField[0], Length(BitField) * 4, 0);
                    BitFieldInitialized := true;
                end;
            end;
        acAnd:
            begin
                FillChar(TempBitField[0], Length(TempBitField) * 4, 0);
            end;
    end;

    Ti5 := GetTickCount - Ti4;
    HashCode := CalcCRC(ThisKey);;
    Str(HashCode and 63, s);
    Keys := @KeyDbs[HashCode and 63];
    Fancy := @FancyDbs[HashCode and 63];

    if FeatureFlags.EnableCompressedIndex then
        HashCode := (HashCode shr 6) and cCompressedMaxIndexHash
    else
        HashCode := (HashCode shr 6) and cMaxIndexHash;


    if Length(s) < 2 then s := '0' + s;
    FileMode := 0;

    if FeatureFlags.EnableCompressedIndex then
    begin
        Keys.Seek(HashCode * 4);
        Keys.Read(Po32, 4);
        Po := Po32;
    end
    else
    begin
        Keys.Seek(HashCode * 8);
        Keys.Read(Po, 8);
    end;
    Keys.Seek(Po);

    PreviousDocumentID := 0;

    while Po <> 0 do
    begin
        Keys.Read(s, 1);
        if s = '' then
        begin
            if Action = acAnd then
                FillChar(BitField[0], Length(BitField) * 4, 0);
            Ti6 := GetTickCount - Ti5 - Ti4;
            exit;
        end;
        Keys.Read(s[1], Length(s));
        Keys.Read(An, 4);

        if FeatureFlags.EnableCompressedIndex then
        begin
            Keys.Read(THP32, 4);
            TopHitsPointer := THP32;
        end
        else
            Keys.Read(TopHitsPointer, SizeOf(TopHitsPointer));

        if FeatureFlags.EnableCompressedIndex then
            Keys.Read(Po32, 4);

        if s = ThisKey then
        begin
            UseFancyHits := (QueryPass = 1) and (TopHitsPointer <> -1);
            if UseFancyHits then
            begin
                Fancy.Seek(TopHitsPointer);
                Fancy.Read(An, 4);
            end;

            if (Action = acSet) and (not BitFieldInitialized) and (An > 0) then
            begin
                FillChar(BitField[0], Length(BitField) * 4, 0);
                BitFieldInitialized := true;
            end;

            if KeyWordCount = 1 then
            begin
                MaxValue := 0;
                BitFieldInitialized := false;
            end;

            while An > 0 do
            begin
                ThisAn := An;
                if ThisAn > cMaxTempPages then ThisAn := cMaxTempPages;

                if UseFancyHits then
                begin
                    Fancy.Read(TempBuf, 4 * ThisAn);
                end
                else
                begin
                    if FeatureFlags.EnableCompressedIndex then
                    begin
                        for i := 1 to ThisAn do
                        begin
                            TempBuf[i] := ReadCompressedDocumentID(
                                Keys^, PreviousDocumentID);
                            PreviousDocumentID := TempBuf[i] shr 3;
                        end;
                    end
                    else
                    begin
                        Keys.Read(TempBuf, 4 * ThisAn);
                    end;
                end;


                if (Action = acAnd) and AllLocations and (FilterMask = 0) then ProcessThisKeyViaAnd(ThisAn)
                else
                begin
                    for i := 1 to ThisAn do
                    begin
                        Data := TempBuf[i];
                        if  (AllLocations or (UrlOnly and ((Data and 4) <> 0)) or
                            (TitleOnly and ((Data and 2) <> 0))) and
                            ((FilterMask = 0) or
                            ((FilterMask <> 0) and ((FilterData[Data shr 3] and FilterMask) <> 0)))
                        then
                        begin

                            if  ((Action = acAnd) and
                                ((BitField[Data shr 8] and IndexShl[(Data shr 3) and 31]) > 0))
                                or (Action <> acAnd) then
                            begin
                                fd := FilterData[Data shr 3];
                                ThisValue := ValueFlags[(Data and 7) or (fd and 192)];


                                if PreferDe and ((fd and 32) <> 0) then Inc(ThisValue, 10);
                                if PreferEn and ((fd and 32) = 0) then Inc(ThisValue, 10);
                                Inc(ThisValue, (31 - (fd and 31)) * b7);

                                ThisRankValue := RankData[Data shr 3] + 1;

                                ThisUrlData := UrlData[Data shr 3];
                                HostElements := (ThisUrlData and 15) + 1;

                                if ThisRankValue = 0 then ThisRankValue := 1000001;

                                Inc(ThisValue, Round(GetBackLinkValue(Data shr 3)));
                                ThisValue :=
                                    Round((1.0 - ThisRankValue * 0.000000027) *
                                        ThisValue / HostElements *
                                        InverseDocumentFrequency);

                                if ThisValue > 65535 then ThisValue := 65535;
                            end;

                            case Action of
                                acSet:
                                    begin
                                        if KeyWordCount > 1 then
                                        begin
                                            Values[Data shr 3] := ThisValue;
                                            Inc(BitField[Data shr 8], IndexShl[(Data shr 3) and 31]);
                                        end
                                        else
                                        begin
                                            Inc(Count);
                                            if ThisValue > MaxValue then MaxValue := ThisValue;
                                            Inc(ValueTable[ThisValue]);
                                            if ValueTable[ThisValue] <= 1024 then
                                            begin
                                                SetValueData(ThisValue, ValueTable[ThisValue], Data shr 3);
                                            end;
                                        end;
                                    end;
                                acAnd:
                                    begin
                                        if (BitField[Data shr 8] and IndexShl[(Data shr 3) and 31]) > 0 then
                                        begin
                                            TempBit := TempBitField[Data shr 8];
                                            NewValue := Values[Data shr 3] + ThisValue;
                                            if NewValue > 65535 then NewValue := 65535;
                                            Values[Data shr 3] := NewValue;
                                            TempBitField[Data shr 8] :=
                                                TempBit + IndexShl[(Data shr 3) and 31];
                                        end;
                                    end;
                                acNot:
                                    begin
                                        BitField[Data shr 8] := BitField[Data shr 8] and
                                            (not IndexShl[(Data shr 3) and 31]);
                                    end;
                            end; { case Action of }
                        end; { Is in Filtermask }
                    end; { For-Loop }
                end;

                Dec(An, ThisAn);
            end; { while An>0 }

            Ti5 := GetTickCount - Ti4;
            if Action = acAnd then
            begin
                TempBit := 0;
                for i := 0 to High(BitField) do
                begin
                    BitField[i] := BitField[i] and TempBitField[i];
                    TempBit := TempBit or BitField[i];
                end;
                if TempBit = 0 then
                begin
                    EarlyAbort := true;
                end;
            end;

            Ti6 := GetTickCount - Ti5 - Ti4;
            exit;
        end;

        if FeatureFlags.EnableCompressedIndex then
        begin
            if Po32 = 0 then
            begin
                if Action = acAnd then
                    FillChar(BitField[0], Length(BitField) * 4, 0);
                Ti6 := GetTickCount - Ti5 - Ti4;
                exit;
            end;
            Keys.Seek(Po32);
        end
        else
            Keys.Seek(Keys.FilePos + uint32(An) * 4);
    end;

    if Action = acAnd then
        FillChar(BitField[0], Length(BitField) * 4, 0);

    Ti6 := GetTickCount - Ti5 - Ti4;
end;



procedure OptimizeQuery;
var
    i: integer;
    Method: (logAnd, logNot);
    ThisKey: shortstring;
    ChangesMade: boolean;

    procedure SwapKeywords(Nr1, Nr2: integer);
    var
        s: string;
        Action: tAction;
        Count: integer;
    begin
        s := KeyWords[Nr1];
        KeyWords[Nr1] := KeyWords[Nr2];
        KeyWords[Nr2] := s;

        Action := KeyWordAction[Nr1];
        KeyWordAction[Nr1] := KeyWordAction[Nr2];
        KeyWordAction[Nr2] := Action;

        Count := KeyWordResultCount[Nr1];
        KeyWordResultCount[Nr1] := KeyWordResultCount[Nr2];
        KeyWordResultCount[Nr2] := Count;

        ChangesMade := true;
    end;


begin
    for i := 1 to KeyWordCount do
    begin
        KeyWordIsHostName[i] := false;
        ThisKey := KeyWords[i];
        Method := logAnd;
        if ThisKey <> '' then
        begin
            case ThisKey[1] of
                '+':
                    begin
                        Method := logAnd;
                        Delete(ThisKey, 1, 1);
                    end;
                '-':
                    begin
                        Method := logNot;
                        Delete(ThisKey, 1, 1);
                    end;
            end;
        end;
        if Method = logAnd then KeyWordAction[i] := acAnd
        else KeyWordAction[i] := acNot;

        KeyWordResultCount[i] := FindKeyWordResultCount(ThisKey, i);
    end;

    repeat
        ChangesMade := false;
        for i := 1 to KeyWordCount - 1 do
        begin
            if (KeyWordAction[i] <> acAnd) and (KeyWordAction[i + 1] = acAnd) then SwapKeywords(i, i + 1);

            if (KeyWordAction[i] = acAnd) and (KeyWordAction[i + 1] = acAnd) and
            (KeyWordResultCount[i + 1] < KeyWordResultCount[i]) then SwapKeywords(i, i + 1);
        end;
    until not ChangesMade;
end;



procedure FindKeys;
var
    i: integer;
    ThisKey: shortstring;
    Method: (logAnd, logNot);
    ThisMax, ThisValue: integer;
    HashCode: integer;
begin
    MaxValue := 1;
    Count := 0;
    EarlyAbort := false;

    ThisQuery := '';
    ThisIsCachedResult := false;
    for i := 1 to KeyWordCount do
        if i = 1 then ThisQuery := KeyWords[i]
        else ThisQuery := ThisQuery + ' ' + KeyWords[i];
    ThisQuery := ThisQuery + #255 + IntToStr(b1) + #255 + IntToStr(b2) + #255 + IntToStr(b3) +
    #255 + IntToStr(b4) + #255 + IntToStr(b5) + #255 + IntToStr(b6) + #255 + IntToStr(b7);

    if FindParam('preferde') <> '' then ThisQuery := ThisQuery + #255 + 'preferde=on';
    if FindParam('preferen') <> '' then ThisQuery := ThisQuery + #255 + 'preferen=on';
    if FindParam('geronly') <> '' then ThisQuery := ThisQuery + #255 + 'geronly=on';

    HashCode := CalcCRC(ThisQuery) and cMaxCachedResults;
    {$IFNDEF DISABLE_RESULT_CACHING}
    if CacheUsed[HashCode] then
    begin
        if CachedResults[HashCode].Query = ThisQuery then
        begin
            Inc(Counter);
            ThisIsCachedResult := true;
            BitFieldInitialized := false;
            Count := CachedResults[HashCode].Count;
            MaxValue := CachedResults[HashCode].MaxValue;
            FillChar(ValueTable, SizeOf(ValueTable), 0);
            ThisMax := Count;
            if ThisMax > 1000 then ThisMax := 1000;
            for i := 1 to ThisMax do
            begin
                ThisValue := CachedResults[HashCode].Values[i];
                Inc(ValueTable[ThisValue]);
                SetValueData(ThisValue, ValueTable[ThisValue], CachedResults[HashCode].Pages[i]);
            end;
            exit;
        end;
    end;
    {$ENDIF}

    if KeyWordCount = 0 then
        FillChar(BitField[0], Length(BitField) * 4, 0);

    OptimizeQuery;

    for i := 1 to KeyWordCount do
    begin
        try
            InverseDocumentFrequency :=
                log10( 11.0 +
                     (CorpusSize + KeyWordResultCount[i] + 0.5) /
                     (KeyWordResultCount[i] + 0.5) );
        except
            InverseDocumentFrequency := 1.0;
        end;

        ThisKey := KeyWords[i];
        Method := logAnd;
        if ThisKey <> '' then
        begin
            case ThisKey[1] of
                '+':
                    begin
                        Method := logAnd;
                        Delete(ThisKey, 1, 1);
                    end;
                '-':
                    begin
                        Method := logNot;
                        Delete(ThisKey, 1, 1);
                    end;
            end;
        end;

        if i = 1 then FindThisKey(ThisKey, acSet)
        else
        begin
            case Method of
                logAnd: FindThisKey(ThisKey, acAnd);
                logNot: FindThisKey(ThisKey, acNot);
            end;
        end;
        if EarlyAbort then break;
    end;
end;



procedure ShowLink(Nr, Value, LfdNr: integer; Li: tStringList);
var
    Url: shortstring;
    s: shortstring;
    DbNr: integer;
    DocID: integer;
    UrlPo: int64;
    PageInfoSize: integer;
begin
    PageInfoSize := 258 + cMaxUrlLength + cMaxTitleLength;
    DocID := Nr;

    DbNr := Nr and (cDbCount - 1);
    UrlPo := Nr shr cDbBits;
    UrlPo := UrlPo * PageInfoSize;

    Html[DbNr].Position := UrlPo;
    Html[DbNr].Read(Url, 1 + cMaxUrlLength);
    Li.Add('url=http://' + Url);

    Html[DbNr].Read(s, 1 + cMaxTitleLength);
    if Trim(s) = '' then s := '(Ohne Titel)'; // "Ohne Titel" is German for "Without a title"
    Li.Add('title=' + s);

    Html[DbNr].Read(s, 256);
    s := Trim(s);
    Li.Add('text=' + s);
    Li.Add('rel=' + IntToStr(Value));

    Li.Add('domainrank=' + IntToStr(RankData[DocID]));
    Li.Add('backlinks=' + IntToStr(BackLinkData[DocID]));
end;



procedure GenResults(Li: tStringList);
var
    Nr: integer;
    i, j, i32: integer;
    Data: integer;
    EndWithNr: integer;
    ThisValue: integer;
    HashCode: integer;
    f: TextFile;
    s: string;
begin
    if BitFieldInitialized then
    begin
        MaxValue := 1;
        FillChar(ValueTable, SizeOf(ValueTable), 0);

        for i := 0 to High(BitField) do
        begin
            if BitField[i] <> 0 then
            begin
                Data := BitField[i];
                i32 := i * 32;

                if (Data and 255) <> 0 then
                begin
                    for j := 0 to 7 do
                    begin
                        if (Data and 1) > 0 then
                        begin
                            Inc(Count);
                            ThisValue := Values[i32];
                            if ThisValue > MaxValue then MaxValue := ThisValue;
                            Inc(ValueTable[ThisValue]);
                            if ValueTable[ThisValue] <= 1001 then
                                SetValueData(ThisValue, ValueTable[ThisValue], i32);
                        end;
                        Data := Data shr 1;
                        Inc(i32);
                    end;
                end
                else
                begin
                    Data := Data shr 8;
                    Inc(i32, 8);
                end;



                if (Data and 255) <> 0 then
                begin
                    for j := 0 to 7 do
                    begin
                        if (Data and 1) > 0 then
                        begin
                            Inc(Count);
                            ThisValue := Values[i32];
                            if ThisValue > MaxValue then MaxValue := ThisValue;
                            Inc(ValueTable[ThisValue]);
                            if ValueTable[ThisValue] <= 1001 then
                                SetValueData(ThisValue, ValueTable[ThisValue], i32);
                        end;
                        Data := Data shr 1;
                        Inc(i32);
                    end;
                end
                else
                begin
                    Data := Data shr 8;
                    Inc(i32, 8);
                end;



                if (Data and 255) <> 0 then
                begin
                    for j := 0 to 7 do
                    begin
                        if (Data and 1) > 0 then
                        begin
                            Inc(Count);
                            ThisValue := Values[i32];
                            if ThisValue > MaxValue then MaxValue := ThisValue;
                            Inc(ValueTable[ThisValue]);
                            if ValueTable[ThisValue] <= 1001 then
                                SetValueData(ThisValue, ValueTable[ThisValue], i32);
                        end;
                        Data := Data shr 1;
                        Inc(i32);
                    end;
                end
                else
                begin
                    Data := Data shr 8;
                    Inc(i32, 8);
                end;



                if (Data and 255) <> 0 then
                begin
                    for j := 0 to 7 do
                    begin
                        if (Data and 1) > 0 then
                        begin
                            Inc(Count);
                            ThisValue := Values[i32];
                            if ThisValue > MaxValue then MaxValue := ThisValue;
                            Inc(ValueTable[ThisValue]);
                            if ValueTable[ThisValue] <= 1001 then
                                SetValueData(ThisValue, ValueTable[ThisValue], i32);
                        end;
                        Data := Data shr 1;
                        Inc(i32);
                    end;
                end;

            end;
        end;
    end;


    if StartWithNr > 991 then Count := 0;
    ResultCount := Count;
    Nr := 0;

    Li.Add('TotalCount=' + IntToStr(Count));
    if (Count = 0) and (QueryPass = 2) then
    begin
        if not ThisIsCachedResult then Inc(NoResults);

        (*
        try
            AssignFile(f, 'noresults.txt');
            if FileExists('noresults.txt') then Append(f)
            else ReWrite(f);
            WriteLn(f, OrgRbs);
            CloseFile(f);
        except
        end;
        *)
    end;

    Li.Add('corpussize=' + IntToStr(CorpusSize));
    for i := 1 to KeyWordCount do
    begin
        Li.Add('keyword=' + KeyWords[i]);
        Li.Add('keywordoccurences=' + IntToStr(KeyWordResultCount[i]));
        Str(log10( 11.0 + (CorpusSize + KeyWordResultCount[i] + 0.5) /
                     (KeyWordResultCount[i] + 0.5) ):8:6,s);
        Li.Add('idf=' + s);
    end;

    EndWithNr := StartWithNr + ShowCount - 1;
    if EndWithNr > Count then EndWithNr := Count;

    Li.Add('StartWith=' + IntToStr(StartWithNr));
    Li.Add('EndWith=' + IntToStr(EndWithNr));



    for ThisValue := MaxValue downto 0 do
    begin
        for i := 1 to ValueTable[ThisValue] do
        begin
            Inc(Nr);
            if (Nr >= StartWithNr) and (Nr <= EndWithNr) then
            begin
                j := GetValueData(ThisValue, i);
                if (QueryPass = 2) or (ResultCount >= 1000) then ShowLink(j, ThisValue, Nr, Li);
            end; { Show this link }
            if Nr >= EndWithNr then break;
        end;
        if Nr >= EndWithNr then break;
    end;

    if (ResultCount >= 1000) or (QueryPass = 2) then
    begin
        if (not ThisIsCachedResult) and (Length(ThisQuery) <= 255) then
        begin
            HashCode := CalcCRC(ThisQuery) and cMaxCachedResults;
            CachedResults[HashCode].Query := ThisQuery;
            CachedResults[HashCode].Count := Count;
            CachedResults[HashCode].MaxValue := MaxValue;
            CacheUsed[HashCode] := true;
            Nr := 0;
            for ThisValue := MaxValue downto 0 do
            begin
                for i := 1 to ValueTable[ThisValue] do
                begin
                    Inc(Nr);
                    CachedResults[HashCode].Pages[Nr] := GetValueData(ThisValue, i);
                    CachedResults[HashCode].Values[Nr] := ThisValue;
                    if Nr >= 1000 then break;
                end;
                if Nr >= 1000 then break;
            end;
        end;
    end;
end;



procedure RefineSearch;
var
    i, j: integer;
    s: shortstring;
begin
    i := 1;
    while i < KeyWordCount do
    begin
        if (KeyWords[i] = 'und') or (KeyWords[i] = 'and') then
        begin
            s := KeyWords[i + 1];
            if copy(s, 1, 1) = '+' then
                Delete(s, 1, 1);
            if copy(s, 1, 1) = '-' then
                Delete(s, 1, 1);
            s := '+' + s;
            KeyWords[i + 1] := s;
            for j := i + 1 to KeyWordCount do
                KeyWords[j - 1] := KeyWords[j];
            Dec(KeyWordCount);
        end
        else if (KeyWords[i] = 'nicht') or (KeyWords[i] = 'not') then
        begin
            s := KeyWords[i + 1];
            if copy(s, 1, 1) = '+' then
                Delete(s, 1, 1);
            if copy(s, 1, 1) = '-' then
                Delete(s, 1, 1);
            s := '-' + s;
            KeyWords[i + 1] := s;
            for j := i + 1 to KeyWordCount do
                KeyWords[j - 1] := KeyWords[j];
            Dec(KeyWordCount);
        end
        else
            Inc(i);
    end;


    i := 1;
    while i <= KeyWordCount do
    begin
        if IsFillWord(KeyWords[i]) then
        begin
            for j := i + 1 to KeyWordCount do
                KeyWords[j - 1] := KeyWords[j];
            Dec(KeyWordCount);
        end
        else
            Inc(i);
    end;
end;



procedure LoadCacheData;
var
    f: tCacheFile;
    i, j: integer;
    NonMinusOne: integer;
    f2: TextFile;
    Key, Value, s: string;
    IndexExtension: string;
    BackLinkTemp: array of int64;
    FilterTemp: array of byte;
    UrlDataTemp: array of byte;
    DomainRankTemp: array of int32;
    DocumentID, ThisDocCount: int32;
begin
    FeatureFlags.EnableCompressedIndex := false;

    try
        AssignFile(f2, cSData + 'features.cfg');
        Reset(f2);

        while not eof(f2) do
        begin
            ReadLn(f2, s);
            i := Pos('=', s);
            if i > 0 then
            begin
                Key := LowerCase(Trim(  copy(s, 1, i-1) ));

                Delete(s, 1, i);
                Value := LowerCase( Trim(s) );

                if (Key = 'enablecompressedindex') and
                    ((Value = 'true') or (Value = 'yes')) then
                begin
                    FeatureFlags.EnableCompressedIndex := true;
                    WriteLn('EnableCompressedIndex=true');
                end;
            end;
        end;

        CloseFile(f2);
    except
    end;



    if FeatureFlags.EnableCompressedIndex then
        IndexExtension := '.cidx'
    else
        IndexExtension := '.idx';



    for i := 0 to 63 do
    begin
        KeyDbs[i] := tCacheFile.Create;
        if i < 10 then
            KeyDbs[i].Assign(cSData + 'keys0' + IntToStr(i) + IndexExtension)
        else
            KeyDbs[i].Assign(cSData + 'keys' + IntToStr(i) + IndexExtension);
        KeyDbs[i].Reset;

        FancyDbs[i] := tCacheFile.Create;
        if i < 10 then
            FancyDbs[i].Assign(cSData + 'fancy0' + IntToStr(i) + '.idx')
        else
            FancyDbs[i].Assign(cSData + 'fancy' + IntToStr(i) + '.idx');
        FancyDbs[i].Reset;
    end;

    NonMinusOne := 0;
    MaxBackLinkCount := 0;
    CorpusSize := 0;

    for i := 0 to cDbCount - 1 do
    begin
        f := tCacheFile.Create;
        f.Assign(cSData + 'filter.dat' + IntToStr(i));
        f.Reset;
        Inc(CorpusSize, f.FileSize);
        f.Close;
        f.Free;
    end;

    SetLength(FilterData, CorpusSize);
    SetLength(RankData, CorpusSize);
    SetLength(UrlData, CorpusSize);
    SetLength(BackLinkData, CorpusSize);
    SetLength(Values, CorpusSize);
    SetLength(BitField, (CorpusSize+31) div 32);
    SetLength(TempBitField, (CorpusSize+31) div 32);

    DocumentID := 0;

    for i := 0 to cDbCount - 1 do
    begin
        f := tCacheFile.Create;
        f.Assign(cSData + 'filter.dat' + IntToStr(i));
        f.Reset;
        ThisDocCount := f.FileSize;
        SetLength(FilterTemp, ThisDocCount);
        f.Read(FilterTemp[0], ThisDocCount);
        for j := 0 to High(FilterTemp) do
            FilterData[j*cDbCount+i] := FilterTemp[j];
        SetLength(FilterTemp, 0);
        f.Close;
        f.Free;


        f := tCacheFile.Create;
        f.Assign(cSData + 'rank.dat' + IntToStr(i));
        f.Reset;
        SetLength(DomainRankTemp, ThisDocCount);
        f.Read(DomainRankTemp[0], f.FileSize);
        for j := 0 to High(DomainRankTemp) do
        begin
            RankData[j*cDbCount+i] := DomainRankTemp[j];
            if RankData[j*cDbCount+i] <> -1 then Inc(NonMinusOne);
        end;
        SetLength(DomainRankTemp, 0);
        f.Close;
        f.Free;


        f := tCacheFile.Create;
        f.Assign(cSData + 'rank2.dat' + IntToStr(i));
        f.Reset;
        SetLength(UrlDataTemp, ThisDocCount);
        f.Read(UrlDataTemp[0], ThisDocCount);
        for j := 0 to High(UrlDataTemp) do
            UrlData[j*cDbCount+i] := UrlDataTemp[j];
        SetLength(UrlDataTemp, 0);
        f.Close;
        f.Free;



        f := tCacheFile.Create;
        f.Assign(cSData + 'backlink.dat' + IntToStr(i));
        f.Reset;
        SetLength(BackLinkTemp, f.FileSize div 8);
        f.Read(BackLinkTemp[0], f.FileSize);
        for j := 0 to High(BackLinkTemp) do
        begin
            if BackLinkTemp[j] > 2147483647 then
                BackLinkData[j*cDbCount+i] := 2147483647
            else
                BackLinkData[j*cDbCount+i] := BackLinkTemp[j];

            if BackLinkData[j*cDbCount+i] > MaxBackLinkCount then
                MaxBackLinkCount := BackLinkData[j*cDbCount+i];
        end;
        SetLength(BackLinkTemp,0);
        f.Close;
        f.Free;

        Inc(DocumentID, ThisDocCount);
    end;


    InitBackLinkValueArray;
end;



procedure CheckDataPath;
var
    NewPath: string;
    i: integer;
    Sr: tSearchRec;
    Ti1: integer;
begin
    NewPath := '';
    if FileExists(FirstPath + 'ready2.dat') and
    (not FileExists(SecondPath + 'ready2.dat')) then NewPath := FirstPath;

    if FileExists(SecondPath + 'ready2.dat') and
    (not FileExists(FirstPath + 'ready2.dat')) then NewPath := SecondPath;

    if FileExists(FirstPath + 'ready2.dat') and
    FileExists(SecondPath + 'ready2.dat') then
    begin
        i := FindFirst(FirstPath + 'ready2.dat', faAnyFile, Sr);
        if i = 0 then
        begin
            Ti1 := Sr.Time;
            FindClose(Sr);
            i := FindFirst(SecondPath + 'ready2.dat', faAnyFile, Sr);
            if i = 0 then
            begin
                if Ti1 > Sr.Time then NewPath := FirstPath
                else NewPath := SecondPath;
            end;
            FindClose(Sr);
        end
        else FindClose(Sr);
    end;

    if NewPath = '' then exit;

    if NewPath <> cSData then
    begin
	WriteLn('Switching to new index in: ', NewPath);
        if cSData <> '' then
        begin
            for i := 0 to 63 do
            begin
                KeyDbs[i].Close;
                KeyDbs[i].Free;
                FancyDbs[i].Close;
                FancyDbs[i].Free;
            end;
        end;

        for i := 0 to cMaxCachedResults do
            CacheUsed[i] := false;

        //Searchs := 0;
        //NoResults := 0;
        //Counter := 0;

        cSData := NewPath;
        LoadCacheData;
    end;
end;



function ParseThisParameter(s: string; Default, Min, Max: integer): integer;
var
    Value: integer;
begin
    if s <> '' then
    begin
        if not TryStrToInt(s, Value) then
            Value := Default;
    end
    else Value := Default;

    if Value < Min then
        Value := Min;

    if Value > Max then
        Value := Max;

    Result := Value;
end;



procedure OpenSnippetDatabases;
var
    i: integer;
begin
    for i := 0 to cDbCount - 1 do
    begin
        Html[i] := tFileStream.Create(cSData + 'urls.dat' + IntToStr(i), fmOpenRead or fmShareDenyNone);
    end;
end;



procedure CloseSnippetDatabases;
var
    i: integer;
begin
    for i := 0 to cDbCount - 1 do
    begin
        Html[i].Free;
    end;
end;



procedure EmptyCache;
var
    i: integer;
begin
    for i := 0 to cMaxCachedResults do
        CacheUsed[i] := false;
end;



procedure SetupQuery(Req: TIdHTTPRequestInfo; Res: TIdHTTPResponseInfo);
var
    Ti, Ti2, Ti3: int64;
    s: string;
    Li: tStringList;
    rbs: RawByteString;
    u8: UTF8String;
    s2: string;
begin
    try
        CritSec.Enter;
        CheckDataPath;


        Ti := GetTickCount;

        b1 := 8 * 256;
        b2 := 2 * 256;
        b3 := 2 * 256;
        b4 := 1 * 256;
        b5 := 1 * 256;
        b6 := 1 * 256;
        b7 := 1; // 1

        b1 := ParseThisParameter(Req.Params.Values['b1'], 8, 0, 32) * 256;
        b2 := ParseThisParameter(Req.Params.Values['b2'], 2, 0, 32) * 256;
        b3 := ParseThisParameter(Req.Params.Values['b3'], 2, 0, 32) * 256;
        b4 := ParseThisParameter(Req.Params.Values['b4'], 1, 0, 32) * 256;
        b5 := ParseThisParameter(Req.Params.Values['b5'], 1, 0, 32) * 256;
        b6 := ParseThisParameter(Req.Params.Values['b6'], 1, 0, 32) * 256;

        b7 := ParseThisParameter(Req.Params.Values['b7'], 1, 0, 32 * 256);
        // Yes, the above line IS CORRECT!


        rbs := Req.Params.Values['q'];
        OrgRbs := rbs;
        u8 := rbs;
        s := AnsiLowerCase(u8);
        Begriff := '';

        while s <> '' do
        begin
            s2 := copy(s, 1, 2);
            if (Length(s2) = 2) and (Ord(s[1]) = 227) then
            begin
                case Ord(s2[2]) of
                    164: Begriff := Begriff + 'ae';
                    182: Begriff := Begriff + 'oe';
                    188: Begriff := Begriff + 'ue';
                    132: Begriff := Begriff + 'ae';
                    150: Begriff := Begriff + 'oe';
                    156: Begriff := Begriff + 'ue';
                    159: Begriff := Begriff + 'ss';
                end;
                Delete(s, 1, 2);
            end
            else
            begin
                Begriff := Begriff + AnsiString(s[1]);
                Delete(s, 1, 1);
            end;
        end;

        FilterMask := 0;
        StartWithNr := ParseThisParameter(Req.Params.Values['startwith'], 1, 1, 991);
        ShowCount := ParseThisParameter(Req.Params.Values['showcount'], 10, 10, 1000);
        PreferDe := true;
        PreferEn := false;
        GerOnly := false;
        if GerOnly then
            FilterMask := FilterMask or 32;


        ExtractKeywords;
        RefineSearch;

        OpenSnippetDatabases;

        Ti2 := GetTickCount;
        QueryPass := 1;
        {$IFNDEF DISABLE_FANCY_HITS}
        FindKeys;
        {$ENDIF}
        Ti3 := GetTickCount;

        Li := tStringList.Create;
        Li.Sorted := false;
        Li.Duplicates := dupAccept;

        {$IFNDEF DISABLE_FANCY_HITS}
        GenResults(Li);
        if ResultCount < 1000 then
        {$ENDIF}
        begin
            QueryPass := 2;
            FindKeys;
            Li.Clear;
            GenResults(Li);
        end;

        Res.ContentType := 'text/html';
        Res.CharSet := 'utf-8';
        Res.ContentText := UTF8Decode(Li.Text);

        Li.Free;
        CloseSnippetDatabases;

        if not ThisIsCachedResult then
        begin
            Inc(Searchs);
            Ti := GetTickCount - Ti;
            if Ti < 0 then
                Ti := 0;
            Ticks := Ticks + Ti;
            if Ti < MinTicks then
                MinTicks := Ti;
            if Ti > MaxTicks then
                MaxTicks := Ti;
            FormatSettings.LongTimeFormat := 'hh:nn:ss';
            Str(Ti: 5, s);
            if Ti >= 0 then
            begin
                AddToMemo(s + 'ms ' + '(' + IntToStr(ResultCount)
                + ') ' + Begriff + '(' + IntToStr(KeyWordCount)
                + ')' + ' - ' + IntToStr(Ti3 - Ti2) + '/' + IntToStr
                (GetTickCount - Ti3) + ' (' + IntToStr(Ti5)
                + '/' + IntToStr(Ti6) + ')');
            end;
        end;

        // ShowQueryStatistics;

        if Ti > 2000 then
            AppendToLog(IntToStr(Ti) + 'ms   Query=' + Begriff);

    finally
        CritSec.Leave;
    end;
end;



procedure ShowAdminRoot(Req: TIdHTTPRequestInfo; Res: TIdHTTPResponseInfo);
var
  Li: tStringList;
  s: string;
begin
  Li := tStringList.Create;
  Li.Sorted := false;
  Li.Duplicates := dupAccept;

  Li.Add('<META HTTP-EQUIV="refresh" CONTENT="300">');
  Li.Add('<body><style>');
  Li.Add('body {');
  Li.Add('font-size: 12px;');
  Li.Add('}');
  Li.Add('</style>');
  Li.Add(cSData + '<br>');

  Li.Add(IntToStr(Searchs)+' of which ' + IntToStr(NoResults) + ' are without result<br/>');

  if Searchs>0 then
  begin
    Str(Ticks / Searchs: 5: 3, s);
    Li.Add(s + 'ms/query<br/>');
  end;

  Str(MaxTicks: 5, s);
  Li.Add(s + 'ms max-query<br/>');

  Str(MinTicks: 5, s);
  Li.Add(s + 'ms min-query<br/>');

  Li.Add('<form action="/admin/shutdown" method="post">');
  Li.Add('<input type="submit" value="Shutdown"></form><br>');

  Res.ContentText := Li.Text;
  Li.Free;
end;



procedure tServerObject.IdHTTPServer1CommandGet(AContext: TIdContext;
    ARequestInfo: TIdHTTPRequestInfo;
    AResponseInfo: TIdHTTPResponseInfo);
begin
    // WriteLn('Command="',ARequestInfo.Command,'"');
    // WriteLn('Document="',ARequestInfo.Document,'"');

    if AnsiLowerCase(ARequestInfo.Document) = '/query.html' then
        SetupQuery(ARequestInfo, AResponseInfo) else

    if AnsiLowerCase(ARequestInfo.Document) = '/admin/' then
      ShowAdminRoot(ARequestInfo, AResponseInfo) else

    if (AnsiLowerCase(ARequestInfo.Command) = 'post') and
      (AnsiLowerCase(ARequestInfo.Document) = '/admin/shutdown') then
        halt;

end;



procedure InitServer;
var
    i: integer;
    s: string;
    Binding: TIdSocketHandle;
begin
    InitValueArray;

    ServerObject := tServerObject.Create;

    IdHTTPServer1:=TIdHTTPServer.Create;
    IdHTTPServer1.Bindings.Clear;
    Binding := IdHTTPServer1.Bindings.Add;
    Binding.IP := '0.0.0.0';
    Binding.Port := 8081;
    IdHTTPServer1.OnCommandGet := ServerObject.IdHTTPServer1CommandGet;


    i := 1;
    while i <= ParamCount do
    begin
        s := LowerCase(ParamStr(i));

        if (s = '-p') or (s = '-port') then
        begin
            Inc(i);
            //IdHTTPServer1.DefaultPort := StrToIntDef(ParamStr(i), 8081);
            Binding.Port := StrToIntDef(ParamStr(i), 8081);
        end

        else if (s = '-path1') or (s='-1') then
        begin
            Inc(i);
            FirstPath := ParamStr(i);
            if FirstPath = '' then FirstPath := cSearchFirstPath;
            if copy(FirstPath,1,Length(FirstPath)) <> DirectorySeparator then
                FirstPath := FirstPath + DirectorySeparator;
        end

        else if (s = '-path2') or (s='-2') then
        begin
            Inc(i);
            SecondPath := ParamStr(i);
            if SecondPath = '' then SecondPath := cSearchSecondPath;
            if copy(SecondPath,1,Length(SecondPath)) <> DirectorySeparator then
                SecondPath := SecondPath + DirectorySeparator;
        end;



        Inc(i);
    end;
    Randomize;
    RefreshCachesCountdown := 0;
    WriteLn('SearchServer ', cVersion);
    WriteLn(cCopyright);

    EmptyCache;
    ResetStatistics;

    cSData := '';
    CheckDataPath;
    if not IdHTTPServer1.Active then IdHTTPServer1.Active := true;
end;



begin
    CritSec := tCriticalSection.Create;
    FirstPath := cSearchFirstPath;
    SecondPath := cSearchSecondPath;

    InitServer;
    while true do Sleep(1000);
end.
