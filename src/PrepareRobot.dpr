program PrepareRobot;

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

{
  Dieses Tool dient dazu aus der URL-Datenbank die URLs zu extrahieren,
  die vom Robot bearbeitet werden sollen. Seine zweite Aufgabe ist
  dafuer zu sorgen, daß die Seiten nach einer bestimmten Zeit zur
  Überprüfung vorgemerkt werden. Weitere Details können Sie der
  Benutzerdokumentation entnehmen
}

uses
    // Windows,
    SysUtils,
    Classes,
    CacheFile in 'CacheFile.pas',
    DbTypes in 'DbTypes.pas',
    Hash in 'Hash.pas',
    GlobalTypes in 'GlobalTypes.pas',
    FileLocation in 'FileLocation.pas',
    Config in 'Config.pas',
    UrlDatabase in 'UrlDatabase.pas';

const
    cMaxHosts = 10000; { Maximalanzahl der zu sperrenden Verzeichnisse oder Hosts }
    cMaxHostHash = 16 * 1024 * 1024 - 1;

type
    pEntry = ^tEntry; { Datenstruktur in der die einzelnen URLs zwischengespeichert werden }

    tEntry = record
        Next: pEntry;
        Url: shortstring;
    end;

    pString = ^shortstring;

    pHostList = ^tHostList; { Datenstruktur für die Zählung der Urls pro Host }

    tHostList = record
        Next: pHostList;
        An: integer;
        Name: shortstring;
    end;

    pPathIg = ^tPathIg; { Datenstrukur für die Liste mit zu sperrenden Pfaden }

    tPathIg = record
        Next: pPathIg;
        Path: shortstring;
    end;

var
    Entries: array [0 .. cMaxHostHash] of pEntry; { Hash-Table zur Speicherung der URLs }
    HostList: array [0 .. cMaxHostHash] of pHostList;
    { Hash-Table zur Speicherung der Anzahl der einzelen URLs pro Host }
    ThisPriority: tPriority;
    Count: integer; { Anzahl der bisher gesammelten URLs }
    ThisCount: integer;
    UrlList: TextFile; { Ausgabedatei der URL-Liste }
    MaxUrl: integer; { Maximale Anzahl der URLs, die in der Liste gespeichert werden sollen }
    Domains: array [1 .. cMaxHosts] of pString; { Gesperrte Hosts }
    DomainsAn: integer; { Anzahl der gesperrten Hosts }
    PathIg: array [0 .. 16383] of pPathIg; { Gesperrte Pfade }
    NewPathIg: pPathIg; { Anzahl der gesperrten Pfade }
    MaximumPathDepth: integer; // Used to restrict URLs to a maximum path-depth
    MaxUrlsPerHost: integer;
    DACH_MaxUrlsPerHost: integer;
    CountMaxUrlsPerPart: boolean;
    StartShard, EndShard, ShardMask: int32;
    MaxPerDb: int64;
    DAChOnly: boolean;
    DoExportUrls: boolean;
    DoListShards: boolean;



function InDbRange(DbNr: int32):boolean;
begin
    Result :=
        ((DbNr and ShardMask) >= StartShard) and
        ((DbNr and ShardMask) <= EndShard);
end;



function PathDepth(const Url: shortstring): integer;
// Calculates the number of path-elements of a given URL. For example
// "www.acoon.de/" has a path-depth of 1, "www.acoon.de/addurl.asp" has a
// path-depth of 2 as does "www.acoon.de/test/". "www.acoon.de/test/test.html"
// would have a path-depth of 3.
var
    i: integer;
    SlashCount, LastSlash: integer;
begin
    SlashCount := 0;
    LastSlash := -1;
    for i := 1 to Length(Url) do
    begin
        if Url[i] = '/' then
        begin
            Inc(SlashCount);
            LastSlash := i;
        end;
    end;
    if SlashCount = 0 then Result := 1
    else
    begin
        if LastSlash < Length(Url) then Inc(SlashCount);
        Result := SlashCount;
    end;
end;


procedure AddEntry(const s: shortstring; UrlPo, InLinkCount: int64; IsDACH: boolean);
{ Eine URL in die interne URL-Liste aufnehmen, falls die Maximalzahl von URLs für diesen Host noch nicht erreicht sind. }
var
    p: pEntry;
    HashCode: uint32;
    HostName: shortstring;
    p2: pHostList;
    s2: shortstring;
    SLD: shortstring;
    //Dots, i: integer;
begin
    if s = '' then exit;


    // We do NOT want to crawl the update-history of wikipedia
    s2 := LowerCase(s);
    if (Pos('.wikipedia.org',s2)>0) and (Pos('curid=',s2)>0) then exit;


    if Pos(#255, s) > 0 then exit; { URLs die ASCII 255 enthalten dürfen hier nicht weiter }
    if Pos('/', s) > 0 then { Einen "/" sollten eigentlich alle URLs enthalten, aber sicher ist sicher }
    begin
        HostName := LowerCase(copy(s, 1, Pos('/', s) - 1)); { Wir wollen nur den Hostnamen komplett in Kleinbuchstaben }

        SLD := HostName;
        (*
            This code was used to reduce the hostname to a 2nd-level
            domain. I *think* it's better to use the hostname instead.
            TODO: This should be completely removed once it is clear
            that this change is actually correct.
        repeat
            Dots := 0;
            for i := 1 to Length(SLD) do
                if SLD[i] = '.' then Inc(Dots);
            if Dots > 1 then Delete(SLD, 1, Pos('.', SLD));
        until Dots <= 2;
        *)

        HashCode := CalcCRC(SLD) and cMaxHostHash; { HashCode für den Hostnamen bilden }
        p2 := HostList[HashCode]; { Zum Anfang der Liste für diesen HashCode }

        while p2 <> nil do { Die ganze Liste durchgehen }
        begin
            if p2^.Name = SLD then break { HostName gefunden, raus aus der Schleife }
            else p2 := p2^.Next; { Nicht gefunden, also zum nächsten Listeneintrag }
        end;

        if p2 = nil
        then { p2=nil bedeutet der Hostname ist noch nicht in der Liste, also neuen Listeneintrag vornehmen }
        begin
            GetMem(p2, SizeOf(tHostList) - 255 + Length(SLD)); { Speicher für den Listeneintrag anfordern }
            p2^.Next := HostList[HashCode];
            { Next-Zeiger auf bisherigen Anfang der Liste. Das neue Element wird also vor die bisherige Liste gesetzt }
            p2^.An := 0; { Anzahl der URLs für diesen Hostnamen auf 0 setzen }
            p2^.Name := SLD; { Hostnamen im Listeneintrag speichern }
            HostList[HashCode] := p2; { Listenanfang zeigt jetzt auf das neue Element }
        end;


        // Don't add if there are already too many URLs from this host
        if (not IsDACH) and (p2^.An >= MaxUrlsPerHost) then exit;
        if IsDACH and (p2^.An >= DACH_MaxUrlsPerHost) then exit;



        Inc(p2^.An); { URL-Anzahl für diesen Listeneintrag um 1 erhöhen }
    end;

    // Str(InLinkCount: 10, s2);
    // s2 := s2 + ' ' + s; // REMOVE THIS FOR PRODUCTION !!!!!
    s2 := s;

    Inc(Count); { Gesamtzahl der URLs um 1 erhöhen }
    Inc(ThisCount);
    HashCode := CalcCRC(s2) and cMaxHostHash; { HashCode für die URL berechnen }
    s2 := s2 + #255 + IntToStr(UrlPo);
    { Position der URL in der Datenbank mit einem ASCII 255 als Trennzeichen hinten an die URL anhängen }
                                { Dies dient zum späteren schnellen Zugriff auf den URL-Eintrag }
    GetMem(p, SizeOf(tEntry) - 255 + Length(s2)); { Speicher für diese URL anforden }
    p^.Next := Entries[HashCode]; { Und einen neuen Listeneintrag in der internen URL-Liste anlegen }
    p^.Url := s2;
    Entries[HashCode] := p;
end;


procedure WriteUrls;
{ Speichert die URLs, die sich in der internen Liste befinden, in die Ausgabedatei }
{ Außerdem den Speicher für die interne Liste wieder freigeben }
var
    p, p2: pEntry;
    p3: pHostList;
    i: integer;
begin
    for i := 0 to cMaxHostHash do { Die gesamte Hash-Table durchgehen }
    begin { Dies bewirkt, daß die Reihenfolge der URLs durcheinandergewürfelt wird, was dann wieder }
          { bewirkt, daß nicht zu viele Zugriffe in zu kurzer Reihenfolge auf denselben Host erfolgen }
        p := Entries[i]; { Zum Anfang der Liste }
        while p <> nil do { Und durch die ganze Liste durch }
        begin
            WriteLn(UrlList, p^.Url); { Einzelne URL in die Ausgabedatei schreiben }
            p2 := p^.Next;
            FreeMem(p, SizeOf(tEntry) - 255 + Length(p^.Url)); { Speicher für Listeneintrag wieder freigeben }
            p := p2;
        end;
        Entries[i] := nil;
    end;

    if CountMaxUrlsPerPart then
    begin
        for i := 0 to cMaxHostHash do
        begin
            p3 := HostList[i];
            while p3 <> nil do
            begin
                p3^.An := 0;
                p3 := p3^.Next;
            end;
        end;
    end;
end;


function Right(const s: shortstring; Len: integer): shortstring;
{ Gibt die letzten "Len" Zeichen des String "s" zurück }
begin
    Result := LowerCase(copy(s, Length(s) - Len + 1, Len));
end;


procedure DoAdd(Pr: tPriority; s: shortstring);
{ Liest die Datenbank und speichert die URLs mit der
  gewünschten Priorität in der Ausgabedatei }
type
    Str2 = string[2];
    Str3 = string[3];
var
    UrlDb: tCacheFile; { Datei zum Lesen der URL-Datenbank }
    UrlData: tUrlData; { Datenstruktur mit den Angaben für die URL }
    Po, i, k: integer;
    ThisNr: integer;
    DbNr: integer; { Nummer der momentanen Datenbank }
    UrlPo: int64; { Position innerhalb der Datei, an der die URL-Datenstruktur beginnt }
    AcceptNew: boolean;
    s2, s3, s4, s5: string[10];
    sx, Ul: shortstring;
    MaxThisCount: integer;
    TmpFile: tPreloadedFile;
    IsDACH: boolean;
begin
    Write(#13, s, Count);

    for DbNr := 0 to cDbCount -1 do if InDbRange(DbNr) then
    begin
        MaxThisCount := 100000000; // 100 million
        if MaxPerDb <> -1 then
        begin
            TmpFile := tPreloadedFile.Create;
            TmpFile.Assign(cInfDb + IntToStr(DbNr));
            TmpFile.OpenRead;
            MaxThisCount := ((MaxPerDb * 1850) - TmpFile.FileSize) div 1850;
            if MaxThisCount <= 0 then MaxThisCount := 0;
        end;

        (* AssignFile(TmpFile,cInfDb+IntToStr(DbNr));
        Reset(TmpFile,1);
        AcceptNew:=(FileSize(TmpFile)<2147400000) and (FileSize(TmpFile)>=0);
        MaxThisCount:=(2147400000-FileSize(TmpFile)) div SizeOf(tPageInfo);
        MaxThisCount:=1000+Round(1.25*MaxThisCount);
        CloseFile(TmpFile); *)
        AcceptNew := true;

        ThisCount := 0;

        UrlDb := tCacheFile.Create; { Eine Datenbank öffnen }
        UrlDb.Assign(cUrlDb + IntToStr(DbNr));
        UrlDb.Reset;

        UrlDb.Seek((cMaxUrlHash + 1) * 4 + 4); { Die Hash-Table und die Angabe der URL-Anzahl überspringen }
        ThisNr := 0;
        if not AcceptNew then UrlDb.Seek(UrlDb.FileSize);
        while not UrlDb.Eof do { Die ganze Datenbank durchgehen }
        begin
            if Count >= MaxUrl then { Wenn die Maximalzahl an URLs bereits erreicht wurde }
            begin { die bereits gesammelten URLs abspeichern und dann abbrechen }
                WriteLn(#13, s, ' #', DbNr, ': ', Count, ' (', ThisNr, ')');
                UrlDb.Close;
                WriteUrls;
                exit;
            end;
            if ThisCount > MaxThisCount then break;

            UrlPo := UrlDb.FilePos; { Position der URL merken }
            UrlDb.Read(UrlData, SizeOf(UrlData)); { URL-Daten einlesen }

            Ul := LowerCase(UrlData.Url);


            if (UrlData.InfPo = -1) and
            // (UrlData.InLinkCount > 10) and
            (UrlData.Priority <> prIgnore) and
            (copy(Ul, 1, 7) <> 'http:') and { bestimmte URLs ignorieren }
            (Pos(' ', UrlData.Url) = 0) and (Pos(#8, UrlData.Url) = 0) and
            (Pos('/./', UrlData.Url) = 0) and (Pos('..', UrlData.Url) = 0) and
            (Pos('#', UrlData.Url) = 0) and (Pos('http:', UrlData.Url) = 0) and
            // (Pos('/cgi/', UrlData.Url) = 0) and (Pos('/cgi-bin', UrlData.Url) = 0) and
            // (Pos('/cgi-bin-dgpsp/', UrlData.Url) = 0) and (Pos('.pl', UrlData.Url) = 0) and
            // (Pos('.cgi', UrlData.Url) = 0) and (Pos('.phtml', UrlData.Url) = 0) and
            // (Pos('.man', UrlData.Url) = 0) and (Pos('.tcl', UrlData.Url) = 0) and
            // (Pos('/db.s97/', UrlData.Url) = 0) and (* (Pos('?', UrlData.Url) = 0) and *)
            (Pos('javascript:', Ul) = 0) and // (Pos('.idc', UrlData.Url) = 0) and
            (Pos('internal&sk=', UrlData.Url) = 0) and (Pos('(', UrlData.Url) = 0) and
            (Pos('+++', UrlData.Url) = 0) and (Pos('//', UrlData.Url) = 0) and
            (Pos('&title', UrlData.Url) = 0) and (Pos('dbpage.pl', UrlData.Url) = 0) and
            // (Pos('.exe', UrlData.Url) = 0) and (Pos('.dll', UrlData.Url) = 0) and
            (Pos('/cgi-local/', UrlData.Url) = 0) and (Pos('file:', UrlData.Url) = 0) and
            (Pos('hornyhotpanties.com', UrlData.Url) = 0) and
            (Pos('hornyhotpussy.com', UrlData.Url) = 0) and
            (Pos('opera:', Ul) = 0) and
            (Pos('tel:', Ul) = 0) and
            (Pos('/realmedia/', Ul) = 0) and (copy(Ul, 1, 4) <> 'ftp.') and
            (Pos('.htm/', Ul) = 0) and (Pos('.html/', Ul) = 0) and
            (Pos('/cgi-map/', Ul) = 0) and (Pos('"', Ul) = 0) then
            begin
                Po := -1; { Ab hier kommt eine Menge Code, um bestimmte URL-Endungen zu ignorieren }
                        { Dies sind alles Binär-Dateien, die man gar nicht erst zu überprüfen braucht }
                IsDACH := false;


                if UrlHasBlockedExtension(Ul) then Po := 0;

                if Po = -1 then
                begin { Und hier kommt der Anti-Spam Filter gegen die ganzen Sex-Site Anbieter }
                    sx := LowerCase(UrlData.Url);
                    { Im folgenden alle URLs rausfiltern, deren Hostname einen dieser Begriffe enthält }
                    if Pos('/', sx) > 0 then sx := copy(sx, 1, Pos('/', sx) - 1);
                    // if Pos('/doc/sdb/', sx) > 0 then Po := 1;
                    // if Pos('/doc/susehilf/', sx) > 0 then Po := 1;
                    if Pos(#39, sx) > 0 then Po := 1;

                    repeat { Reduce hostname to actual domain - www.acoon.de will be turned into acoon.de }
                        k := 0;
                        for i := 1 to Length(sx) do
                            if sx[i] = '.' then Inc(k);
                        if k > 1 then Delete(sx, 1, Pos('.', sx));
                    until k <= 1;

                    for i := 1 to DomainsAn do { Prüfe ob diese Domain auf der Sperrliste ist }
                        if sx = Domains[i]^ then
                        begin
                            Po := i;
                            break;
                        end;



                    // Reduce Hostname to TLD
                    repeat
                        i := Pos('.', sx);
                        if i > 0 then Delete(sx, 1, i);
                    until i = 0;
                    IsDACH := (sx = 'de') or (sx = 'at') or (sx = 'ch') or
                              (sx = 'nl') or (sx = 'dk');
                    // .nl and .dk added by popular demand (1 person) :)

                    if DAChOnly then
                    begin
                        if not IsDACH then Po := 1;
                    end
                    else
                    begin
                        // Block some *spammy* TLDs
                        if (sx = 'science') or (sx = 'faith') or (sx = 'date') or
                            (sx = 'loan') or (sx = 'review') or (sx = 'party') or
                            (sx = 'win') or (sx = 'club') or (sx = 'webcam') or
                            (sx = 'cricket') then Po := 1;
                    end;
                end;

                ThisPriority := Pr;

                if (UrlData.InfPo = -1) and (not AcceptNew) then
                    Po := 1; { Wenn INF-Dateigröße zu groß, keine neuen Seiten
                    mehr zulassen, es sei denn, es sind AddURLs }

                if Po = -1 then
                begin
                    sx := LowerCase(UrlData.Url); { Prüfe ob diese Pfad-Angabe auf der Sperrliste ist }
                    i := CalcCRC(copy(sx, 1, Pos('/', sx) - 1)) and 16383;
                    NewPathIg := PathIg[i];
                    while NewPathIg <> nil do
                    begin
                        if copy(sx, 1, Length(NewPathIg^.Path)) = NewPathIg^.Path then
                        begin
                            Po := 1;
                            break;
                        end;
                        NewPathIg := NewPathIg^.Next;
                    end;
                end;


                if (Po = -1) and (Pos('://', UrlData.Url) = 0) then
                begin
                    if // (UrlData.InfPo = -1) and
                    (PathDepth(UrlData.Url) <= MaximumPathDepth) then
                    begin
                        AddEntry(UrlData.Url, UrlPo, UrlData.InLinkCount, IsDACH);
                    end;
                end;
            end;
            Inc(ThisNr);
            if (ThisNr and 1023) = 0 then
                Write(#13, s, ' #', DbNr, ': ', Count, ' (', ThisNr, ')');
            { regelmäßig den Fortschritt auf dem Bildschirm anzeigen }
        end;

        WriteLn(#13, s, ' #', DbNr, ': ', Count, ' (', ThisNr, ')');
        WriteUrls; { Gesammelte URLs in die Liste schreiben }

        UrlDb.Close; { Diese Datenbank schließen }
    end;
end;


procedure HandleExUrls;
{ Alle URLs, die nicht als Ignore markiert sind in die urls.txt schreiben }
var
    f: tCacheFile; { Datei zum Lesen der Daten }
    fOut: TextFile;
    An, Po: integer;
    UrlData: tUrlData; { Datenstruktur für die URL }
    DbNr: integer;
    TotalWritten: integer;
begin
    TotalWritten := 0;
    AssignFile(fOut, cUrls);
    ReWrite(fOut);

    for DbNr := 0 to cDbCount - 1 do if InDbRange(DbNr) then
    begin
        f := tCacheFile.Create; { Datenbank zum Lesen öffnen }
        f.Assign(cUrlDb + IntToStr(DbNr));
        f.Reset;
        Po := (cMaxUrlHash + 1) * 4 + 4; { Startposition festlegen, Hash-Table und Angabe der URL-Anzahl überspringen }
        An := 0; { Bisher 0 URLs überprüft }

        while not f.Eof do { Die ganze Datei durchlesen }
        begin
            f.Seek(Po);
            f.Read(UrlData, SizeOf(UrlData)); { URL-Daten lesen }
            Po := f.FilePos;

            if UrlData.Priority <> prIgnore then
            begin
                Inc(TotalWritten);
                WriteLn(fOut, UrlData.Url);
            end;

            Inc(An); { Zähler für gecheckte URLs um 1 erhöhen }
            if (An and 255) = 0 then Write(#13, TotalWritten, '/', An); { regelmäßig Fortschritt anzeigen }
        end;
        WriteLn(#13, TotalWritten, '/', An); { Und am Ende nochmal die Gesamtstatistik }

        f.Close; { Die beiden Dateien wieder schließen }
        f.Free;
    end;
    CloseFile(fOut);

    halt;
end;


procedure ShowHostCount;
var
    i: integer;
    p: pHostList;
begin
    for i := 0 to cMaxHostHash do
    begin
        p := HostList[i];
        while p <> nil do
        begin
            if p^.An > 1000 then WriteLn(p^.Name, '=', p^.An);
            p := p^.Next;
        end;
    end;
end;



procedure ShowUsage;
begin
    WriteLn;
    WriteLn('Usage: PrepareRobot [export] {Options}');
    WriteLn;
    WriteLn('Options are NOT case-sensitive.');
    WriteLn;
    WriteLn('-s --StartDb --StartWith --StartShard <Number>');
    WriteLn('    Sets the shard-number with which to begin.');
    WriteLn;
    WriteLn('-e --EndDb --EndWith --EndShard <Number>');
    WriteLn('    Sets the shard-number with which to end.');
    WriteLn;
    WriteLn('-m --DbMask --ShardMask <Number>');
    WriteLn('    Sets the shard-mask which is used to determine which shards');
    WriteLn('    get processed. Shards are processed if:');
    WriteLn('    (shard-number AND shard-mask) is in range StartShard...EndShard');
    WriteLn;
    WriteLn('-l --ListShards');
    WriteLn('    This will only list the shard-numbers which would be processed');
    WriteLn('    without actually doing anything. This option is useful if you');
    WriteLn('    want to test a combination of start/end shard-numbers with a');
    WriteLn('    shard-mask.');
    WriteLn;
    WriteLn('-d --DeOnly --DAChOnly');
    WriteLn('    If set, then only URLs from .de, .at and .ch domains get');
    WriteLn('    queued for the robot.');
    WriteLn;
    WriteLn('-f --DACHFactor <Factor>');
    WriteLn('    In deusu.config[.default] there is a setting "robot.MaxUrlsPerHost"');
    WriteLn('    which defines how many URLs of a single host are allowed to be crawled.');
    WriteLn;
    WriteLn('    With this parameter you can set a different value for hosts from');
    WriteLn('    .de, .at and .ch domains. The value for this parameter is used as a');
    WriteLn('    multiplier to the setting in deusu.config. So if for example in');
    WriteLn('    deusu.config robot.MaxUrlsPerHost is set to 4 and with this parameter');
    WriteLn('    you set a factor of 5, then for DACH-domains the limit will be');
    WriteLn('    20 URLs per host.');
    WriteLn;
    WriteLn('    Note that this setting is independent of the --DACHOnly option.');
    WriteLn;
    halt;
end;



procedure ListShards;
var
    i,j: integer;
begin
    WriteLn('Shards that would be processed:');
    j := 0;
    for i := 0 to cDbCount - 1 do
        if InDbRange(i) then
        begin
            Write(i:4,' ');
            Inc(j);
            if (j and 7) = 0 then WriteLn;
        end;

    if (j and 7) <> 0 then WriteLn;
    halt;
end;



procedure CheckOptions;
var
    i: integer;
    s: UTF8String;
begin
    DoExportUrls := false;
    DoListShards := false;
    StartShard := 0;
    EndShard := cDbCount-1;
    ShardMask := cDbCount -1;

    i := 1;
    while i <= ParamCount do
    begin
        s := LowerCase(ParamStr(i));
        Inc(i);

        if (s = '-h') or (s = '--help') then
            ShowUsage // Halts further execution

        else if (s = '-s') or (s = '--startdb') or (s = '--startwith') or (s = '--startshard') then
        begin
            StartShard := StrToIntDef(ParamStr(i), 0);
            Inc(i);
            if StartShard < 0 then StartShard := 0;
            if StartShard > (cDbCount - 1) then StartShard := cDbCount - 1;
        end

        else if (s = '-e') or (s = '--enddb') or (s = '--endwith') or (s = '--endshard') then
        begin
            EndShard := StrToIntDef(ParamStr(i), cDbCount - 1);
            Inc(i);
            if EndShard < 0 then EndShard := 0;
            if EndShard > (cDbCount - 1) then EndShard := cDbCount - 1;
        end

        else if (s = '-m') or (s = '--dbmask') or (s = '--shardmask') then
        begin
            ShardMask := StrToIntDef(ParamStr(i), cDbCount - 1);
            Inc(i);
            if ShardMask < 1 then ShardMask := 1;
            if ShardMask > (cDbCount - 1) then ShardMask := cDbCount - 1;
        end

        else if (s = '-d') or (s = '--deonly') or (s = '--dachonly') then
            DAChOnly := true

        else if (s = '-l') or (s = '--listshards') then
            DoListShards := true

        else if (s = '-f') or (s = '--dachfactor') then
        begin
            DACH_MaxUrlsPerHost := MaxUrlsPerHost * StrToIntDef(ParamStr(i), 1);
            Inc(i);
            if DACH_MaxUrlsPerHost < 1 then DACH_MaxUrlsPerHost := 1;
            if DACH_MaxUrlsPerHost > 10000 then DACH_MaxUrlsPerHost := 10000;
        end

        else if s = 'export' then
            DoExportUrls := true

        else begin
            WriteLn('Unknown commandline option "', ParamStr(i - 1), '"');
            WriteLn;
            ShowUsage; // Halts further execution
        end;

    end;
end;



var
    HashCode: integer; { Temporäre Variable zur HashCode Berechnung }
    fIn: TextFile; { Wird zum Lesen der Ignore-Liste benutzt }
    s: string; { Ein paar Strings zur allgemeinern Verwendung }
    i: integer;

begin
    WriteLn('PrepareRobot ', cVersionCopy);
    WriteLn(cGPLNotice);
    WriteLn;

    MaxUrlsPerHost := Config.ReadIntegerDefault('robot.MaxUrlsPerHost', 5);
    if MaxUrlsPerHost < 1 then MaxUrlsPerHost := 1;
    if MaxUrlsPerHost > 10000 then MaxUrlsPerHost := 10000;
    DACH_MaxUrlsPerHost := MaxUrlsPerHost;

    CountMaxUrlsPerPart :=
    LowerCase(Config.ReadString('robot.CountMaxUrlsPerPart')) = 'true';

    CheckOptions;
    if DoListShards then ListShards; // Halts the program when done
    if DoExportUrls then HandleExUrls; // Halts the program when done

    MaximumPathDepth := 9999; // Allow all URLs by default
    (*if ParamCount = 2 then
    begin
        MaximumPathDepth := StrToIntDef(ParamStr(2), 9999);
        if MaximumPathDepth < 1 then MaximumPathDepth := 1;
        if MaximumPathDepth <> 9999 then
            WriteLn('Maximum path depth set to ', MaximumPathDepth);
    end;*)

    MaxPerDb := -1;
    (*if (ParamCount >= 4) and (copy(ParamStr(4),1,1)<>'-') then
    begin
        MaxPerDb := StrToIntDef(ParamStr(4), 500 * 1000);
        if MaxPerDb < 1000 then MaxPerDb := 1000;
        if MaxPerDb > (100 * 1000 * 1000) then MaxPerDb := 100 * 1000 * 000;
    end;*)

    FillChar(Entries, SizeOf(Entries), 0);
    if FileExists(cIgnoreHosts) then
    begin
        AssignFile(fIn, cIgnoreHosts); { Liste mit gesperrten Hosts und Pfaden einlesen }
        Reset(fIn);
        DomainsAn := 0;
        FillChar(PathIg, SizeOf(PathIg), 0);
        while not Eof(fIn) do
        begin
            ReadLn(fIn, s);
            if Length(s) > 255 then s := copy(s, 1, 255);
            if s <> '' then
            begin
                if copy(s, 1, 1) = '@' then { Gesperrter Host }
                begin
                    Delete(s, 1, 1);
                    Inc(DomainsAn);
                    GetMem(Domains[DomainsAn], Length(s) + 1);
                    Domains[DomainsAn]^ := LowerCase(s);
                end
                else
                begin { Gesperrter Pfad }
                    s := LowerCase(s);
                    HashCode := CalcCRC(copy(s, 1, Pos('/', s) - 1)) and 16383;
                    New(NewPathIg);
                    NewPathIg^.Next := PathIg[HashCode];
                    NewPathIg^.Path := s;
                    PathIg[HashCode] := NewPathIg;
                end;
            end;
        end;
        CloseFile(fIn);
    end;

    AssignFile(UrlList, cUrlList); { Datei mir URL-Liste anlegen }
    ReWrite(UrlList);

    Count := 0;
    MaxUrl := 100*1000*1000; { Maximalanzahl der URLs lesen, Default 100 Mio }
    for i := 0 to cMaxHostHash do { Liste mit Hostnamen initialisieren }
        HostList[i] := nil;

    DoAdd(prNormal, 'Normal'); { Dann alle anderen }

    CloseFile(UrlList); { wir sind fertig }

    ShowHostCount;

end.
