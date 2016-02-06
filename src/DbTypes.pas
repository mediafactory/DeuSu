unit DbTypes;

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
  GlobalTypes;

(*
  Below you should $DEFINE FULLSCALE_VERSION if you want the regular
  version of DeuSu. This means that the software will use:
  - cDbBit = 7, which means that there will be 128 database shards
  - cMaxUrlLength = 60
  - cMaxTitleLength = 60;
  - cMaxIndexHash = 4*1024*1024-1
  - cMaxUrlHash = ((256*1024*1024) div cDbCount)-1

  If you $UNDEF FULLSCALE_VERSION instead then the software will use
  different values for these constants. You will get software that is
  optimized for a much smaller index that way. In this mode the
  constants mentioned above will have the following values:
  - cDbBit = 2, which means that there will be 4 database shards
  - cMaxUrlLength = 200
  - cMaxTitleLength = 100;
  - cMaxIndexHash = 16*1024-1
  - cMaxUrlHash = ((1024*1024) div cDbCount)-1
*)
{$DEFINE FULLSCALE_VERSION}



const

{$IFDEF FULLSCALE_VERSION}
  cDbBits = 9; // Number of bits for the shard-number
{$ELSE}
  cDbBits = 2;
{$ENDIF}


  // cDbCount = 2^cDbBits !!!
  cDbCount = 1 shl cDbBits; // Number of database-shards


{$IFDEF FULLSCALE_VERSION}
  cMaxUrlLength = 60;
  cMaxTitleLength = 60;
{$ELSE}
  cMaxUrlLength = 200;
  cMaxTitleLength = 100;
{$ENDIF}


  cMaxKeywords = 10000; // Maximum number of keywords per Web-Page


// The following numbers all need to (2^x)-1, otherwise the code will
// *not* work!

{$IFDEF FULLSCALE_VERSION}
  // Size of RWI HashTable for keywords and hostnames
  cMaxIndexHash = 4*1024*1024-1;
  cCompressedMaxIndexHash = 1024*1024-1;

  // Size of the URL hashtable
  cMaxUrlHash = ((256*1024*1024) div cDbCount)-1;
{$ELSE}
  cMaxIndexHash = 16*1024-1;
  cCompressedMaxIndexHash = 16*1024-1;
  cMaxUrlHash = ((1024*1024) div cDbCount)-1;
{$ENDIF}




type
  (*
    prNormal = Everything is fine. This URL can be crawled or has
        already been crawled.
    prIgnore = Ignore this URL. Will be set if the page does not
        exist or if the server returns a 301 or 302 relocation.
  *)
  TPriority = (prNormal, prIgnore);


  TUrlData = packed record // URL data-structure
    Next: int32; // File-position of next entry

    (*
      This is set to -1 while the URL has not been crawled, afterwards
      it points to the position in the info-database where the info
      about this page is stored
    *)
    InfPo: int64;

    (*
      Counts the number of incoming links. While int64 is probably
      bigger than necessary, it's better to be safe than sorry.
    *)
    InLinkCount: int64;

    Priority: TPriority;
    Url: string[cMaxUrlLength];
  end;

  THit = packed record
    PageID: uint32;
    (*
      PageID:
      Bit0=1: Word is in description
      Bit1=1: Word is in title
      Bit2=1: Word is in URL
    *)
  end;

  // All the info for a successfully crawled page
  TPageInfo = packed record
    Url: string[cMaxUrlLength];
    Title: string[cMaxTitleLength];

    // Snippet. Limited to 255 characters.
    // Either from the page-text or from the "description" meta-tag
    Description: shortstring;

    WordCount: uint32;
    Language: int8; // 0=German; -1=Unknown
  end;



implementation



procedure AbortMsg(s: string);
begin
  WriteLn('Internal error: Size of ',s,' is incorrect.');
  halt;
end;



function IsTHitSizeCorrect:boolean;
begin
  Result:= SizeOf(tHit) = 4;
end;



function IsTPageInfoSizeCorrect:boolean;
begin
  Result:= SizeOf(tPageInfo) = (cMaxUrlLength+cMaxTitleLength+263);
end;


begin
  (*
    Some data-structurs need to have fixed sizes. Having the wrong
    sizes could damage the databases. VERY bad karma!!!
    So do some checking to see if they are correct.
  *)
  if not IsTHitSizeCorrect then AbortMsg('THit');
  if not IsTPageInfoSizeCorrect then AbortMsg('TPageInfo');
end.

