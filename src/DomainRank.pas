unit DomainRank;

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

type
  TDomainRank = class
  strict protected
    RankData: array of pointer;

    procedure AddEntry(ARanking: integer; const ADomain: shortstring);
    function CalcHashCode(const ADomain: shortstring): integer;
    function NormalizeDomain(ADomain: shortstring): shortstring;
  public
    DomainCount: integer;
    LastDomain: shortstring;

    constructor Create;
    destructor Destroy; override;
    function GetDomainRanking(ADomain: shortstring): integer;
  end;



implementation

uses
  Hash,
  SysUtils;

const
  cHashElements = 1024*1024; // MUST (!) be a power of 2

type
  PRankData = ^TRankData;

  TRankData = record
    Next: PRankData;
    Rank: integer;
    Domain: shortstring;
  end;



procedure TDomainRank.AddEntry(ARanking: integer;
  const ADomain: shortstring);
var
  p: PRankData;
  HashCode: integer;
begin
  HashCode := CalcHashCode(ADomain);

  // The following checks if the domain is already in the list and
  // just Exits if that is the case.
  p := RankData[HashCode];
  while p <> nil do
  begin
    if p^.Domain = ADomain then
      exit
    else
      p := p^.Next;
  end;

  GetMem(p, SizeOf(TRankData) - 255 + Length(ADomain));
  p^.Domain := ADomain;
  p^.Rank := ARanking;
  p^.Next := RankData[HashCode];
  RankData[HashCode] := p;
  Inc(DomainCount);
end;



constructor TDomainRank.Create;
var
  f: TextFile;
  Posi: integer;
  s: shortstring;
  i: integer;
begin
  inherited;

  // Initialize and zero the RankData array
  SetLength(RankData, cHashElements);
  for i := 0 to high(RankData) do
    RankData[i] := nil;

  DomainCount := 0;


  AssignFile(f, 'domainrank.txt');
  Reset(f);
  Posi := 0;

  while not eof(f) do
  begin
    ReadLn(f, s);
    s := NormalizeDomain(s);

    AddEntry(Posi, s);

    Inc(Posi);
  end;

  CloseFile(f);
end;



function TDomainRank.NormalizeDomain(ADomain: shortstring): shortstring;
var
  i: integer;
begin
  ADomain := shortstring(LowerCase(string(ADomain)));
  if copy(ADomain, 1, 7) = 'http://' then
      Delete(ADomain, 1, 7);

  if copy(ADomain, 1, 4) = 'www.' then
      Delete(ADomain, 1, 4);

  i := Pos('/', string(ADomain));
  if i > 0 then
    ADomain := copy(ADomain, 1, i-1);

  Result := ADomain;
end;



destructor TDomainRank.Destroy;
var
  i: integer;
  p, Next: PRankData;
begin
  for i := 0 to high(RankData) do
  begin
    p := RankData[i];
    while p <> nil do
    begin
      Next := p^.Next;
      FreeMem(p, SizeOf(TRankData)-255+Length(p^.Domain));
      p := Next;
    end;
  end;

  inherited;
end;



function TDomainRank.GetDomainRanking(ADomain: shortstring): integer;
// Returns -1 if the Domain isn't found in the list.
var
  p: PRankData;
begin
  ADomain := NormalizeDomain(ADomain);
  LastDomain := ADomain;

  p := RankData[CalcHashCode(ADomain)];
  while p <> nil do
  begin
    if p^.Domain = ADomain then
    begin
      Result := p^.Rank;
      exit;
    end;
    p := p^.Next;
  end;

  Result := -1;
end;



function TDomainRank.CalcHashCode(const ADomain: shortstring): integer;
begin
  Result := CalcCRC(ADomain) and high(RankData);
end;



end.
