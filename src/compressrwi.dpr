program compressrwi;

uses
  rwi;

begin
  rwi.CompressRWI(ParamStr(1)+'.idx', ParamStr(1)+'.cidx');
end.
