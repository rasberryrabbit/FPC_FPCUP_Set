unit fpcupset_main;

{$mode objfpc}{$H+}


{ $define DEBUG_LAZ_XML}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    EdLaz: TEdit;
    EdFPC: TEdit;
    EdFPCDIR: TEdit;
    FPC: TLabel;
    FPCDIR: TLabel;
    Lazarus: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure str_fpc_cfg(const Source: TStringList; new_path, fpcpath: string);
    procedure Patch_fpc_cfg(const File_Path: string; new_path, fpcpath: string);

    procedure str_lazarus_cfg(const Source: TStringList; new_path: string);
    procedure Patch_lazarus_cfg(const File_Path: string; new_path: string);

    function str_lazarus_xml(const Source: string; new_path: string): string;
    procedure Patch_lazarus_xml(const File_Path: string; new_path: string);

    procedure str_fpcdeluxe_ini(const Source: TStringList; new_path: string);
    procedure Patch_fpcupdeluxe_ini(const File_Path: string; new_path: string);

  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, RegExpr, strutils,
  LazFileUtils, FindFile;

{$R *.lfm}

var
  WorkPath:string;


const
  config_folder='config_lazarus';
  config_fpc='fpc\bin\i386-win32\fpc.cfg';

  lazarus_cfg = 'idemake.cfg';
  lazarus_xml : array[0..8] of string = (
                'codetoolsoptions.xml',
                'editoroptions.xml',
                'environmentoptions.xml',
                'fpcdefines.xml',
                'inputhistory.xml',
                'packagefiles.xml',
                'debuggeroptions.xml',
                'helpoptions.xml',
                'pas2jsdsgnoptions.xml'
                );

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  ffind: TFindFile;
  flist: TStringList;
  i: Integer;
begin
  WorkPath:=ExtractFilePath(ParamStr(0));
  EdLaz.Text:=WorkPath+config_folder;
  EdFPC.Text:=WorkPath+config_fpc;

  // find fpc.cfg
  ffind:=TFindFile.Create(nil);
  try
    ffind.FileMask:='*.cfg';
    ffind.Path:=WorkPath;
    ffind.InSubFolders:=True;
    flist:=ffind.SearchForFiles;
    if flist.Count>0 then begin
      for i:=0 to flist.Count-1 do begin
        if Pos('fpc.cfg',flist[i])>0 then begin
          EdFPC.Text:=flist[i];
          Memo1.Lines.Add('fpc.cfg is found : '+flist[i]);
          break;
        end;
      end;
    end;
  finally
    ffind.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  xml_path, lazcfg_path, fpccfg_path : string;
  i : Integer;
begin
  Memo1.Clear;
  Memo1.Lines.Add('New Path : '+WorkPath);
  // patch fpc cfg file
  fpccfg_path:=EdFPC.Text;
  if FileExistsUTF8(fpccfg_path) then begin
    DeleteFile(fpccfg_path+'.bak');
    Patch_fpc_cfg(fpccfg_path,WorkPath,EdFPCDIR.Text);
    Memo1.Lines.Add('Patched : '+ fpccfg_path);
  end else
    Memo1.Lines.Add('File not found : '+fpccfg_path);

  // patch cfg files
  lazcfg_path:=EdLaz.Text+PathDelim+lazarus_cfg;
  if FileExistsUTF8(lazcfg_path) then begin
    DeleteFile(lazcfg_path+'.bak');
    Patch_lazarus_cfg(lazcfg_path,WorkPath);
    Memo1.Lines.Add('Patched : '+lazarus_cfg);
  end else
    Memo1.Lines.Add('File not found : '+lazcfg_path);

  // patch xml files
  for i:=Low(lazarus_xml) to High(lazarus_xml) do begin
    xml_path:=EdLaz.Text+PathDelim+lazarus_xml[i];
    if FileExistsUTF8(xml_path) then begin
        {$ifndef DEBUG_LAZ_XML} DeleteFile(xml_path+'.bak'); {$endif}
        Patch_lazarus_xml(EdLaz.Text+PathDelim+lazarus_xml[i],WorkPath);
        Memo1.Lines.Add('Patched : '+lazarus_xml[i]);
      end
      else
        Memo1.Lines.Add('File Not Found : '+xml_path);
  end;

  if FileExistsUTF8('fpcupdeluxe.ini') then begin
    DeleteFile('fpcupdeluxe.ini.bak');
    Patch_fpcupdeluxe_ini('fpcupdeluxe.ini',WorkPath);
    Memo1.Lines.Add('Patched : fpcupdeluxe.ini');
  end else
    Memo1.Lines.Add('File Not Found : fpcupdeluxe.ini');
end;

function RemovePreDir(const path: string):string;
var
  i, j, k: Integer;
begin
  Result:='';
  i:=1;
  j:=Length(path);
  k:=0;
  while i<=j do begin
    if path[i] in ['\','/'] then begin
      Inc(k);
      if k=2 then begin
        Result:=Copy(path,i);
        break;
      end;
    end;
    inc(i);
  end;
end;

function IsDirectory(const s:string):Boolean;
var
  i: Longint;
begin
  Result:=False;
  i:=FileGetAttrUTF8(s);
  if i<>-1 then
    Result:=i and faDirectory<>0;
end;

procedure TForm1.Patch_fpc_cfg(const File_Path: string; new_path, fpcpath: string);
var
  Inbuff: TStringList;
begin
  if FileExistsUTF8(File_path) then begin
    InBuff := TStringList.Create;
    try
      Inbuff.LoadFromFile(File_path);
      str_fpc_cfg(Inbuff,new_path,fpcpath);
      RenameFile(File_Path,File_Path+'.bak');
      Inbuff.SaveToFile(File_Path);
    finally
      Inbuff.Free;
    end;
  end;
end;

procedure TForm1.str_fpc_cfg(const Source: TStringList; new_path, fpcpath: string);
const
  ini_pattern = '([a-zA-Z]\:)?[\\/][^\\/]+[\\/][^;\n]+';
var
  i, j, len, np: Integer;
  old_path, txt, head, tail, res: string;
  reg_fpccfg: RegExpr.TRegExpr;
  patched: Boolean;
begin
  new_path:=ExtractFileDir(new_path);
  reg_fpccfg:=TRegExpr.Create(ini_pattern);
  try
    reg_fpccfg.ModifierM:=True;
    if Source.Count>0 then begin
      for i:=0 to Source.Count-1 do begin
        txt:=Source[i];
        len:=Length(txt);
        // if not comment
        if Pos('#',txt)=0 then begin
          if reg_fpccfg.Exec(txt) then begin
            res:='';
            np:=1;
            repeat
              patched:=False;
              // copy unmatched part
              res:=res+Copy(txt,np,reg_fpccfg.MatchPos[0]-np);
              np:=reg_fpccfg.MatchPos[0]+reg_fpccfg.MatchLen[0];

              // check macro
              j:=Pos('$',reg_fpccfg.Match[0]);
              if j>0 then begin
                head:=Copy(reg_fpccfg.Match[0],1,j-1);
                tail:=Copy(reg_fpccfg.Match[0],j);
              end else begin
                head:=reg_fpccfg.Match[0];
                tail:='';
              end;
              if head<>'' then begin
                old_path:=head;
                repeat
                  old_path:=RemovePreDir(old_path);
                  if (Pos(fpcpath,old_path)>2) and (old_path<>'') and (Length(old_path)>1) and
                  ( FileExistsUTF8(new_path+old_path) or
                    IsDirectory(new_path+old_path) )
                  then begin
                    res:=res+new_path+old_path+tail;
                    patched:=True;
                    break;
                  end;
                until old_path='';
                if not patched then
                  res:=res+head+tail;
              end else
                res:=res+head+tail;
            until not reg_fpccfg.ExecNext;
            if np<len then
              res:=res+Copy(txt,np);

            if txt<>res then
              Source[i]:=res;
            //Memo1.Lines.Add(res);
          end;
        end;
      end;
    end;
  finally
    reg_fpccfg.Free;
  end;
end;

procedure TForm1.Patch_lazarus_cfg(const File_Path: string; new_path: string);
var
  Inbuff: TStringList;
begin
  if FileExistsUTF8(File_path) then begin
    InBuff := TStringList.Create;
    try
      Inbuff.LoadFromFile(File_path);
      str_lazarus_cfg(Inbuff,new_path);
      RenameFile(File_Path,File_Path+'.bak');
      Inbuff.SaveToFile(File_Path);
    finally
      Inbuff.Free;
    end;
  end;
end;

procedure TForm1.str_lazarus_cfg(const Source: TStringList; new_path: string);
const
  lazarus_pattern = '([a-zA-Z]\:)?[\\/][^\\/]+[\\/][^;\n]+';
var
  i, len, np: Integer;
  old_path, txt, res: string;
  reg_fpccfg: RegExpr.TRegExpr;
  patched: Boolean;
begin
  new_path:=ExtractFileDir(new_path);

  reg_fpccfg := TRegExpr.Create(lazarus_pattern);
  try
    reg_fpccfg.ModifierM:=True;
    if Source.Count>0 then
    for i:=0 to Source.Count-1 do begin
      txt:=Source[i];
      np:=1;
      len:=Length(txt);
      res:='';

      if reg_fpccfg.Exec(txt) then begin
        repeat
          patched:=False;
          res:=res+Copy(txt,np,reg_fpccfg.MatchPos[0]-np);
          np:=reg_fpccfg.MatchPos[0]+reg_fpccfg.MatchLen[0];

          old_path:=reg_fpccfg.Match[0];
          if FilenameIsAbsolute(old_path) then begin
            repeat
              old_path:=RemovePreDir(old_path);
              if (Length(old_path)>1) and
                 IsDirectory(new_path+old_path) then begin
                  res:=res+new_path+old_path;
                  patched:=True;
                  break;
              end;
            until old_path='';
          end;
          if not patched then
            res:=res+reg_fpccfg.Match[0];
        until not reg_fpccfg.ExecNext;
        if np<len then
          res:=res+Copy(txt,np);
        Source[i]:=res;
      end;
    end;
  finally
    reg_fpccfg.Free;
  end;
end;

procedure TForm1.Patch_lazarus_xml(const File_Path: string; new_path: string);
var
  Child : TDOMNode;
  Doc : TXMLDocument;
  i : Integer;

  procedure ProcessNode(Node: TDOMNode; lvl:Integer);
  var
    cNode: TDOMNode;
    s: string;
    i : Integer;
  begin
    if Node = nil then Exit;
    Inc(lvl);

    // node value
    if Node.NodeValue<>'' then begin
      s:=Node.NodeValue;
      s := str_lazarus_xml(s,new_path);
      if s<>'' then
        Node.NodeValue:=s;
    end;
    // attributes
    if Node.HasAttributes and (Node.Attributes.Length>0) then
      for i:=0 to Node.Attributes.Length-1 do begin
        s:=Node.Attributes[i].NodeValue;
        s := str_lazarus_xml(s,new_path);
        if s<>'' then
          Node.Attributes[i].NodeValue:=s;
      end;

    cNode := Node.FirstChild;

    while cNode <> nil do
    begin
      ProcessNode(cNode, lvl);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  try
  ReadXMLFile(Doc,File_Path);
  Child := Doc.DocumentElement.FirstChild;
  while Child<>nil do begin
    ProcessNode(Child,0);
    Child:=Child.NextSibling;
  end;
  {$ifndef DEBUG_LAZ_XML}
  RenameFile(File_Path,File_Path+'.bak');
  WriteXML(Doc,File_Path);
  {$endif}

  finally
    Doc.Free;
  end;
end;

function TForm1.str_lazarus_xml(const Source: string; new_path: string): string;
const
  FilePath_Pattern = '([a-zA-Z]\:)?[\\/][^;]+';
var
  j, np, len, lvl: Integer;
  str,tail: string;
  reg_fpccfg: RegExpr.TRegExpr;
  old_path: string;
  patched: Boolean;
begin
  Result:='';
  new_path:=ExtractFileDir(new_path);
  reg_fpccfg:=TRegExpr.Create(FilePath_Pattern);
  try
    reg_fpccfg.ModifierM:=True;
    if reg_fpccfg.Exec(Source) then begin
      j:=Pos('$',Source);
      if (j<1) or (j>=reg_fpccfg.MatchPos[0]) then begin
        len:=Length(Source);
        np:=1;
        repeat
          Result:=Result+Copy(Source,np,reg_fpccfg.MatchPos[0]-np);
          np:=reg_fpccfg.MatchPos[0]+reg_fpccfg.MatchLen[0];
          patched:=False;

          str:=reg_fpccfg.Match[0];
          // macro folder
          j:=Pos('$',str);
          if j>0 then begin
            old_path:=Copy(str,1,j-1);
            tail:=Copy(str,j);
          end else begin
            old_path:=str;
            tail:='';
          end;
          if (old_path<>'') and FilenameIsAbsolute(old_path) then begin
            lvl:=0;
            repeat
              old_path:=RemovePreDir(old_path);
              Inc(lvl);
              if (old_path<>'') and (Length(old_path)>1)
                 {$ifndef DEBUG_LAZ_XML} and
                 ( FileExistsUTF8(new_path+old_path) or
                   IsDirectory(new_path+old_path) ) {$endif}
              then begin
                Result:=Result+new_path+old_path+tail;
                patched:=True;
                {$ifdef DEBUG_LAZ_XML}Memo1.Lines.Add(Result);{$endif}
                break;
              end;
            until old_path='';
            if not patched then begin
              // base path
              if (lvl=1) and not IsDirectory(str) then
                Result:=Result+new_path+tail
                else
                  Result:=Result+str;
            end;
          end else
            Result:=Result+str;
        until not reg_fpccfg.ExecPos(np);
        if np<len then
          Result:=Result+Copy(Source,np);
      end;
    end;
  finally
    reg_fpccfg.Free;
  end;
end;

procedure TForm1.str_fpcdeluxe_ini(const Source: TStringList; new_path: string);
const
  ini_pattern = '([a-zA-Z]\:)?[\\/][^\\/]+[\\/][^,\n]+';
var
  i, len, np: Integer;
  old_path, txt, res: string;
  reg_fpccfg: RegExpr.TRegExpr;
  patched: Boolean;
begin
  new_path:=ExtractFileDir(new_path);
  reg_fpccfg:=TRegExpr.Create(ini_pattern);
  try
    reg_fpccfg.ModifierM:=True;
    if Source.Count>0 then
    for i:=0 to Source.Count-1 do begin
      txt:=Source[i];
      len:=Length(txt);
      np:=1;
      res:='';
      if reg_fpccfg.Exec(txt) then begin
        repeat
          patched:=False;
          res:=res+Copy(txt,np,reg_fpccfg.MatchPos[0]-np);
          np:=reg_fpccfg.MatchPos[0]+reg_fpccfg.MatchLen[0];

          old_path:=reg_fpccfg.Match[0];
          if old_path<>'' then begin
            repeat
              old_path:=RemovePreDir(old_path);
              if (old_path<>'') and (Length(old_path)>1) and
              ( FileExistsUTF8(new_path+old_path) or
                IsDirectory(new_path+old_path) )
              then begin
                res:=res+new_path+old_path;
                patched:=True;
                break;
              end;
            until old_path='';
            if not patched then
              res:=res+reg_fpccfg.Match[0];
          end;
        until not reg_fpccfg.ExecNext;
        if np<len then
          res:=res+Copy(txt,np);

        if txt<>res then
          Source[i]:=res;
      end;
    end;
  finally
    reg_fpccfg.Free;
  end;
end;

procedure TForm1.Patch_fpcupdeluxe_ini(const File_Path: string; new_path: string
  );
var
  Inbuff: TStringList;
  str: string;
begin
  if FileExistsUTF8(File_path) then begin
    InBuff := TStringList.Create;
    try
      Inbuff.LoadFromFile(File_path);
      str_fpcdeluxe_ini(Inbuff,new_path);
      RenameFile(File_Path,File_Path+'.bak');
      Inbuff.SaveToFile(File_Path);
    finally
      Inbuff.Free;
    end;
  end;
end;


end.

