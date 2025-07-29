unit fpcupset_main;

{$mode objfpc}{$H+}

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
    function Patch_fpc_cfg(const File_Path: string; new_path,
      fpc_dirname: string): string;
    function Patch_lazarus_cfg(const File_Path: string; new_path: string): string;
    procedure Patch_lazarus_xml(const File_Path: string; new_path: string);
    function str_lazarus_cfg(const Source: string; new_path: string): string;
    function str_lazarus_xml(const Source: string; new_path: string): string;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses shlobj, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, RegExpr, strutils;

{$R *.lfm}

var
  newpath:string;


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
begin
  newpath:=ExtractFilePath(ParamStr(0));
  EdLaz.Text:=newpath+config_folder;
  EdFPC.Text:=newpath+config_fpc;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  xml_path, lazcfg_path, fpccfg_path : string;
  i : Integer;
begin
  Memo1.Clear;
  Memo1.Lines.Add('New Path : '+newpath);
  // patch fpc cfg file
  fpccfg_path:=EdFPC.Text;
  if FileExists(fpccfg_path) then begin
    DeleteFile(fpccfg_path+'.bak');
    Patch_fpc_cfg(fpccfg_path,newpath,EdFPCDIR.Text);
    Memo1.Lines.Add('Patched : '+ fpccfg_path);
  end else
    Memo1.Lines.Add('File not found : '+fpccfg_path);
  // patch cfg files
  lazcfg_path:=EdLaz.Text+PathDelim+lazarus_cfg;
  if FileExists(lazcfg_path) then begin
    DeleteFile(lazcfg_path+'.bak');
    Patch_lazarus_cfg(lazcfg_path,newpath);
    Memo1.Lines.Add('Patched : '+lazarus_cfg);
  end else
    Memo1.Lines.Add('File not found : '+lazcfg_path);
  // patch xml files
  for i:=Low(lazarus_xml) to High(lazarus_xml) do begin
    xml_path:=EdLaz.Text+PathDelim+lazarus_xml[i];
    if FileExists(xml_path) then begin
        DeleteFile(xml_path+'.bak');
        Patch_lazarus_xml(EdLaz.Text+PathDelim+lazarus_xml[i],newpath);
        Memo1.Lines.Add('Patched : '+lazarus_xml[i]);
      end
      else
        Memo1.Lines.Add('File Not Found : '+xml_path);
  end;
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
begin
  Result:=FileGetAttr(s) and faDirectory<>0;
end;

function TForm1.Patch_fpc_cfg(const File_Path: string; new_path,
  fpc_dirname: string): string;
const
  reg_fpc1 = '([a-z,A-Z]\:)?[\\/][^\n;]+([\\/]';
  reg_fpc2 = '[\\/][^\n;]+)([\n;])?';
var
  len: Integer;
  i: Integer;
  resstr: string;
  Inbuff: TStringList;
  reg_fpccfg: RegExpr.TRegExpr;
begin
  Result:='';
  i := 1;
  new_path:=ExtractFileDir(new_path);
  len := length(new_path);
  while i<=len do begin
    if new_path[i] in ['\', '.', '[', ']', '(', ')', '$', '^', '-', '+']
      then begin
      Insert('\', new_path, i);
      Inc(i); Inc(len);
    end;
    Inc(i);
  end;
  if FileExists(File_path) then begin
    InBuff := TStringList.Create;
    try
      Inbuff.LoadFromFile(File_path);
      reg_fpccfg := TRegExpr.Create(reg_fpc1+fpc_dirname+reg_fpc2);
      try
        reg_fpccfg.ModifierM:=True;
        i:=0;
        if reg_fpccfg.Exec(Inbuff.Text) then begin
          (*
          repeat
            Memo1.Lines.Add(reg_fpccfg.Match[0]+'!'+reg_fpccfg.Match[2]);
          until not reg_fpccfg.ExecNext;
          *)
          Result:=reg_fpccfg.Replace(Inbuff.Text,new_path+'$2$3',True);
          Inbuff.Text:=Result;
          RenameFile(File_Path,File_Path+'.bak');
          Inbuff.SaveToFile(File_Path);
        end;
      finally
        reg_fpccfg.Free;
      end;
    finally
      Inbuff.Free;
    end;
  end;
end;

function TForm1.Patch_lazarus_cfg(const File_Path: string; new_path: string
  ): string;
var
  Inbuff: TStringList;
begin
  Result:='';
  if FileExists(File_path) then begin
    InBuff := TStringList.Create;
    try
      Inbuff.LoadFromFile(File_path);
      Result := str_lazarus_cfg(Inbuff.Text,new_path);
      Inbuff.Text:=Result;
      RenameFile(File_Path,File_Path+'.bak');
      Inbuff.SaveToFile(File_Path);
    finally
      Inbuff.Free;
    end;
  end;
end;

function TForm1.str_lazarus_cfg(const Source: string; new_path: string): string;
const
  lazarus_pattern = '(\-F.)([^\n;]+)';
var
  i, len, sp, ep: Integer;
  old_path: string;
  reg_fpccfg: RegExpr.TRegExpr;
begin
  Result:='';
  i := 1;
  new_path:=ExtractFileDir(new_path);
  len := length(Source);

  reg_fpccfg := TRegExpr.Create(lazarus_pattern);
  try
    reg_fpccfg.ModifierM:=True;
    sp:=1;
    ep:=1;
    if reg_fpccfg.Exec(Source) then begin
      repeat
        ep:=reg_fpccfg.MatchPos[0];
        Result:=Result+Copy(Source,sp,ep-sp);
        sp:=ep;

        old_path:=RemovePreDir(reg_fpccfg.Match[2]);
        while old_path<>'' do begin
          if IsDirectory(new_path+old_path) then begin
            Result:=Result+reg_fpccfg.Match[1]+new_path+old_path;
            break;
          end;
          old_path:=RemovePreDir(old_path);
        end;
        if old_path<>'' then
          sp:=ep+reg_fpccfg.MatchLen[0];
      until not reg_fpccfg.ExecNext;

      if len >= sp then
        Result:=Result+Copy(Source,sp,len-sp);
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

    if Node.HasAttributes and (Node.Attributes.Length>0) then
      for i:=0 to Node.Attributes.Length-1 do begin
        s:=Node.Attributes[i].NodeValue;
        s := str_lazarus_xml(s,new_path);
        if s<>'' then begin
          Node.Attributes[i].NodeValue:=s;
        end;
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
  RenameFile(File_Path,File_Path+'.bak');
  WriteXML(Doc,File_Path);

  finally
    Doc.Free;
  end;
end;

function TForm1.str_lazarus_xml(const Source: string; new_path: string): string;
const
  FilePath_Pattern = '[\\/][^\.\\/]+\.[^\.\\/]+';
var
  len: Integer;
  i: Integer;
  resstr: string;
  reg_fpccfg: RegExpr.TRegExpr;
  old_path: string;
begin
  Result:='';
  i:= 1;
  new_path:=ExtractFileDir(new_path);
  reg_fpccfg:=TRegExpr.Create(FilePath_Pattern);
  try
    reg_fpccfg.ModifierM:=True;
    if reg_fpccfg.Exec(Source) then begin
      old_path:=Source;
      repeat
        old_path:=RemovePreDir(old_path);
        if (old_path<>'') and
           ( FileExists(new_path+old_path) or
             IsDirectory(new_path+old_path) )
        then begin
          Result:=new_path+old_path;
          //Memo1.Lines.Add(Result);
          break;
        end;
      until old_path='';
    end;
  finally
    reg_fpccfg.Free;
  end;
end;


end.

