unit fpcupset_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function Patch_fpc_cfg(const File_Path: string; new_path,
      fpc_dirname: string): string;
    function Patch_lazarus_cfg(const File_Path: string; new_path: string;
      customdir: array of string): string;
    procedure Patch_lazarus_xml(const File_Path: string; new_path: string;
      customdir: array of string);
    function str_lazarus_cfg(const Source: string; new_path: string;
      customdir: array of string): string;
    function str_lazarus_xml(const Source: string; new_path: string;
      customdir: array of string): string;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses shlobj, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, RegExpr, strutils;

{$R *.lfm}

const
  config_folder='config_lazarus';
  config_fpc='fpc\bin\i386-win32\fpc.cfg';

var
  newpath:string;


const
  lazarus_cfg = 'idemake.cfg';
  lazarus_xml : array[0..5] of string = (
                'codetoolsoptions.xml',
                'editoroptions.xml',
                'environmentoptions.xml',
                'fpcdefines.xml',
                'inputhistory.xml',
                'packagefiles.xml'
                );
type
  TArrayString = array of string;

  procedure StrToArray(const s:string; var sarray:TArrayString);
  var
    i, start, len : Integer;
  begin
    SetLength(sarray,0);
    start := 1;
    len := Length(s);
    if len>0 then begin
      repeat
        i:= PosEx(';',s,Start);
        if i=0 then
          i := len+1;
        SetLength(sarray,Length(sarray)+1);
        sarray[Length(sarray)-1]:=Copy(s,Start,i-Start);
        Start:=i+1;
      until i>=len;
    end;
  end;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  newpath:=ExtractFilePath(ParamStr(0));
  Edit1.Text:=newpath+config_folder;
  Edit2.Text:=newpath+config_fpc;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  xml_path, lazcfg_path, fpccfg_path : string;
  i : Integer;
  sarray : array of string;
begin
  Memo1.Clear;
  SetLength(sarray,0);
  StrToArray(Edit4.Text,sarray);
  Memo1.Lines.Add('New Path : '+newpath);
  // patch fpc cfg file
  fpccfg_path:=Edit2.Text;
  if FileExists(fpccfg_path) then begin
    Patch_fpc_cfg(fpccfg_path,newpath,Edit3.Text);
    Memo1.Lines.Add('Patched : '+ fpccfg_path);
  end else
    Memo1.Lines.Add('File not found : '+fpccfg_path);
  // patch cfg files
  lazcfg_path:=Edit1.Text+PathDelim+lazarus_cfg;
  if FileExists(lazcfg_path) then begin
    Patch_lazarus_cfg(lazcfg_path,newpath,sarray);
    Memo1.Lines.Add('Patched : '+lazarus_cfg);
  end else
    Memo1.Lines.Add('File not found : '+lazcfg_path);
  // patch xml files
  for i:=Low(lazarus_xml) to High(lazarus_xml) do begin
    xml_path:=Edit1.Text+PathDelim+lazarus_xml[i];
    if FileExists(xml_path) then begin
        Patch_lazarus_xml(Edit1.Text+PathDelim+lazarus_xml[i],newpath,sarray);
        Memo1.Lines.Add('Patched : '+lazarus_xml[i]);
      end
      else
        Memo1.Lines.Add('File Not Found : '+xml_path);
  end;
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
  if FileExistsUTF8(File_path) then begin
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
        end;
      finally
        reg_fpccfg.Free;
      end;
    finally
      Inbuff.Free;
    end;
  end;
end;

function TForm1.Patch_lazarus_cfg(const File_Path: string; new_path: string;
  customdir : array of string): string;
var
  Inbuff: TStringList;
begin
  Result:='';
  if FileExistsUTF8(File_path) then begin
    InBuff := TStringList.Create;
    try
      Inbuff.LoadFromFile(File_path);
      Result := str_lazarus_cfg(Inbuff.Text,new_path,customdir);
    finally
      Inbuff.Free;
    end;
  end;
end;

function TForm1.str_lazarus_cfg(const Source: string; new_path: string;
  customdir: array of string): string;
const
  lazarus_pattern1 = '([a-z,A-Z]\:)?[\\/][^\n]+([\\/]((?i)';
  lazarus_pattern2 = ')[\\/]?[^\n]+)';
var
  len: Integer;
  i: Integer;
  resstr: string;
  reg_fpccfg: RegExpr.TRegExpr;
  regstr : string;
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
  regstr := lazarus_pattern1;
  for i:=0 to Length(customdir)-1 do
    if customdir[i]<>'' then
      if i>0 then
        regstr:=regstr+'|'+customdir[i]
        else
          regstr:=regstr+customdir[i];
  regstr:=regstr+lazarus_pattern2;
  reg_fpccfg := TRegExpr.Create(regstr);
  try
    reg_fpccfg.ModifierM:=True;
    if reg_fpccfg.Exec(Source) then begin
      (*
      repeat
        Memo1.Lines.Add('=======================');
        Memo1.Lines.Add(reg_fpccfg.Match[0]+'!'+reg_fpccfg.Match[2]);
      until not reg_fpccfg.ExecNext;
      *)
      Result:=reg_fpccfg.Replace(Source, new_path+'$2', True);
    end;
  finally
    reg_fpccfg.Free;
  end;
end;

procedure TForm1.Patch_lazarus_xml(const File_Path: string; new_path: string;
  customdir: array of string);
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
        s := str_lazarus_xml(s,new_path,customdir);
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
  WriteXML(Doc,File_Path);

  finally
    Doc.Free;
  end;
end;

function TForm1.str_lazarus_xml(const Source: string; new_path: string;
  customdir: array of string): string;
const
  lazarus_pattern1 = '((?i)[\\/]';
  lazarus_pattern2 = '[\\/]?)';
var
  len: Integer;
  i: Integer;
  resstr: string;
  reg_fpccfg: RegExpr.TRegExpr;
  regstr : string;
begin
  Result:='';
  i := 1;
  new_path:=ExtractFileDir(new_path);
  for i:=0 to Length(customdir)-1 do
    if customdir[i]<>'' then begin
      regstr := lazarus_pattern1+customdir[i]+lazarus_pattern2;
      reg_fpccfg:=TRegExpr.Create(regstr);
      try
        reg_fpccfg.ModifierM:=True;
        if reg_fpccfg.Exec(Source) then begin
          Result:=new_path+ Copy(Source,reg_fpccfg.MatchPos[1],Length(Source)-reg_fpccfg.MatchPos[1]+1);
          (*
          Memo1.Lines.Add('==========================================');
          Memo1.Lines.Add(Result);
          *)
          break;
        end;
      finally
        reg_fpccfg.Free;
      end;
    end;
end;


end.

