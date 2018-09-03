unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, AsyncProcess, Menus, LCLIntf, LazFileUtils,
  Clipbrd;

type

  { TForm1 }
  TForm1 = class(TForm)
    Audio: TGroupBox;
    Button3: TButton;
    cboVCodec: TComboBox;
    cboVMode: TComboBox;
    cboVPreset: TComboBox;
    cboVTune: TComboBox;
    chkFResize: TCheckBox;
    chkSubs: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lstFiles: TListBox;
    lstLogs: TListBox;
    txtABitrate: TEdit;
    txtDestination: TEdit;
    mmoHelp: TMemo;
    Help: TTabSheet;
    oProcess: TAsyncProcess;
    cmdStart: TButton;
    cmdStop: TButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pmFiles: TPopupMenu;
    memoAbout: TMemo;
    pages: TPageControl;
    StatusBar1: TStatusBar;
    Main: TTabSheet;
    About: TTabSheet;
    txtVBitrate: TEdit;
    Video: TGroupBox;
    procedure cboVCodecChange(Sender: TObject);
    procedure cboVModeChange(Sender: TObject);
    procedure cmdStartClick(Sender: TObject);
    procedure cmdStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure Label13Click(Sender: TObject);
    procedure Label14Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure oProcessReadData(Sender: TObject);
    procedure oProcessTerminate(Sender: TObject);
  private
    { private declarations }
    procedure encode_start(sFile: string; sParameters: TStrings);
    procedure addLog(sMessage: string);
    procedure updateStatus(sStatus: string);
    function makeOutput(sFile: string):string;
  public
    { public declarations }
  end; 

var
  Form1: TForm1;

const
  sVersion: string = '2018-01-04 x64 dev';

implementation

{$R *.lfm}

{ Misc functions }
function CountPos(const subtext: string; Text: string): Integer;
begin
  if (Length(subtext) = 0) or (Length(Text) = 0) or (Pos(subtext, Text) = 0) then
    Result := 0
  else
    Result := (Length(Text) - Length(StringReplace(Text, subtext, '', [rfReplaceAll]))) div Length(subtext);
end;

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  Form1.Text:='Recode v' + sVersion;
  Application.Title:=Form1.Text;
  addLog(Form1.Text);
  memoAbout.Lines.Text := StringReplace(memoAbout.Lines.Text, '{version}', sVersion, [rfIgnoreCase]);   ;

  pages.TabIndex:=0;
end;

{ Main }
procedure TForm1.cmdStartClick(Sender: TObject);
var
  sFile: string;
  sParameters: TStrings;
begin
  if (lstFiles.Items.Count = 0) then
  begin
    cmdStopClick(Sender);
    updateStatus('done');
    Exit;
  end;

  sParameters := TStringList.Create;

  cmdStop.Enabled:=true;
  cmdStart.Enabled:=false;

  { make cmdline }
  sFile := lstFiles.Items.Strings[0];

  { input / output }
  sParameters.Add('-i');
  sParameters.Add('"' + sFile + '"');

  sParameters.Add('-y');

  {video}
  sParameters.Add('-c:v');
  case cboVCodec.ItemIndex of
    0: // x264
    begin
      sParameters.Add('libx264');
      sParameters.Add('-preset');
      sParameters.Add(cboVPreset.Items.Strings[cboVPreset.ItemIndex]);
      if ( cboVTune.ItemIndex > 0 ) then
      begin
           sParameters.Add('-tune');
           sParameters.Add(cboVTune.Items.Strings[cboVTune.ItemIndex]);
      end;
    end;
    1: // H264 (NVENC)
    begin
      sParameters.Add('h264_nvenc');
    end;
    2: // H264 (qsv)
    begin
      sParameters.Add('h264_qsv');
      sParameters.Add('-preset');
      sParameters.Add(cboVPreset.Items.Strings[cboVPreset.ItemIndex]);
      sParameters.Add('-look_ahead');
      sParameters.Add('0');
    end;
    3: // x265
    begin
      sParameters.Add('libx265');
      sParameters.Add('--encoder-preset');
      sParameters.Add(cboVPreset.Items.Strings[cboVPreset.ItemIndex]);
    end;
    4: // H265 (NVENC)
    begin
      sParameters.Add('hevc_nvenc');
    end;
    5: // H265 (qsv)
    begin
      sParameters.Add('hevc_qsv');
    end;
    6: // vp8
    begin
      sParameters.Add('libvpx');
    end;
    7: // vp9
    begin
      sParameters.Add('libvpx-vp9');
    end;
  end;

  { settings / codec / common}
  case cboVMode.ItemIndex of
    0: // bitrate
    begin
      sParameters.Add('-b:v');
      sParameters.Add( Concat(txtVBitrate.Text, 'K') );
    end;
    1: // quality
    begin
      sParameters.Add('-crf');
      sParameters.Add(txtVBitrate.Text);
    end;
  end;

  { audio } { forced he-aac v2 }
  sParameters.Add('-b:a');
  sParameters.Add( Concat(txtABitrate.Text, 'K') );
  sParameters.Add('-c:a');
  sParameters.Add('libfdk_aac');
  sParameters.Add('-profile:a');
  sParameters.Add('aac_he_v2');
  sParameters.Add('-ac');
  sParameters.Add('2');

  if (chkSubs.Checked) then
  begin
      sParameters.Add('-c:s');
      sParameters.Add('copy');
  end;

  if (chkFResize.Checked) then
  begin

  end;

  {output}
  sParameters.Add('"' + makeOutput(sFile) + '"');

  Clipboard.AsText := sParameters.Text;

  { start }
  encode_start(sFile, sParameters);
end;

function TForm1.makeOutput(sFile: string):string;
var
  bFound: boolean;
  sOutput: string;
  sSuffix: string;
  iCpt: integer;
begin
  sSuffix := '';
  iCpt := 0;
  repeat
    bFound := false;
    sOutput := txtDestination.Text + ExtractFileNameOnly(sFile) + sSuffix;
    sOutput := sOutput + '.mkv';
    sOutput := StringReplace(sOutput, '{source}', ExtractFilePath(sFile), [rfIgnoreCase]);

    sOutput := stringreplace(sOutput, '\\', '\', [rfReplaceAll, rfIgnoreCase]);

    if (FileExists(sOutput)) then
    begin
      // File exists, add suffix
      inc(iCpt);
      sSuffix := '('+IntToStr(iCpt)+')';
      bFound:=true;
    end;
  until bFound = false;

  Result:= sOutput;
end;

procedure TForm1.cmdStopClick(Sender: TObject);
begin
  oProcess.Terminate(1);
  cmdStop.Enabled:=false;
  cmdStart.Enabled:=true;
  // updateStatus('aborted');
end;

procedure TForm1.addLog(sMessage: string);
var
  sDate: string;
begin
  sDate := '[' + TimeToStr(time()) + '] ';
  lstLogs.Items.Add(sDate + sMessage);
  lstLogs.ItemIndex := lstLogs.Items.Count - 1;
end;

procedure TForm1.updateStatus(sStatus: string);
begin
  StatusBar1.Panels.Items[1].Text := sStatus;
end;

{ Files }
procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  iCpt: integer;
begin
  for iCpt := 0 to (Length(FileNames) - 1) do
  begin
    if (FileExistsUTF8(FileNames[iCpt])) then
        lstFiles.Items.Add(FileNames[iCpt]);
  end;
  pages.TabIndex:=0;
end;

procedure TForm1.Label13Click(Sender: TObject);
begin
  OpenURL('https://github.com/sirber/recode');
end;

procedure TForm1.Label14Click(Sender: TObject);
begin
  OpenURL('https://paypal.me/sirber');
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  if ((oProcess.Active) and (lstFiles.ItemIndex = 0)) then
    ShowMessage('Cannot remove this file while encoding.')
  else
    lstFiles.Items.Delete(lstFiles.ItemIndex);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if (oProcess.Active) then
  begin
    while (lstFiles.Items.Count > 1) do // remove all files except the first one
      lstFiles.Items.Delete(1);
  end
  else
    lstFiles.Clear;
end;

procedure TForm1.oProcessReadData(Sender: TObject);
var
  aOutput: TStringList;
begin
  aOutput := TStringList.Create();
  aOutput.LoadFromStream(oProcess.Output);
  if (aOutput.Count > 0) then
  begin
    updateStatus(aOutput.Strings[aOutput.Count - 1]);
    mmoHelp.Lines.AddStrings(aOutput);
  end;
end;

{ Settings }
procedure TForm1.cboVCodecChange(Sender: TObject);
begin
  case cboVCodec.ItemIndex of
    0: // x264
    begin
      cboVPreset.Enabled := true;
      cboVTune.Enabled := true;
    end;
    1: // h264 nvenc
    begin
      cboVPreset.Enabled := false;
      cboVTune.Enabled := false;
    end;
    2: // h264_qsv
    begin
      cboVPreset.Enabled := true;
      cboVTune.Enabled := false;
    end;
    3: // x265
    begin
      cboVPreset.Enabled := true;
      cboVTune.Enabled := false;
    end;
    4,5: // h265 hw
    begin
      cboVPreset.Enabled := false;
      cboVTune.Enabled := false;
    end;
    6,7: // vp8, vp9
    begin
      cboVPreset.Enabled := false;
      cboVTune.Enabled := false;
    end;
  end;
end;

procedure TForm1.cboVModeChange(Sender: TObject);
begin
  case cboVMode.ItemIndex of
       0: // Bitrate
       begin
            txtVBitrate.Text := '512';
       end;
       1: // Quality
       begin
            txtVBitrate.Text := '24';
       end;
  end;
end;

{ About }

{*** ENCODING ***}
procedure TForm1.encode_start(sFile: string; sParameters: TStrings);
begin
  addLog('Encoding: ' + ExtractFileNameOnly(sFile));
  oProcess.Executable := 'bin/ffmpeg.exe';
  oProcess.Parameters := sParameters;

  updateStatus('encoding...');
  oProcess.Execute;
end;

procedure TForm1.oProcessTerminate(Sender: TObject);
begin
  if (oProcess.ExitStatus <> 0) then
  begin
    addLog('> error #' + oProcess.ExitStatus.ToString);
    cmdStopClick(Sender);
    Exit;
  end;

  addLog('> finished.');

  // Remove encoded file
  lstFiles.Items.Delete(0);

  // Start again
  cmdStartClick(Sender);
end;


end.

