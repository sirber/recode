unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, AsyncProcess, Menus, LCLIntf, Grids, LazFileUtils;

type

  { TForm1 }
  TForm1 = class(TForm)
    cboVPreset: TComboBox;
    cboVProfile: TComboBox;
    cboVTune: TComboBox;
    cboVDenoise: TComboBox;
    CheckBox1: TCheckBox;
    chkDXVA2: TCheckBox;
    chkOpenCL: TCheckBox;
    chkFResize: TCheckBox;
    chkFDeblock: TCheckBox;
    chkFDenoise: TCheckBox;
    chkSBurn: TCheckBox;
    cboVMode: TComboBox;
    GroupBox4: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    txtFHResize: TEdit;
    txtFWResize: TEdit;
    GroupBox3: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    mmoHelp: TMemo;
    Subtitle: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Help: TTabSheet;
    txtATrack: TEdit;
    txtSTrack: TEdit;
    txtVBitrate: TEdit;
    Label3: TLabel;
    oProcess: TAsyncProcess;
    cmdStart: TButton;
    cmdStop: TButton;
    Button3: TButton;
    Audio: TGroupBox;
    cboVCodec: TComboBox;
    cboACodec: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    txtABitrate: TEdit;
    Video: TGroupBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pmFiles: TPopupMenu;
    txtDestination: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lstLogs: TListBox;
    lstFiles: TListBox;
    memoAbout: TMemo;
    pages: TPageControl;
    StatusBar1: TStatusBar;
    Status: TTabSheet;
    Files: TTabSheet;
    Settings: TTabSheet;
    About: TTabSheet;
    procedure cboACodecChange(Sender: TObject);
    procedure cboVCodecChange(Sender: TObject);
    procedure cboVModeChange(Sender: TObject);
    procedure chkFResizeChange(Sender: TObject);
    procedure chkFDenoiseChange(Sender: TObject);
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
    encoder: string;
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
  sVersion: string = '2017-12-04 x64 dev';

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

  { Find Handbreak CLI}
  encoder := 'C:\Program Files\Handbrake\HandBrakeCLI.exe';
  if (FileExists(encoder)) then
  begin
    AddLog('- Handbreak CLI detected.');
  end
  else
  begin
    AddLog('- Handbreak CLI not found. Please install Handbrerak.');
  end;

  pages.TabIndex:=0;
end;

{ Status }
procedure TForm1.cmdStartClick(Sender: TObject);
var
  sFile: string;
  sCmdLine: string;
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

  {video}
  case cboVCodec.ItemIndex of
    0: // x264
    begin
      sParameters.Add('-e');
      sParameters.Add('x264');
      sParameters.Add('--encoder-preset');
      sParameters.Add(cboVPreset.Items.Strings[cboVPreset.ItemIndex]);
      sParameters.Add('--encoder-tune');
      sParameters.Add(cboVTune.Items.Strings[cboVTune.ItemIndex]);
      sParameters.Add('--encoder-profile');
      sParameters.Add(cboVProfile.Items.Strings[cboVProfile.ItemIndex]);
    end;
    1: // x265
    begin
      sParameters.Add('-e');
      sParameters.Add('x265');
      sParameters.Add('--encoder-preset');
      sParameters.Add(cboVPreset.Items.Strings[cboVPreset.ItemIndex]);
      sParameters.Add('-2'); // 2-pass
    end;
    2: // vp8
    begin
      sParameters.Add('-e');
      sParameters.Add('vp8');
    end;
    3: // mpeg4
    begin
      sParameters.Add('-e');
      sParameters.Add('mpeg4');
    end;
  end;

  { settings / codec / common}
  case cboVMode.ItemIndex of
    0: // bitrate
    begin
      sParameters.Add('-2'); // 2-pass
      sParameters.Add('-T'); // turbo first pass
      sParameters.Add('-b');
    end;
    1: sParameters.Add('-q');  // quality
  end;
  sParameters.Add(txtVBitrate.Text);

  { advanced }
  if (chkOpenCL.Checked) then sParameters.Add('--use-opencl');
  if (chkDXVA2.Checked) then sParameters.Add('--use-hwd');
  if (chkDXVA2.Checked <> false) then sParameters.Add('--disable-qsv-decoding');

  {output}
  sParameters.Add('-o');
  sParameters.Add('"' + makeOutput(sFile) + '"');

  { audio }
  sParameters.Add('-E');
  sParameters.Add(cboACodec.Text);
  sParameters.Add('-a');
  sParameters.Add(txtATrack.Text);
  sParameters.Add('-B');
  sParameters.Add(txtABitrate.Text);

  { subtitle }
  if (strlen(pchar(txtSTrack.Text)) > 0) then
  begin
    sParameters.Add('-s');
    sParameters.Add(txtSTrack.Text);
    sCmdLine := sCmdLine + '-s ' + txtSTrack.Text + ' ';
    if (chkSBurn.Checked) then
       sParameters.Add('--subtitle-burn');
  end;

  { filtering }
  if (chkFDenoise.Checked) then
  begin
    sParameters.Add('--denoise');
    sParameters.Add(cboVDenoise.Items.Strings[cboVDenoise.ItemIndex]);
  end;
  if (chkFDeblock.Checked) then
    sParameters.Add('--deblock');
  if (chkFResize.Checked) then
  begin
    if (Length(txtFWResize.Text) > 0) then
    begin
      sParameters.Add('--width');
      sParameters.Add(txtFWResize.Text);
    end;
    if (Length(txtFHResize.Text) > 0) then
    begin
      sParameters.Add('--height');
      sParameters.Add(txtFHResize.Text);
    end;
  end;

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
    {case cboPreset.ItemIndex of
      0: sOutput := sOutput + '.mkv';
      1: sOutput := sOutput + '.mp4';
      2: sOutput := sOutput + '.mp4';
    end;}
    sOutput := StringReplace(sOutput, '{source}', ExtractFilePath(sFile), [rfIgnoreCase]);

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
  updateStatus('aborted');
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
  pages.TabIndex:=1;
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

{ Settings }
procedure TForm1.cboVCodecChange(Sender: TObject);
begin
  // x264 specific
  case cboVCodec.ItemIndex of
    0: // x264
    begin
      cboVPreset.Enabled := true;
      cboVTune.Enabled := true;
      cboVProfile.Enabled := true;
    end;
    1: // x265
    begin
      cboVPreset.Enabled := true;
      cboVTune.Enabled := false;
      cboVProfile.Enabled := false;
    end;
    2,3: // vp8, mpeg4
    begin
      cboVPreset.Enabled := false;
      cboVTune.Enabled := false;
      cboVProfile.Enabled := false;
    end;
  end;
end;

procedure TForm1.cboVModeChange(Sender: TObject);
begin
  case cboVMode.ItemIndex of
       0: // Bitrate
       begin
            txtVBitrate.Text := '352';
            Label3.Caption := 'kbps';
       end;
       1: // Quality
       begin
            txtVBitrate.Text := '20';
            Label3.Caption := 'Q';
       end;
  end;
end;

procedure TForm1.chkFResizeChange(Sender: TObject);
begin
  txtFWResize.Enabled:=false;
  txtFWResize.Text:='';
  txtFHResize.Enabled:=false;
  txtFhResize.Text:='';
  if (chkFResize.Checked) then
  begin
    txtFWResize.Enabled:=true;
    txtFHResize.Enabled:=true;
  end;
end;

procedure TForm1.chkFDenoiseChange(Sender: TObject);
begin
  if ((chkFDenoise.Checked) and (cboVCodec.ItemIndex = 0)) then
    cboVDenoise.Enabled:=true
  else
    cboVDenoise.Enabled:=false;
end;

procedure TForm1.cboACodecChange(Sender: TObject);
begin
  case cboACodec.ItemIndex of
    0: txtABitrate.Text:='64';
    1: txtABitrate.Text:='96';
    2: txtABitrate.Text:='64';
    3: txtABitrate.Text:='128';
  end;
  txtATrack.Text := '1';
end;

{ About }

{*** ENCODING ***}
procedure TForm1.encode_start(sFile: string; sParameters: TStrings);
begin
  addLog('Encoding: ' + ExtractFileNameOnly(sFile));
  oProcess.Executable:=encoder;  // global
  oProcess.Parameters:=sParameters;

  oProcess.Execute;
end;

procedure TForm1.oProcessTerminate(Sender: TObject);
begin
  if (oProcess.ExitStatus <> 0) then
  begin
    addLog('> error.');
    cmdStopClick(Sender);
    Exit;
  end;

  addLog('> finished.');

  // Remove encoded file
  lstFiles.Items.Delete(0);

  // Start again
  cmdStartClick(Sender);
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
  end;
end;



end.

