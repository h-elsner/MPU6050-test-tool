(*
MPU6050 am Raspberry Pi

https://tutorials-raspberrypi.de/raspberry-pi-mpu-6050-rotationssensor-webgl-nodejs-server/

sudo raspi-config  --> I2C enable

i2cdetect -y 1  ---> Addr=68
An Adresse 0x68 (Hexadezimal) befindet sich also ein I2C Gerät – in unserem Fall ist es das MPU 6050 Gyroskop.

Usage: i2cset [-f] [-y] [-m MASK] [-r] [-a] I2CBUS CHIP-ADDRESS DATA-ADDRESS [VALUE] ... [MODE]
  I2CBUS is an integer or an I2C bus name
  ADDRESS is an integer (0x03 - 0x77, or 0x00 - 0x7f if -a is given)
  MODE is one of:
    c (byte, no value)
    b (byte data, default)
    w (word data)
    i (I2C block data)
    s (SMBus block data)
    Append p for SMBus PEC

Usage: i2cget [-f] [-y] [-a] I2CBUS CHIP-ADDRESS [DATA-ADDRESS [MODE]]
  I2CBUS is an integer or an I2C bus name
  ADDRESS is an integer (0x03 - 0x77, or 0x00 - 0x7f if -a is given)
  MODE is one of:
    b (read byte data, default)
    w (read word data)
    c (write byte/read byte)
    Append p for SMBus PEC

Auslesen am i2c bus 1 Beispiel:
sudo i2cget -y 1 0x68 0x75

Weitere Kommandos:
Für einen ersten Test wecken Sie den Chip aus dem Schlafmodus, indem Sie den Wert 0x00h ins Statusregister 0x6Bh schreiben (Listing 1).
Danach lesen Sie das High-Byte des Sensors für die X-Achse aus. Die Werte sollten sich ändern, sobald Sie den Sensor um die X-Achse (siehe Aufdruck auf dem Modul) drehen.
$ i2cset -y 1 0x68 0x6b 0x00
$ watch -n 0.5 'i2cget -y 1 0x68 0x44'

https://invensense.tdk.com/wp-content/uploads/2015/02/MPU-6000-Register-Map1.pdf
*)


unit imu_test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, ComCtrls, TAGraph, TASources, TASeries, strutils, mpu_ctrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRead: TButton;
    btnRdVal: TButton;
    btnClose: TButton;
    btnWrAdr: TButton;
    btnWrPrev: TButton;
    btnSave: TButton;
    btnStop: TButton;
    btnWrZero: TButton;
    btnRefresh: TButton;
    chGyro: TChart;
    chAcc: TChart;
    chGyroLineX: TLineSeries;
    chAccLineX: TLineSeries;
    chGyroLineY: TLineSeries;
    chAccLineY: TLineSeries;
    chGyroLineZ: TLineSeries;
    chAccLineZ: TLineSeries;
    edAdr: TEdit;
    edValue: TEdit;
    gridReg: TStringGrid;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblError: TLabel;
    lblBin: TLabel;
    lblHex: TLabel;
    lblTemp: TLabel;
    lblMPU: TLabel;
    lblAddr: TLabel;
    ListChartSource1: TListChartSource;
    PageControl: TPageControl;
    SaveDialog: TSaveDialog;
    tsChartA: TTabSheet;
    tsTools: TTabSheet;
    tsTable: TTabSheet;
    tsChartG: TTabSheet;
    Timer1: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnRdValClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnWrAdrClick(Sender: TObject);
    procedure btnWrPrevClick(Sender: TObject);
    procedure btnWrZeroClick(Sender: TObject);
    procedure edAdrChange(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridRegPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure Timer1Timer(Sender: TObject);        {Cyclic read register}
  private
    procedure RegHeader;
    procedure ValHeader;
  public
    procedure RefreshSensor;
  end;

var
  Form1: TForm1;
  fs_sel, afs_sel: byte;
  samples: integer;

type
  TWdVal  = array[0..6] of int16;

const
  df='%2.2d';                                      {Format two digits with leading zero}
  tf='0.0';
  gf='0.000';
  maxSamples=100;
  ziff=['0'..'9'];

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.RegHeader;                        {Header and register names}
var
  i: byte;

begin
  PageControl.ActivePage:=tsTable;
  gridReg.BeginUpdate;
  gridReg.RowCount:=83;
  gridReg.Cells[0, 0]:='Name';
  gridReg.Cells[1, 0]:='Addr hex';
  gridReg.Cells[2, 0]:='Addr dec';
  gridReg.Cells[4, 0]:='Value bin';
  gridReg.Cells[5, 0]:='Hex';
  gridReg.Cells[6, 0]:='Dec';

  gridReg.Cells[0, 1]:='SELF_TEST_X';
  gridReg.Cells[0, 2]:='SELF_TEST_Y';
  gridReg.Cells[0, 3]:='SELF_TEST_Z';
  gridReg.Cells[0, 4]:='SELF_TEST_A';
  gridReg.Cells[0, 5]:='SMPLRT_DIV';
  gridReg.Cells[0, 6]:='CONFIG';
  gridReg.Cells[0, 7]:='GYRO_CONFIG';
  gridReg.Cells[0, 8]:='ACCEL_CONFIG';
  gridReg.Cells[0, 9]:='FIFO_EN';
  gridReg.Cells[0, 10]:='I2C_MST_CTRL';
  gridReg.Cells[0, 11]:='I2C_SLV0_ADDR';
  gridReg.Cells[0, 12]:='I2C_SLV0_REG';
  gridReg.Cells[0, 13]:='I2C_SLV0_CTRL';
  gridReg.Cells[0, 14]:='I2C_SLV1_ADDR';
  gridReg.Cells[0, 15]:='I2C_SLV1_REG';
  gridReg.Cells[0, 16]:='I2C_SLV1_CTRL';
  gridReg.Cells[0, 17]:='I2C_SLV2_ADDR';
  gridReg.Cells[0, 18]:='I2C_SLV2_REG';
  gridReg.Cells[0, 19]:='I2C_SLV2_CTRL';
  gridReg.Cells[0, 20]:='I2C_SLV3_ADDR';
  gridReg.Cells[0, 21]:='I2C_SLV3_REG';
  gridReg.Cells[0, 22]:='I2C_SLV3_CTRL';
  gridReg.Cells[0, 23]:='I2C_SLV4_ADDR';
  gridReg.Cells[0, 24]:='I2C_SLV4_REG';
  gridReg.Cells[0, 25]:='I2C_SLV4_DO';
  gridReg.Cells[0, 26]:='I2C_SLV4_CTRL';
  gridReg.Cells[0, 27]:='I2C_SLV4_DI';
  gridReg.Cells[0, 28]:='I2C_MST_STATUS';
  gridReg.Cells[0, 29]:='INT_PIN_CFG';
  gridReg.Cells[0, 30]:='INT_ENABLE';
  gridReg.Cells[0, 31]:='INT_STATUS';
  gridReg.Cells[0, 32]:='ACCEL_XOUT_H';            {Acceleration}
  gridReg.Cells[0, 33]:='ACCEL_XOUT_L';
  gridReg.Cells[0, 34]:='ACCEL_YOUT_H';
  gridReg.Cells[0, 35]:='ACCEL_YOUT_L';
  gridReg.Cells[0, 36]:='ACCEL_ZOUT_H';
  gridReg.Cells[0, 37]:='ACCEL_ZOUT_L';
  gridReg.Cells[0, 38]:='TEMP_OUT_H';              {Temperatur}
  gridReg.Cells[0, 39]:='TEMP_OUT_L';
  gridReg.Cells[0, 40]:='GYRO_XOUT_H';             {Gyroscope}
  gridReg.Cells[0, 41]:='GYRO_XOUT_L';
  gridReg.Cells[0, 42]:='GYRO_YOUT_H';
  gridReg.Cells[0, 43]:='GYRO_YOUT_L';
  gridReg.Cells[0, 44]:='GYRO_ZOUT_H';
  gridReg.Cells[0, 45]:='GYRO_ZOUT_L';

  for i:=0 to 23 do                                {External sensor data}
    gridReg.Cells[0, i+46]:='EXT_SENS_DATA_'+Format(df, [i]);

  gridReg.Cells[0, 70]:='I2C_SLV0_DO';
  gridReg.Cells[0, 71]:='I2C_SLV1_DO';
  gridReg.Cells[0, 72]:='I2C_SLV2_DO';
  gridReg.Cells[0, 73]:='I2C_SLV3_DO';
  gridReg.Cells[0, 74]:='I2C_MST_DELAY_CT RL';
  gridReg.Cells[0, 75]:='SIGNAL_PATH_RESET';
  gridReg.Cells[0, 76]:='USER_CTRL';
  gridReg.Cells[0, 77]:='PWR_MGMT_1';
  gridReg.Cells[0, 78]:='PWR_MGMT_2';
  gridReg.Cells[0, 79]:='FIFO_COUNTH';
  gridReg.Cells[0, 80]:='FIFO_COUNTL';
  gridReg.Cells[0, 81]:='FIFO_R_W';
  gridReg.Cells[0, 82]:='WHO_AM_I';
  gridReg.EndUpdate;
  MPUWakeUp;                                       {Wake up}
end;

procedure TForm1.ValHeader;                        {Header and shown values}
begin
  gridReg.BeginUpdate;
  gridReg.RowCount:=8;
  gridReg.Cells[0, 0]:='Value name';
  gridReg.Cells[1, 0]:='Raw hex';
  gridReg.Cells[2, 0]:='Raw dec';
  gridReg.Cells[4, 0]:='Info';
  gridReg.Cells[5, 0]:='Value';
  gridReg.Cells[6, 0]:='Unit';

  gridReg.Cells[0, 1]:='ACCELERATION_X';
  gridReg.Cells[0, 2]:='ACCELERATION_Y';
  gridReg.Cells[0, 3]:='ACCELERATION_Z';
  gridReg.Cells[0, 4]:='IMU_TEMPERATUR';
  gridReg.Cells[0, 5]:='GYRO_X';
  gridReg.Cells[0, 6]:='GYRO_Y';
  gridReg.Cells[0, 7]:='GYRO_Z';

  gridReg.Cells[6, 1]:='mG';
  gridReg.Cells[6, 2]:=gridReg.Cells[6, 1];
  gridReg.Cells[6, 3]:=gridReg.Cells[6, 1];
  gridReg.Cells[6, 4]:='°C';
  gridReg.Cells[6, 5]:='°/s';
  gridReg.Cells[6, 6]:=gridReg.Cells[6, 5];
  gridReg.Cells[6, 7]:=gridReg.Cells[6, 5];

  gridReg.Cells[4, 1]:='Acc scale';
  gridReg.Cells[4, 2]:=afsToStr(afs_sel);
  gridReg.Cells[4, 3]:='';
  gridReg.Cells[4, 4]:='t/340+36.53°C';
  gridReg.Cells[4, 5]:='Gyro scale';
  gridReg.Cells[4, 6]:=fsToStr(fs_sel);
  gridReg.Cells[4, 7]:='';
  gridReg.EndUpdate;
  MPUWakeUp;                                       {Wake up}
end;

procedure TForm1.RefreshSensor;                    {Check again if MPU is available}
var
  i: integer;

begin
  if GetAdrStr then begin
    lblMPU.Caption:=DefAddr;
    RegHeader;
    for i:=0 to MaxSamples do begin
      chGyroLineX.AddXY(i, 0);
      chGyroLineY.AddXY(i, 0);
      chGyroLineZ.AddXY(i, 0);
      chAccLineX.AddXY(i, 0);
      chAccLineY.AddXY(i, 0);
      chAccLineZ.AddXY(i, 0);
    end;
    samples:=0;
    lblTemp.Caption:=TempToStr;
    btnRead.Enabled:=true;
    btnWrZero.Enabled:=true;
    btnWrPrev.Enabled:=true;
    btnWrAdr.Enabled:=true;
    btnRdVal.Enabled:=true;
  end else begin
    btnRead.Enabled:=false;
    btnWrZero.Enabled:=false;
    btnWrPrev.Enabled:=false;
    btnWrAdr.Enabled:=false;
    btnRdVal.Enabled:=false;
    lbltemp.Caption:='';
    lblMPU.Caption:='No MPU connected';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);      {Init, settings}
begin
  Timer1.Enabled:=false;
  btnWrPrev.Enabled:=false;
  fs_sel:=0;                                       {Default scale factors}
  afs_sel:=0;
  samples:=0;
  Caption:='Read/write register from IMU MPU60x0'; {Init, try MPU6050}
  RefreshSensor;
end;

procedure TForm1.gridRegPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if aCol=3 then begin
    if aRow=0 then begin
      gridReg.Canvas.Brush.Color:=gridReg.FixedGridLineColor
    end else
      gridReg.Canvas.Brush.Color:=gridReg.GridLineColor;
  end;
end;

procedure TForm1.btnReadClick(Sender: TObject);    {Read all register}
var
  i, b, x: byte;

begin
  Timer1.Enabled:=false;
  RegHeader;
  lblTemp.Caption:=TempToStr;
  gridReg.BeginUpdate;
  for i:=1 to 4 do begin
    x:=i+12;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=5 to 8 do begin
    x:=i+20;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=9 to 30 do begin
    x:=i+26;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=31 to 69 do begin
    x:=i+27;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=70 to 75 do begin
    x:=i+29;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=76 to 78 do begin
    x:=i+30;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=79 to 82 do begin
    x:=i+35;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
//  gridReg.TopRow:=31;                            {Jump to acc values}
  gridReg.EndUpdate;
  btnWrPrev.Enabled:=true;
end;

procedure TForm1.btnRefreshClick(Sender: TObject); {Refresh sensor status}
begin
  RefreshSensor;
end;

procedure TForm1.btnRdValClick(Sender: TObject);   {Start reading values}
begin
  btnWrPrev.Enabled:=false;
  btnWrAdr.Enabled:=false;
  fs_sel:=GetFS_SEL;                               {Gyro Scale 0..3}
  afs_sel:=GetAFS_SEL;                             {Acc Scale 0..3}
  ValHeader;
  samples:=0;
  Timer1.Enabled:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);     {Cyclic read values Acc/Temp/Gyro}
var
  i, a: byte;
  varr: TWdVal;

begin
  a:=59;                                           {Register for Acc}
  for i:=0 to 6 do begin                           {Read from MPU Acc, Temp, Gyro}
    varr[i]:=GetRegW(DefAddr, a);
    a:=a+2;
  end;

  if PageControl.ActivePage=tsTable then begin     {Write to table}
    gridReg.BeginUpdate;                           {Values raw}
    for i:=0 to 2 do begin                         {Acceleration}
      gridReg.Cells[1, i+1]:=IntToHex(varr[i], 4);
      gridReg.Cells[2, i+1]:=IntToStr(varr[i]);
      gridReg.Cells[5, i+1]:=FormatFloat(tf, ConvAcc(afs_sel, varr[i], true));
    end;
    gridReg.Cells[1, 4]:=IntToHex(varr[3], 4);
    gridReg.Cells[2, 4]:=IntToStr(varr[3]);
    gridReg.Cells[5, 4]:=FormatFloat(tf, ConvTemp(varr[3])); {Temperature}
    lblTemp.Caption:=gridReg.Cells[5, 4]+'°C';
    for i:=4 to 6 do begin                         {Gyroscope}
      gridReg.Cells[1, i+1]:=IntToHex(varr[i], 4);
      gridReg.Cells[2, i+1]:=IntToStr(varr[i]);
      gridReg.Cells[5, i+1]:=FormatFloat(gf, ConvGyro(fs_sel, varr[i]));
    end;
    gridReg.EndUpdate;
  end else begin
    lblTemp.Caption:=FormatFloat(tf, ConvTemp(varr[3]))+'°C';
  end;

  if PageControl.ActivePage=tsChartG then begin    {Gyroscope}
    chGyroLineX.SetYValue(samples, ConvGyro(fs_sel, varr[4]));
    chGyroLineY.SetYValue(samples, ConvGyro(fs_sel, varr[5]));
    chGyroLineZ.SetYValue(samples, ConvGyro(fs_sel, varr[6]));
  end;

  if PageControl.ActivePage=tsChartA then begin    {Accelerometer}
    chAccLineX.SetYValue(samples, ConvAcc(afs_sel, varr[0], true));
    chAccLineY.SetYValue(samples, ConvAcc(afs_sel, varr[1], true));
    chAccLineZ.SetYValue(samples, ConvAcc(afs_sel, varr[2], true));
  end;

  inc(samples);
  if samples>MaxSamples then
    samples:=0;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Timer1.Enabled:=false;
  Close;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  Timer1.Enabled:=false;
  if SaveDialog.Execute then
    gridReg.SaveToCSVFile(SaveDialog.FileName, ';');
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  Timer1.Enabled:=false;
  btnWrAdr.Enabled:=true;
end;

procedure TForm1.btnWrAdrClick(Sender: TObject);   {Write one byte to a register}
var
  a: integer;

begin
  a:=StrToIntDef(edAdr.Text, 0) and $FF;
  if a in rwregs then begin
    MPUWakeUp;                                     {Wake up}
    SetReg(DefAddr, a, btnWrAdr.Tag and $FF);
    lblError.Caption:=IntToStr(btnWrAdr.Tag)+' written to '+IntToStr(a);
  end;
end;

procedure TForm1.btnWrPrevClick(Sender: TObject);
var
  i, b: byte;

begin
  Timer1.Enabled:=false;
  RegHeader;
  for i:=1 to 4 do begin
    b:=StrToIntDef(gridReg.Cells[6, i], 0);
    SetReg(DefAddr, i+12, b);
  end;
  for i:=5 to 8 do begin
    b:=StrToIntDef(gridReg.Cells[6, i], 0);
    SetReg(DefAddr, i+20, b);
  end;
  for i:=9 to 26 do begin                          {I2C registers}
    b:=StrToIntDef(gridReg.Cells[6, i], 0);
    SetReg(DefAddr, i+26, b);
  end;
  b:=StrToIntDef(gridReg.Cells[6, 29], 0);
  SetReg(DefAddr, 55, b);                          {INT_PIN_CFG}
  b:=StrToIntDef(gridReg.Cells[6, 30], 0);
  SetReg(DefAddr, 56, b);                          {INT_ENABLE}

  for i:=70 to 75 do begin
    b:=StrToIntDef(gridReg.Cells[6, i], 0);
    SetReg(DefAddr, i+29, b);
  end;
  b:=StrToIntDef(gridReg.Cells[6, 76], 0);
  SetReg(DefAddr, 106, b);                         {USER_CTRL}
  b:=StrToIntDef(gridReg.Cells[6, 78], 0);
  SetReg(DefAddr, 108, b);                         {PWR_MGMT_2}
  for i:=79 to 81 do begin
    b:=StrToIntDef(gridReg.Cells[6, i], 0);
    SetReg(DefAddr, i+35, b);
  end;
end;

procedure TForm1.btnWrZeroClick(Sender: TObject);  {Write zero to all R/W registers except 107}
var
  i, x, b: byte;

begin
  MPUWakeUp;                                       {Wake up}
  btnWrPrev.Enabled:=false;
  Timer1.Enabled:=false;
  RegHeader;
  for i:=1 to 4 do begin
    SetReg(DefAddr, i+12, 0);
  end;
  for i:=5 to 8 do begin
    SetReg(DefAddr, i+20, 0);
  end;
  for i:=9 to 26 do begin                          {I2C registers}
    SetReg(DefAddr, i+26, 0);
  end;
  SetReg(DefAddr, 55, 0);                          {INT_PIN_CFG}
  SetReg(DefAddr, 56, 0);                          {INT_ENABLE}

  for i:=31 to 69 do begin                         {Read-only registers}
    x:=i+27;
    b:=GetReg(DefAddr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;

  for i:=70 to 75 do begin
    SetReg(DefAddr, i+29, 0);
  end;
  SetReg(DefAddr, 106, 0);                         {USER_CTRL}
  SetReg(DefAddr, 108, 0);                         {PWR_MGMT_2}
  for i:=79 to 81 do begin
    SetReg(DefAddr, i+35, 0);
  end;
  SetReg(DefAddr, 107, rst107);                    {PWR_MGMT_1}
end;

procedure TForm1.edAdrChange(Sender: TObject);     {Check address}
var
  a: integer;
begin
  a:=StrToIntDef(edAdr.Text, 0) and $FF;
  if a in rwregs then begin
    lblError.Caption:=hexidc+IntToHex(a, 2);
    btnWrAdr.Enabled:=true;
  end else begin
    lblError.Caption:='Not a valid R/W register address';
    btnWrAdr.Enabled:=false;
  end;
end;

procedure TForm1.edValueChange(Sender: TObject);   {Check input value}
var
  s: string;
  i: integer;

begin
  s:='';
  for i:=1 to length(edValue.Text) do
    if edValue.Text[i] in ziff then
      s:=s+edValue.Text[i];
  btnWrAdr.Tag:=StrToIntDef(s, 0) and $FF;
  lblHex.Caption:=hexidc+IntToHex(btnWrAdr.Tag, 2);
  lblBin.Caption:=IntToBin(btnWrAdr.Tag, 8);
end;

end.

