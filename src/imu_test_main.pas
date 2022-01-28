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
    btnAddSlave: TButton;
    btnSelftest: TButton;
    btnISTRead: TButton;
    btnISTcyc: TButton;
    btnMPURead: TButton;
    btnMPUcyc: TButton;
    btnClose: TButton;
    btnWrAdr: TButton;
    btnWriteTable: TButton;
    btnSave: TButton;
    btnStop: TButton;
    btnWrZero: TButton;
    btnScan: TButton;
    cbISTdren: TCheckBox;
    cbISTsingle: TCheckBox;
    cbISTSTR: TCheckBox;
    chGyro: TChart;
    chAcc: TChart;
    chMag: TChart;
    chGyroLineX: TLineSeries;
    chAccLineX: TLineSeries;
    chMagLineX: TLineSeries;
    chGyroLineY: TLineSeries;
    chAccLineY: TLineSeries;
    chMagLineY: TLineSeries;
    chGyroLineZ: TLineSeries;
    chAccLineZ: TLineSeries;
    chMagLineZ: TLineSeries;
    edAdr: TEdit;
    edValue: TEdit;
    gridReg: TStringGrid;
    gbWrReg: TGroupBox;
    gbIST8310: TGroupBox;
    gbMPU6050: TGroupBox;
    gbTimer: TGroupBox;
    gbHexBin: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblScan: TLabel;
    lblST: TLabel;
    leDec: TLabeledEdit;
    leHex: TLabeledEdit;
    leBin: TLabeledEdit;
    lblError: TLabel;
    lblBin: TLabel;
    lblHex: TLabel;
    lblTemp: TLabel;
    lblChipAdr: TLabel;
    lblAddr: TLabel;
    ListChartSource1: TListChartSource;
    PageControl: TPageControl;
    rgISTtimer: TRadioGroup;
    rgMPUTimer: TRadioGroup;
    SaveDialog: TSaveDialog;
    TimerST: TTimer;
    tsMag: TTabSheet;
    TimerIST: TTimer;
    tsChartA: TTabSheet;
    tsTools: TTabSheet;
    tsTable: TTabSheet;
    tsChartG: TTabSheet;
    TimerMPU: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnISTReadClick(Sender: TObject);
    procedure btnISTcycClick(Sender: TObject);
    procedure btnMPUcycClick(Sender: TObject);
    procedure btnMPUReadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnSelftestClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnAddSlaveClick(Sender: TObject);
    procedure btnWrAdrClick(Sender: TObject);
    procedure btnWriteTableClick(Sender: TObject);
    procedure btnWrZeroClick(Sender: TObject);
    procedure edAdrChange(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridRegPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure lblAddrClick(Sender: TObject);
    procedure leBinKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure leDecKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure leHexKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgISTtimerClick(Sender: TObject);
    procedure rgMPUTimerClick(Sender: TObject);
    procedure TimerMPUTimer(Sender: TObject);        {Cyclic read register}
    procedure TimerISTTimer(Sender: TObject);
    procedure TimerSTTimer(Sender: TObject);
  private
    procedure CommonRegHdr;                       {Common header for register dump}
    procedure CommonValHdr;                       {Common header read values}
    procedure MPURegHdr;
    procedure MPUValHdr;
    procedure ISTRegHdr;                          {Header IST8310 register}
    procedure ISTValHdr;
    procedure SetTimer;
    procedure RefreshSensor;
  public
  end;

var
  Form1: TForm1;
  fs_sel, afs_sel: byte;
  samples: integer;

type
  TWdVal  = array[0..6] of int16;

const
  tf='0.0';
  gf='0.000';
  df='%2.2d';                                      {Format two digits with leading zero}
  maxSamples=100;
  ziff=['0'..'9'];
  clRW=clMoneyGreen;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CommonRegHdr;                     {Common header for register dump}
begin
  PageControl.ActivePage:=tsTable;
  gridReg.BeginUpdate;
  gridReg.Cells[0, 0]:='Name';
  gridReg.Cells[1, 0]:='Addr hex';
  gridReg.Cells[2, 0]:='Addr dec';
  gridReg.Cells[3, 0]:='';
  gridReg.Cells[4, 0]:='Value bin';
  gridReg.Cells[5, 0]:='Hex';
  gridReg.Cells[6, 0]:='Dec';
  gridReg.EndUpdate;
end;

procedure TForm1.CommonValHdr;                     {Common header for register dump}
begin
  gridReg.BeginUpdate;
  gridReg.Cells[0, 0]:='Value name';
  gridReg.Cells[1, 0]:='Raw hex';
  gridReg.Cells[2, 0]:='Raw dec';
  gridReg.Cells[3, 0]:='';
  gridReg.Cells[4, 0]:='Value';
  gridReg.Cells[5, 0]:='Unit';
  gridReg.Cells[6, 0]:='Info';
  gridReg.EndUpdate;
end;

procedure TForm1.MPURegHdr;                        {Header and register names}
var
  i: byte;

begin
  gridReg.RowCount:=83;
  CommonRegHdr;
  gridReg.BeginUpdate;
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

procedure TForm1.MPUValHdr;                        {Header and shown values}
begin
  CommonValHdr;
  gridReg.BeginUpdate;
  gridReg.RowCount:=8;

  gridReg.Cells[0, 1]:='ACCELERATION_X';
  gridReg.Cells[0, 2]:='ACCELERATION_Y';
  gridReg.Cells[0, 3]:='ACCELERATION_Z';
  gridReg.Cells[0, 4]:='IMU_TEMPERATUR';
  gridReg.Cells[0, 5]:='GYRO_X';
  gridReg.Cells[0, 6]:='GYRO_Y';
  gridReg.Cells[0, 7]:='GYRO_Z';

  gridReg.Cells[5, 1]:='mG';
  gridReg.Cells[5, 2]:=gridReg.Cells[5, 1];
  gridReg.Cells[5, 3]:=gridReg.Cells[5, 1];
  gridReg.Cells[5, 4]:='°C';
  gridReg.Cells[5, 5]:='°/s';
  gridReg.Cells[5, 6]:=gridReg.Cells[5, 5];
  gridReg.Cells[5, 7]:=gridReg.Cells[5, 5];

  gridReg.Cells[6, 1]:='Acc scale';
  gridReg.Cells[6, 2]:=afsToStr(afs_sel);
  gridReg.Cells[6, 3]:='';
  gridReg.Cells[6, 4]:='t/340+36.53°C';
  gridReg.Cells[6, 5]:='Gyro scale';
  gridReg.Cells[6, 6]:=fsToStr(fs_sel);
  gridReg.Cells[6, 7]:='';
  gridReg.EndUpdate;
  MPUWakeUp;                                       {Wake up}
end;

procedure TForm1.ISTRegHdr;                        {Header IST8310 register}
begin
  gridReg.RowCount:=17;
  CommonRegHdr;
  gridReg.BeginUpdate;
  gridReg.Cells[0, 1]:='Who am I';                 {Default $10}
  gridReg.Cells[0, 2]:='Status Register 1';
  gridReg.Cells[0, 3]:='Output Value X_L';
  gridReg.Cells[0, 4]:='Output Value X_H';
  gridReg.Cells[0, 5]:='Output Value Y_L';
  gridReg.Cells[0, 6]:='Output Value Y_H';
  gridReg.Cells[0, 7]:='Output Value Z_L';
  gridReg.Cells[0, 8]:='Output Value Z_H';
  gridReg.Cells[0, 9]:='Status Register 2';
  gridReg.Cells[0, 10]:='Control Register 1';
  gridReg.Cells[0, 11]:='Control Register 2';
  gridReg.Cells[0, 12]:='STR Self_test';
  gridReg.Cells[0, 13]:='Output Value T_L';        { $1C Temperature}
  gridReg.Cells[0, 14]:='Output Value T_H';
  gridReg.Cells[0, 15]:='AVGCNTL';                 { $41}
  gridReg.Cells[0, 16]:='PDCNTL';                  { $42}
  gridReg.EndUpdate;
end;

procedure TForm1.ISTValHdr;                        {Header and shown values}
begin
  CommonValHdr;
  gridReg.BeginUpdate;
  gridReg.RowCount:=5;

  gridReg.Cells[0, 1]:='Magnetometer_X';
  gridReg.Cells[0, 2]:='Magnetometer_Y';
  gridReg.Cells[0, 3]:='Magnetometer_Z';
  gridReg.Cells[0, 4]:='Temperature';

  gridReg.Cells[5, 1]:='raw';
  gridReg.Cells[5, 2]:=gridReg.Cells[5, 1];
  gridReg.Cells[5, 3]:=gridReg.Cells[5, 1];
  gridReg.Cells[5, 4]:='°C';

  gridReg.Cells[6, 1]:='';
  gridReg.Cells[6, 2]:=gridReg.Cells[6, 1];
  gridReg.Cells[6, 3]:=gridReg.Cells[6, 1];
  gridReg.Cells[6, 4]:='t/1000 °C';
  gridReg.EndUpdate;
end;

procedure TForm1.RefreshSensor;                    {Check again if MPU is available}
var
  i: integer;
  ist: boolean;

begin
  btnISTRead.Enabled:=false;                       {Gray out all buttons}
  btnISTcyc.Enabled:=false;
  btnMPURead.Enabled:=false;
  btnMPUcyc.Enabled:=false;
  btnWrZero.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnAddSlave.Enabled:=false;
  ist:=false;

  lbltemp.Caption:='';
  lblChipAdr.Caption:='Nothing found';
  for i:=0 to MaxSamples do begin
    chGyroLineX.AddXY(i, 0);
    chGyroLineY.AddXY(i, 0);
    chGyroLineZ.AddXY(i, 0);
    chAccLineX.AddXY(i, 0);
    chAccLineY.AddXY(i, 0);
    chAccLineZ.AddXY(i, 0);
    chMagLineX.AddXY(i, 0);
    chMagLineY.AddXY(i, 0);
    chMagLineZ.AddXY(i, 0);
  end;
  samples:=0;

  if GetAdrStrIST then begin
    TimerMPU.Enabled:=false;
    lblChipAdr.Caption:=ISTadr;
    if TimerIST.Enabled then                       {Cyclic reading is running}
      PageControl.ActivePage:=tsTable
    else
      ISTRegHdr;
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode for temp}
    lblTemp.Caption:=FormatFloat(tf, GetRegWle(ISTAdr, $1C)/1000)+'°C';
    btnISTRead.Enabled:=true;
    btnISTcyc.Enabled:=true;
    btnWrZero.Enabled:=true;
    btnWrAdr.Enabled:=true;
    ist:=true;
  end;

  if GetAdrStrMPU then begin
    lblChipAdr.Caption:=MPUadr;
    TimerIST.Enabled:=false;
    if TimerMPU.Enabled then                       {Cyclic reading is running}
      PageControl.ActivePage:=tsTable
    else
      MPURegHdr;
    lblTemp.Caption:=TempToStr;
    btnMPURead.Enabled:=true;
    btnMPUcyc.Enabled:=true;
    btnWrZero.Enabled:=true;
    btnWrAdr.Enabled:=true;
    if ist then
      btnAddSlave.Enabled:=true;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);      {Init, settings}
begin
  SetTimer;
  edAdr.Color:=clRW;
  btnWriteTable.Enabled:=false;
  fs_sel:=0;                                       {Default scale factors}
  afs_sel:=0;
  samples:=0;
  Caption:='Read/write register from IMU MPU60x0'; {Init, try MPU6050}
  RefreshSensor;
end;

procedure TForm1.gridRegPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  r: integer;

begin
  if aCol=3 then begin
    if aRow=0 then begin
      gridReg.Canvas.Brush.Color:=gridReg.FixedGridLineColor
    end else
      gridReg.Canvas.Brush.Color:=gridReg.GridLineColor;
  end;

  if (aCol=0) and (gridReg.RowCount>16) then begin {Not for values}
    r:=StrToIntDef(gridReg.Cells[2, aRow], 255);   {Mark R/W yellow}
    if ((r in rwregs) and (lblChipAdr.Caption=MPUadr)) or
       ((r in rwIST) and (lblChipAdr.Caption=ISTadr)) then begin
      gridReg.Canvas.Brush.Color:=clRW;
    end;
  end;
end;

procedure TForm1.lblAddrClick(Sender: TObject);
begin
  RefreshSensor;
end;

procedure TForm1.leBinKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=13 then begin                             {vkReturn}
    leDec.Text:=IntToStr(StrToIntDef('%'+leBin.Text, 0));
    leHex.Text:=IntToHex(StrToIntDef('%'+leBin.Text, 0), 2);
  end;
end;

procedure TForm1.leDecKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=13 then begin                             {vkReturn}
    leHex.Text:=IntToHex(StrToIntDef(leDec.Text, 0), 2);
    leBin.Text:=IntToBin(StrToIntDef(leDec.Text, 0), 8);
  end;
end;

procedure TForm1.leHexKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=13 then begin                             {vkReturn}
    leDec.Text:=IntToStr(StrToIntDef(hexidp+leHex.Text, 0));
    leBin.Text:=IntToBin(StrToIntDef(hexidp+leHex.Text, 0), 8);
  end;
end;

procedure TForm1.SetTimer;
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerMPU.Interval:=StrToInt(rgMPUTimer.Items[rgMPUTimer.ItemIndex]);
  TimerIST.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
end;

procedure TForm1.rgISTtimerClick(Sender: TObject);
begin
  SetTimer;
end;

procedure TForm1.rgMPUTimerClick(Sender: TObject);
begin
  SetTimer;
end;

procedure TForm1.btnMPUReadClick(Sender: TObject); {Read all register}
var
  i, b, x: byte;

begin
  if btnISTcyc.Enabled then                        {Get data from IST8310 too}
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode}

  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  lblChipAdr.Caption:=MPUadr;
  btnWriteTable.Enabled:=true;
  MPURegHdr;
  lblTemp.Caption:=TempToStr;
  gridReg.BeginUpdate;
  for i:=1 to 4 do begin
    x:=i+12;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=5 to 8 do begin
    x:=i+20;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=9 to 30 do begin
    x:=i+26;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=31 to 69 do begin
    x:=i+27;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=70 to 75 do begin
    x:=i+29;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=76 to 78 do begin
    x:=i+30;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
  for i:=79 to 82 do begin
    x:=i+35;
    b:=GetReg(MPUadr, x);
    gridReg.Cells[1, i]:=IntToHex(x, 2);
    gridReg.Cells[2, i]:=Format(df, [x]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;
//  gridReg.TopRow:=31;                            {Jump to acc values}
  gridReg.EndUpdate;
  btnWriteTable.Enabled:=true;
end;

procedure TForm1.btnMPUcycClick(Sender: TObject);  {Start reading values}
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  lblChipAdr.Caption:=MPUadr;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  fs_sel:=GetFS_SEL;                               {Gyro Scale 0..3}
  afs_sel:=GetAFS_SEL;                             {Acc Scale 0..3}
  if PageControl.ActivePage=tsTools then
    PageControl.ActivePage:=tsTable;
  MPUValHdr;
  samples:=0;
  TimerMPU.Enabled:=true;
end;

procedure TForm1.TimerMPUTimer(Sender: TObject);   {Cyclic read values Acc/Temp/Gyro}
var
  i, a: byte;
  varr: TWdVal;

begin
  if btnISTcyc.Enabled then                        {Get data from IST8310 too}
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode}

  a:=59;                                           {Register for Acc}
  for i:=0 to 6 do begin                           {Read from MPU Acc, Temp, Gyro}
    varr[i]:=GetRegWbe(MPUadr, a);
    a:=a+2;
  end;

  if PageControl.ActivePage=tsTable then begin     {Write to table}
    gridReg.BeginUpdate;                           {Values raw}

    for i:=0 to 2 do begin                         {Acceleration}
      gridReg.Cells[1, i+1]:=IntToHex(varr[i], 4);
      gridReg.Cells[2, i+1]:=IntToStr(varr[i]);
      gridReg.Cells[4, i+1]:=FormatFloat(tf, ConvAcc(afs_sel, varr[i], true));
    end;
    gridReg.Cells[1, 4]:=IntToHex(varr[3], 4);
    gridReg.Cells[2, 4]:=IntToStr(varr[3]);
    gridReg.Cells[4, 4]:=FormatFloat(tf, ConvTemp(varr[3]));
    lblTemp.Caption:=gridReg.Cells[4, 4]+'°C';     {Temperature}
    for i:=4 to 6 do begin                         {Gyroscope}
      gridReg.Cells[1, i+1]:=IntToHex(varr[i], 4);
      gridReg.Cells[2, i+1]:=IntToStr(varr[i]);
      gridReg.Cells[4, i+1]:=FormatFloat(gf, ConvGyro(fs_sel, varr[i]));
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

procedure TForm1.TimerISTTimer(Sender: TObject);
var
  w: int16;
  i: byte;

begin
  if cbISTsingle.Checked then
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode}
  if PageControl.ActivePage=tsTable then begin     {Write to table}

    gridReg.BeginUpdate;

    for i:=1 to 3 do begin                         {Magnetometer}
      w:=GetRegWle(ISTAdr, i*2+1);
      gridReg.Cells[1, i]:=IntToHex(w, 4);
      gridReg.Cells[2, i]:=IntToStr(w);
      gridReg.Cells[4, i]:=FormatFloat(tf, w);
    end;

    w:=GetRegWle(ISTAdr, $1C);                     {Temperature}
    gridReg.Cells[1, 4]:=IntToHex(w, 4);
    gridReg.Cells[2, 4]:=IntToStr(w);
    gridReg.Cells[4, 4]:=FormatFloat(tf, w/1000);

    gridReg.EndUpdate;
  end;

  if PageControl.ActivePage=tsMag then begin       {Compass}
    chMagLineX.SetYValue(samples, GetRegWle(ISTAdr, 3));
    chMagLineY.SetYValue(samples, GetRegWle(ISTAdr, 5));
    chMagLineZ.SetYValue(samples, GetRegWle(ISTAdr, 7));
    w:=GetRegWle(ISTAdr, $1C);                     {Temperature}
  end;

  lblTemp.Caption:=FormatFloat(tf, w/1000)+'°C';
  inc(samples);
  if samples>MaxSamples then
    samples:=0;
end;

procedure TForm1.TimerSTTimer(Sender: TObject);
begin
  case btnSelfTest.Tag of
    0: begin
      TimerST.Tag:=GetReg(MPUadr, 28);             {Store previous setting}
      TimerST.Interval:=5000;
      lblST.Caption:='Acc X-axis, 2G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(0);
      SetReg(MPUadr, 28, %10000000);
    end;
    1: begin
      lblST.Caption:='Acc x-axis, 4G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(1);
      SetReg(MPUadr, 28, %10001000);
    end;
    2: begin
      lblST.Caption:='Acc X-axis, 8G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(2);
      SetReg(MPUadr, 28, %10010000);
    end;
    3: begin
      lblST.Caption:='Acc X-axis, 16G';
      SetReg(MPUadr, 28, %10011000);
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(3);
    end;

    4: begin
      lblST.Caption:='Acc Y-axis, 2G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(0);
      SetReg(MPUadr, 28, %01000000);
    end;
    5: begin
      lblST.Caption:='Acc Y-axis, 4G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(1);
      SetReg(MPUadr, 28, %01001000);
    end;
    6: begin
      lblST.Caption:='Acc Y-axis, 8G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(2);
      SetReg(MPUadr, 28, %01010000);
    end;
    7: begin
      lblST.Caption:='Acc Y-axis, 16G';
      SetReg(MPUadr, 28, %01011000);
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(3);
    end;

    8: begin
      lblST.Caption:='Acc Z-axis, 2G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(0);
      SetReg(MPUadr, 28, %00100000);
    end;
    9: begin
      lblST.Caption:='Acc Z-axis, 4G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(1);
      SetReg(MPUadr, 28, %00101000);
    end;
    10: begin
      lblST.Caption:='Acc Z-axis, 8G';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(2);
      SetReg(MPUadr, 28, %00110000);
    end;
    11: begin
      lblST.Caption:='Acc Z-axis, 16G';
      SetReg(MPUadr, 28, %00111000);
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(3);
    end;

    12: begin
      SetReg(MPUadr, 28, TimerST.Tag and $18);     {Restore previous Acc setting}
      TimerST.Tag:=GetReg(MPUadr, 27);             {Store previous Gyro setting}
      lblST.Caption:='Gyro X-axis, 250°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(0);
      SetReg(MPUadr, 27, %10000000);
    end;
    13: begin
      lblST.Caption:='Gyro X-axis, 500°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(1);
      SetReg(MPUadr, 27, %10001000);
    end;
    14: begin
      lblST.Caption:='Gyro X-axis, 1000°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(2);
      SetReg(MPUadr, 27, %10010000);
    end;
    15: begin
      lblST.Caption:='Gyro X-axis, 2000°/s';
      SetReg(MPUadr, 27, %10011000);
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(3);
    end;

    16: begin
      lblST.Caption:='Gyro Y-axis, 250°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(0);
      SetReg(MPUadr, 27, %01000000);
    end;
    17: begin
      lblST.Caption:='Gyro Y-axis, 500°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(1);
      SetReg(MPUadr, 27, %01001000);
    end;
    18: begin
      lblST.Caption:='Gyro Y-axis, 1000°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(2);
      SetReg(MPUadr, 27, %01010000);
    end;
    19: begin
      lblST.Caption:='Gyro Y-axis, 2000°/s';
      SetReg(MPUadr, 27, %01011000);
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(3);
    end;

    20: begin
      lblST.Caption:='Gyro Z-axis, 250°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(0);
      SetReg(MPUadr, 27, %00100000);
    end;
    21: begin
      lblST.Caption:='Gyro Z-axis, 500°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(1);
      SetReg(MPUadr, 27, %00101000);
    end;
    22: begin
      lblST.Caption:='Gyro Z-axis, 1000°/s';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(2);
      SetReg(MPUadr, 27, %00110000);
    end;
    23: begin
      lblST.Caption:='Gyro Z-axis, 2000°/s';
      SetReg(MPUadr, 27, %00111000);
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 6]:=fsToStr(3);
    end;

    24: begin                                      {Stop self test}
      TimerST.Enabled:=false;
      TimerMPU.Enabled:=false;
      SetReg(MPUadr, 27, TimerST.Tag and $18);     {Restore previous Gyro setting}
      lblST.Caption:='Self test done';
      gridReg.Cells[0, 0]:=lblST.Caption;
      gridReg.Cells[6, 2]:=afsToStr(GetAFS_SEL);
      gridReg.Cells[6, 6]:=fsToStr(GetFS_SEL);
    end;
  end;
  btnSelfTest.Tag:=btnSelfTest.Tag+1;              {Next step}
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  Close;
end;

procedure TForm1.btnISTReadClick(Sender: TObject);
var
  b, i: byte;

begin
  btnWriteTable.Enabled:=true;
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  lblChipAdr.Caption:=ISTAdr;
  ISTRegHdr;
  SetReg(ISTAdr, 10, 1);                           {Single measurement mode for temp}
  lblTemp.Caption:=FormatFloat(tf, GetRegWle(ISTAdr, $1C)/1000)+'°C';
  PageControl.ActivePage:=tsTable;
  ISTreset;                                        {Control register2 Soft reset}
  if cbISTsingle.Checked then
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode}
  if cbISTdren.Checked then
    SetReg(ISTAdr, 11, 8);                         {DREN}
  if cbISTSTR.Checked then
    SetReg(ISTAdr, 12, 64);                        {Self test mode}

  gridReg.BeginUpdate;

  b:=GetReg(ISTAdr, 0);
  gridReg.Cells[1, 1]:='00';
  gridReg.Cells[2, 1]:=gridReg.Cells[1, 1];
  gridReg.Cells[4, 1]:=IntToBin(b, 8);
  gridReg.Cells[5, 1]:=IntToHex(b, 2);
  gridReg.Cells[6, 1]:=Format(df, [b]);

  for i:=2 to 12 do begin
    b:=GetReg(ISTAdr, i);
    gridReg.Cells[1, i]:=IntToHex(i, 2);
    gridReg.Cells[2, i]:=Format(df, [i]);
    gridReg.Cells[4, i]:=IntToBin(b, 8);
    gridReg.Cells[5, i]:=IntToHex(b, 2);
    gridReg.Cells[6, i]:=Format(df, [b]);
  end;

  b:=GetReg(ISTAdr, $1C);                          {Temperature}
  gridReg.Cells[1, 13]:='1C';
  gridReg.Cells[2, 13]:='28';
  gridReg.Cells[4, 13]:=IntToBin(b, 8);
  gridReg.Cells[5, 13]:=IntToHex(b, 2);
  gridReg.Cells[6, 13]:=Format(df, [b]);
  b:=GetReg(ISTAdr, $1D);
  gridReg.Cells[1, 14]:='1D';
  gridReg.Cells[2, 14]:='29';
  gridReg.Cells[4, 14]:=IntToBin(b, 8);
  gridReg.Cells[5, 14]:=IntToHex(b, 2);
  gridReg.Cells[6, 14]:=Format(df, [b]);

  b:=GetReg(ISTAdr, $41);                          {AVG control}
  gridReg.Cells[1, 15]:='41';
  gridReg.Cells[2, 15]:='65';
  gridReg.Cells[4, 15]:=IntToBin(b, 8);
  gridReg.Cells[5, 15]:=IntToHex(b, 2);
  gridReg.Cells[6, 15]:=Format(df, [b]);
  b:=GetReg(ISTAdr, $42);                          {PD control}
  gridReg.Cells[1, 16]:='42';
  gridReg.Cells[2, 16]:='66';
  gridReg.Cells[4, 16]:=IntToBin(b, 8);
  gridReg.Cells[5, 16]:=IntToHex(b, 2);
  gridReg.Cells[6, 16]:=Format(df, [b]);

  gridReg.EndUpdate;
end;

procedure TForm1.btnISTcycClick(Sender: TObject);
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  lblChipAdr.Caption:=ISTAdr;
  ISTValHDR;
  PageControl.ActivePage:=tsTable;
  ISTreset;                                      {Control register2 Soft reset}
  if cbISTdren.Checked then
    SetReg(ISTAdr, 11, 8);                       {DREN}
  TimerIST.Enabled:=true;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  SaveDialog.FileName:='Register_'+lblChipAdr.Caption+'_1.csv';
  if SaveDialog.Execute then
    gridReg.SaveToCSVFile(SaveDialog.FileName, ';');
end;

procedure TForm1.btnScanClick(Sender: TObject);     {Look for active i2c addresses}
begin
  Screen.Cursor:=crHourglass;
  lblScan.Caption:='';
  lblScan.Refresh;
  try
    lblScan.Caption:=ScanI2C;
    if lblScan.Caption='' then
      lblScan.Caption:='none';
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.btnSelftestClick(Sender: TObject);  {Start self test}
begin
  if btnMPUread.Enabled then begin
    TimerIST.Enabled:=false;
    lblChipAdr.Caption:=MPUadr;
    btnWriteTable.Enabled:=false;
    btnWrAdr.Enabled:=false;
    PageControl.ActivePage:=tsTable;
    MPUValHdr;
    samples:=0;
    btnSelfTest.Tag:=0;
    TimerST.Interval:=200;
    TimerST.Enabled:=true;
    TimerMPU.Enabled:=true;
  end;
end;

procedure TForm1.btnStopClick(Sender: TObject);    {Stop all cyclic work}
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerST.Enabled:=false;                          {Self test timer}
  btnWrAdr.Enabled:=true;
end;

procedure TForm1.btnAddSlaveClick(Sender: TObject);
var
  b: byte;

begin
  b:=$0E;                                          {Address of IST8310}
  b:=b or $80;                                     {Read access}
  SetReg(MPUadr, 37, b);
  SetReg(MPUadr, 38, 3);
  b:=6;                                            {Read 6 bytes}
  b:=b or $80;                                     {I2C enabled for Slave0}
  b:=b or $40;                                     {Byte swapping (to little endian)}
  SetReg(MPUadr, 39, b);
end;

procedure TForm1.edAdrChange(Sender: TObject);     {Check address}
var
  a: integer;
begin
  a:=StrToIntDef(edAdr.Text, 0) and $FF;
  if ((a in rwregs) and (lblChipAdr.Caption=MPUadr)) or
     ((a in rwIST) and (lblChipAdr.Caption=ISTadr)) then begin
    lblError.Caption:=hexidc+IntToHex(a, 2);
    btnWrAdr.Enabled:=true;
  end else begin
    lblError.Caption:='Invalid register address';
    btnWrAdr.Enabled:=false;
  end;
end;

procedure TForm1.btnWrAdrClick(Sender: TObject);   {Write one byte to a register}
var
  a: integer;

begin
  a:=StrToIntDef(edAdr.Text, 0) and $FF;
  if (a in rwregs) and (lblChipAdr.Caption=MPUadr) then
    MPUWakeUp;                                     {Wake up MPU6050}
  if (a in rwIST) and (lblChipAdr.Caption=ISTadr) then
    ISTreset;                                      {Reset IST8310}
  SetReg(lblChipAdr.Caption, a, btnWrAdr.Tag and $FF);
  lblError.Caption:=IntToStr(btnWrAdr.Tag)+' written to '+IntToStr(a);
end;

procedure TForm1.btnWriteTableClick(Sender: TObject);
var
  i, b: byte;

begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;

  if lblChipAdr.Caption=MPUadr then begin          {MPU6050}
    MPURegHdr;
    for i:=1 to 4 do begin
      b:=StrToIntDef(gridReg.Cells[6, i], 0);
      SetReg(MPUadr, i+12, b);
    end;
    for i:=5 to 8 do begin
      b:=StrToIntDef(gridReg.Cells[6, i], 0);
      SetReg(MPUadr, i+20, b);
    end;
    for i:=9 to 26 do begin                        {I2C registers}
      b:=StrToIntDef(gridReg.Cells[6, i], 0);
      SetReg(MPUadr, i+26, b);
    end;
    b:=StrToIntDef(gridReg.Cells[6, 29], 0);
    SetReg(MPUadr, 55, b);                         {INT_PIN_CFG}
    b:=StrToIntDef(gridReg.Cells[6, 30], 0);
    SetReg(MPUadr, 56, b);                         {INT_ENABLE}

    for i:=70 to 75 do begin
      b:=StrToIntDef(gridReg.Cells[6, i], 0);
      SetReg(MPUadr, i+29, b);
    end;
    b:=StrToIntDef(gridReg.Cells[6, 76], 0);
    SetReg(MPUadr, 106, b);                        {USER_CTRL}
    b:=StrToIntDef(gridReg.Cells[6, 78], 0);
    SetReg(MPUadr, 108, b);                        {PWR_MGMT_2}
    for i:=79 to 81 do begin
      b:=StrToIntDef(gridReg.Cells[6, i], 0);
      SetReg(MPUadr, i+35, b);
    end;
  end;

  if lblChipAdr.Caption=ISTadr then begin          {IST8310}
    for i:=10 to 12 do                             {Control register, Self test}
      SetReg(ISTadr, i, StrToIntDef(gridReg.Cells[6, i], 0));
    SetReg(ISTadr, 65, StrToIntDef(gridReg.Cells[6, 15], 0));
    SetReg(ISTadr, 66, StrToIntDef(gridReg.Cells[6, 16], 0));
  end;
end;

procedure TForm1.btnWrZeroClick(Sender: TObject);  {Write zero to all R/W registers}
var
  i, x, b: byte;

begin
  btnWriteTable.Enabled:=false;
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  if lblChipAdr.Caption=MPUadr then begin          {MPU6050}
    MPUWakeUp;                                     {Wake up}
    MPUregHdr;
    for i:=1 to 4 do begin
      SetReg(MPUadr, i+12, 0);
    end;
    for i:=5 to 8 do begin
      SetReg(MPUadr, i+20, 0);
    end;
    for i:=9 to 26 do begin                        {I2C registers}
      SetReg(MPUadr, i+26, 0);
    end;
    SetReg(MPUadr, 55, 0);                         {INT_PIN_CFG}
    SetReg(MPUadr, 56, 0);                         {INT_ENABLE}

    for i:=31 to 69 do begin                       {Read-only registers}
      x:=i+27;
      b:=GetReg(MPUadr, x);
      gridReg.Cells[1, i]:=IntToHex(x, 2);
      gridReg.Cells[2, i]:=Format(df, [x]);
      gridReg.Cells[4, i]:=IntToBin(b, 8);
      gridReg.Cells[5, i]:=IntToHex(b, 2);
      gridReg.Cells[6, i]:=Format(df, [b]);
    end;

    for i:=70 to 75 do begin
      SetReg(MPUadr, i+29, 0);
    end;
    SetReg(MPUadr, 106, 0);                        {USER_CTRL}
    SetReg(MPUadr, 108, 0);                        {PWR_MGMT_2}
    for i:=79 to 81 do begin
      SetReg(MPUadr, i+35, 0);
    end;
    SetReg(MPUadr, 107, rst107);                   {PWR_MGMT_1, has default $40}
  end;

  if lblChipAdr.Caption=ISTadr then begin          {IST8310}
    for i:=10 to 12 do                             {Control register, Self test}
      SetReg(ISTadr, i, 0);
    SetReg(ISTadr, 65, 0);                         { $41 Avarage timer control}
    SetReg(ISTadr, 66, 0);                         { $42 Pulse duration control}
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

