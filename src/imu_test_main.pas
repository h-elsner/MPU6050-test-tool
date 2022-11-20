{********************************************************}
{                                                        }
{     Test common sensors on I²C bus at Raspberry PI     }
{                                                        }
{       Copyright (c) 2019         Helmut Elsner         }
{                                                        }
{       Compiler: FPC 3.2.2   /    Lazarus 2.2.0         }
{                                                        }
{ Pascal programmers tend to plan ahead, they think      }
{ before they type. We type a lot because of Pascal      }
{ verboseness, but usually our code is right from the    }
{ start. We end up typing less because we fix less bugs. }
{           [Jorge Aldo G. de F. Junior]                 }
{********************************************************}


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


IMU Pitch/Roll:

https://create.arduino.cc/projecthub/MissionCritical/mpu-6050-tutorial-how-to-program-mpu-6050-with-arduino-aee39a

*)


unit imu_test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, ComCtrls, MKnob, TAGraph, TASources, TASeries, strutils, mpu_ctrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddSlave: TButton;
    btnAS5Reg: TButton;
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
    btnADCstart: TButton;
    btnADCstop: TButton;
    btnSinus: TButton;
    btnADCSingle: TButton;
    cbISTdren: TCheckBox;
    cbISTsingle: TCheckBox;
    cbISTSTR: TCheckBox;
    chADC: TChart;
    ADCin0: TLineSeries;
    ADCin1: TLineSeries;
    ADCin2: TLineSeries;
    ADCin3: TLineSeries;
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
    gbAS5: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblDAC: TLabel;
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
    knDAC: TmKnob;
    PageControl: TPageControl;
    rgADCChan: TRadioGroup;
    rgISTtimer: TRadioGroup;
    rgMPUTimer: TRadioGroup;
    SaveDialog: TSaveDialog;
    gridADC: TStringGrid;
    TimerADC: TTimer;
    tsADC: TTabSheet;
    TimerST: TTimer;
    tsMag: TTabSheet;
    TimerIST: TTimer;
    tsChartA: TTabSheet;
    tsTools: TTabSheet;
    tsTable: TTabSheet;
    tsChartG: TTabSheet;
    TimerMPU: TTimer;
    procedure btnADCSingleClick(Sender: TObject);
    procedure btnADCstartClick(Sender: TObject);
    procedure btnADCstopClick(Sender: TObject);
    procedure btnAS5RegClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnISTReadClick(Sender: TObject);
    procedure btnISTcycClick(Sender: TObject);
    procedure btnMPUcycClick(Sender: TObject);
    procedure btnMPUReadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnSelftestClick(Sender: TObject);
    procedure btnSinusClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnAddSlaveClick(Sender: TObject);
    procedure btnWrAdrClick(Sender: TObject);
    procedure btnWriteTableClick(Sender: TObject);
    procedure btnWrZeroClick(Sender: TObject);
    procedure edAdrChange(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridADCMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure gridRegPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure knDACChange(Sender: TObject; AValue: Longint);
    procedure lblAddrClick(Sender: TObject);
    procedure leBinKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure leDecKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure leHexKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgISTtimerClick(Sender: TObject);
    procedure rgMPUTimerClick(Sender: TObject);
    procedure TimerADCTimer(Sender: TObject);
    procedure TimerMPUTimer(Sender: TObject);     {Cyclic read register}
    procedure TimerISTTimer(Sender: TObject);
    procedure TimerSTTimer(Sender: TObject);
  private
    procedure CommonRegHdr;                       {Common header for register dump}
    procedure CommonValHdr;                       {Common header read values}
    procedure MPURegHdr;
    procedure MPUValHdr;
    procedure ISTRegHdr;                          {Header IST8310 register}
    procedure HMCRegHdr;                          {Header HMC5883 register}
    procedure ISTValHdr;
    procedure DACValHdr;                          {DAC header for value dump}
    procedure SetTimer;
    procedure RefreshSensor;
    procedure SetVolt(w: byte);                   {Send voltage to DAC}
    procedure ADCstop;                            {Stop cyclic measuremen}
  public
  end;

var
  Form1: TForm1;
  fs_sel, afs_sel: byte;
  samples: integer;
  CompAdr:string;

type
  TWdVal  = array[0..6] of int16;

const
  tf='0.0';
  gf='0.000';
  df='%2.2d';                                      {Format two digits with leading zero}
  maxSamples=100;
  ziff=['0'..'9'];
  clRW=clMoneyGreen;
  appHdr='Read/write register from';
  tab1=' ';
  tempunit='°C';

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

procedure TForm1.DACValHdr;                        {DAC header for value dump}
begin
  gridADC.BeginUpdate;
  gridADC.Cells[0, 0]:='Channel';
  gridADC.Cells[1, 0]:='Raw hex';
  gridADC.Cells[2, 0]:='Raw dec';
  gridADC.Cells[3, 0]:='Voltage';
  gridADC.Cells[0, 1]:='AIN0';                     {LDR}
  gridADC.Cells[0, 2]:='AIN1';                     {Temp}
  gridADC.Cells[0, 3]:='AIN2';                     {frei}
  gridADC.Cells[0, 4]:='AIN3';                     {Poti}
  gridADC.Cells[0, 5]:='AOUT';                     {DAC}
  gridADC.EndUpdate;
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
  gridReg.Cells[5, 4]:=tempunit;
  gridReg.Cells[5, 5]:='°/s';
  gridReg.Cells[5, 6]:=gridReg.Cells[5, 5];
  gridReg.Cells[5, 7]:=gridReg.Cells[5, 5];

  gridReg.Cells[6, 1]:='Acc scale';
  gridReg.Cells[6, 2]:=afsToStr(afs_sel);
  gridReg.Cells[6, 3]:='';
  gridReg.Cells[6, 4]:='t/340+36.53'+tempunit;
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

procedure TForm1.HMCRegHdr;                        {Header HMC5883 register}
begin
  gridReg.RowCount:=14;
  CommonRegHdr;
  gridReg.BeginUpdate;
  gridReg.Cells[0, 1]:='Configuration Register A';
  gridReg.Cells[0, 2]:='Configuration Register B';
  gridReg.Cells[0, 3]:='Mode Register';
  gridReg.Cells[0, 4]:='Data Output X MSB Register';
  gridReg.Cells[0, 5]:='Data Output X LSB Register';
  gridReg.Cells[0, 6]:='Data Output Z MSB Register';
  gridReg.Cells[0, 7]:='Data Output Z LSB Register';
  gridReg.Cells[0, 8]:='Data Output Y MSB Register';
  gridReg.Cells[0, 9]:='Data Output Y LSB Register';
  gridReg.Cells[0, 10]:='Status Register';
  gridReg.Cells[0, 11]:='Identification Register A';   {ASCII H}
  gridReg.Cells[0, 12]:='Identification Register B';   {ASCII 4}
  gridReg.Cells[0, 13]:='Identification Register C';   {ASCII 3}
  gridReg.EndUpdate;
end;

procedure TForm1.ISTValHdr;                        {Header and shown values}
var
  i: byte;

begin
  CommonValHdr;
  gridReg.BeginUpdate;
  gridReg.RowCount:=5;

  gridReg.Cells[0, 1]:='Magnetometer_X';
  gridReg.Cells[0, 2]:='Magnetometer_Y';
  gridReg.Cells[0, 3]:='Magnetometer_Z';
  gridReg.Cells[0, 4]:='Temperature';

  gridReg.Cells[5, 1]:='raw';
  gridReg.Cells[6, 1]:='';
  gridReg.Cells[6, 2]:=gridReg.Cells[6, 1];
  gridReg.Cells[6, 3]:=gridReg.Cells[6, 1];
  if CompAdr=HMCadr then begin
    for i:=1 to 5 do
      gridReg.Cells[i, 4]:='';
    gridReg.Cells[5, 1]:='mGauss';
    gridReg.Cells[6, 4]:='n/a'
  end else begin
    gridReg.Cells[5, 4]:=tempunit;
    gridReg.Cells[6, 4]:='t/1000 '+tempunit;
  end;
  gridReg.Cells[5, 2]:=gridReg.Cells[5, 1];
  gridReg.Cells[5, 3]:=gridReg.Cells[5, 1];
  gridReg.EndUpdate;
end;

procedure TForm1.RefreshSensor;                    {Check again if MPU is available}
var
  i: integer;
  ist: boolean;

begin
  lblTemp.Caption:='';
  lblChipAdr.Caption:='Nothing found';

  btnISTRead.Enabled:=false;                       {Gray out all buttons}
  btnISTcyc.Enabled:=false;
  btnMPURead.Enabled:=false;
  btnMPUcyc.Enabled:=false;
  btnWrZero.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnAddSlave.Enabled:=false;
  gbIST8310.Enabled:=false;
  gbMPU6050.Enabled:=false;
  gbAS5.Enabled:=false;
  ist:=false;
  knDac.Enabled:=false;

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
    ADCin0.AddXY(i, 0);
    ADCin1.AddXY(i, 0);
    ADCin2.AddXY(i, 0);
    ADCin3.AddXY(i, 0);
  end;
  samples:=0;

  if GetAdrStrIST then begin
    TimerMPU.Enabled:=false;
    CompAdr:=ISTadr;
    lblChipAdr.Caption:=CompAdr;
    if TimerIST.Enabled then                       {Cyclic reading is running}
      PageControl.ActivePage:=tsTable
    else
      ISTRegHdr;
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode for temp}
    lblTemp.Caption:=FormatFloat(tf, GetRegWle(ISTAdr, $1C)/1000)+tempunit;
    btnISTRead.Enabled:=true;
    btnISTcyc.Enabled:=true;
    btnWrZero.Enabled:=true;
    btnWrAdr.Enabled:=true;
    ist:=true;
    gbIST8310.Enabled:=true;
  end;

  if GetAdrStrHMC then begin
    CompAdr:=HMCadr;
    lblChipAdr.Caption:=CompAdr;
    btnISTRead.Enabled:=true;
    btnISTcyc.Enabled:=true;
    btnWrZero.Enabled:=true;
    btnWrAdr.Enabled:=true;
    ist:=true;                                     {Add as slave is possible}
  end;

  if GetAdrStrAS5 then begin
    lblChipAdr.Caption:=AS5Adr;
    gbAS5.Enabled:=true;
    ist:=true;
    if CompAdr<>'' then
      CompAdr:=CompAdr+tab1+AS5Adr
    else
      CompAdr:=AS5Adr;
  end;

  tsADC.Tag:=XtoByte(ADCadr);
  for i:=0 to 7 do begin
    if GetAdrStrADC(hexidc+IntToHex(tsADC.Tag+i, 2)) then begin
      tsADC.Tag:=tsADC.Tag+i;                      {Save valid address}
      knDac.Enabled:=true;
      if CompAdr<>'' then
        CompAdr:=CompAdr+tab1+hexidc+IntToHex(tsADC.Tag, 2)
      else begin
        CompAdr:=hexidc+IntToHex(tsADC.Tag, 2);
      end;
      SetVolt(0);
      lblChipAdr.Caption:=CompAdr;
      caption:=AppHdr+tab1+AdrToChip(hexidc+IntToHex(tsADC.Tag, 2));
      exit;
    end;
  end;

  if GetAdrStrMPU then begin
    lblChipAdr.Caption:=MPUadr;
    caption:=AppHdr+AdrToChip(MPUadr);
    gbMPU6050.Enabled:=true;
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
    if ist then begin
      btnAddSlave.Enabled:=true;
      caption:=caption+tab1+AdrToChip(CompAdr)
    end;
  end else
    if ist then
      caption:=AppHdr+tab1+AdrToChip(CompAdr);
end;

procedure TForm1.FormCreate(Sender: TObject);      {Init, settings}
begin
  SetTimer;
  ADCstop;
  edAdr.Color:=clRW;
  btnWriteTable.Enabled:=false;
  fs_sel:=0;                                       {Default scale factors}
  afs_sel:=0;
  samples:=0;
  CompAdr:='';
  lblTemp.Caption:='';
  Caption:=AppHdr+tab1+intfac;                     {Init, try sensors}
  RefreshSensor;
  DACValHdr;                                       {DAC header for value dump}
end;

procedure TForm1.gridADCMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  sp, zl: integer;                                 {Spalte und Zeile}

begin
  zl:=0;
  sp:=0;
  gridADC.MouseToCell(x, y, sp, zl);               {Zelle unter Maus finden}
  if sp=0 then begin
    case zl of
      1: gridADC.Hint:='AIN0 (LDR) yellow';
      2: gridADC.Hint:='AIN1 (temp) red';
      3: gridADC.Hint:='AIN2 (free) blue';
      4: gridADC.Hint:='AIN3 (poti) green';
    else
      gridADC.Hint:=gridADC.Cells[sp, zl];
    end;
  end else begin
    if zl=0 then begin
      gridADC.Hint:=gridADC.Cells[sp, zl];
    end else begin
      case sp of
        1: gridADC.Hint:='$'+gridADC.Cells[sp, zl];
        2: gridADC.Hint:=gridADC.Cells[sp, zl];
        3: gridADC.Hint:=gridADC.Cells[sp, zl]+' V';
      end;
    end;
  end;
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

  if (aCol=0) and (gridReg.RowCount>9) then begin  {Not for values}
    r:=StrToIntDef(gridReg.Cells[2, aRow], 255);   {Mark R/W green}
    if ((r in rwregs) and (lblChipAdr.Caption=MPUadr)) or
       ((r in rwIST) and (lblChipAdr.Caption=ISTAdr)) or
       ((r in rwHMC) and (lblChipAdr.Caption=HMCAdr)) then begin
      gridReg.Canvas.Brush.Color:=clRW;
    end;
  end;
end;

procedure TForm1.SetVolt(w: byte);
begin
  if SetDAC(hexidc+IntToHex(tsADC.Tag, 2), w) then begin
    gridADC.Cells[2, 5]:=IntToStr(w);
    gridADC.Cells[1, 5]:=IntToHex(w, 2);
    gridADC.Cells[3, 5]:=FormatFloat(gf, GetVolt(w));
  end;
end;

procedure TForm1.knDACChange(Sender: TObject; AValue: Longint);
begin
  if btnSinus.Tag=0 then
    SetVolt(AValue);
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
  TimerADC.Enabled:=false;
  TimerMPU.Interval:=StrToInt(rgMPUTimer.Items[rgMPUTimer.ItemIndex]);
  TimerIST.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
  TimerADC.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
end;

procedure TForm1.rgISTtimerClick(Sender: TObject);
begin
  SetTimer;
end;

procedure TForm1.rgMPUTimerClick(Sender: TObject);
begin
  SetTimer;
end;

procedure TForm1.TimerADCTimer(Sender: TObject);
var
  w: byte;
  adr: string;

begin
  if btnSinus.Tag=1 then begin                     {Sinus über DAC ausgeben}
    w:=round(sin(samples/5)*127+128);
    SetVolt(w);
  end;
  adr:=hexidc+IntToHex(tsADC.Tag, 2);
  gridADC.BeginUpdate;

  w:=ReadADC(adr, 0);
  gridADC.Cells[1, 1]:=IntToHex(w, 2);
  gridADC.Cells[2, 1]:=IntToStr(w);
  gridADC.Cells[3, 1]:=FormatFloat(gf, GetVolt(w));
  ADCin0.SetYValue(samples, GetVolt(w));

  w:=ReadADC(adr, 1);
  gridADC.Cells[1, 2]:=IntToHex(w, 2);
  gridADC.Cells[2, 2]:=IntToStr(w);
  gridADC.Cells[3, 2]:=FormatFloat(gf, GetVolt(w));
  ADCin1.SetYValue(samples, GetVolt(w));

  w:=ReadADC(adr, 2);
  gridADC.Cells[1, 3]:=IntToHex(w, 2);
  gridADC.Cells[2, 3]:=IntToStr(w);
  gridADC.Cells[3, 3]:=FormatFloat(gf, GetVolt(w));
  ADCin2.SetYValue(samples, GetVolt(w));

  w:=ReadADC(adr, 3);
  gridADC.Cells[1, 4]:=IntToHex(w, 2);
  gridADC.Cells[2, 4]:=IntToStr(w);
  gridADC.Cells[3, 4]:=FormatFloat(gf, GetVolt(w));
  ADCin3.SetYValue(samples, GetVolt(w));

  gridADC.EndUpdate;


  inc(samples);
  if samples>MaxSamples then
    samples:=0;
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
    lblTemp.Caption:=gridReg.Cells[4, 4]+tempunit; {Temperature}
    for i:=4 to 6 do begin                         {Gyroscope}
      gridReg.Cells[1, i+1]:=IntToHex(varr[i], 4);
      gridReg.Cells[2, i+1]:=IntToStr(varr[i]);
      gridReg.Cells[4, i+1]:=FormatFloat(gf, ConvGyro(fs_sel, varr[i]));
    end;
    gridReg.EndUpdate;

  end else begin
    lblTemp.Caption:=FormatFloat(tf, ConvTemp(varr[3]))+tempunit;
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
  g: double;
  i, gain: byte;

begin
  gain:=88;                                        {invalid}
  if cbISTsingle.Checked and (CompAdr=ISTAdr) then
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode}
  if CompAdr=HMCAdr then begin
    gain:=getGain;
    gridReg.Cells[6, 1]:=GainToStr(gain);
  end;
  if PageControl.ActivePage=tsTable then begin     {Write to table}

    gridReg.BeginUpdate;

    if CompAdr=ISTadr then begin
      for i:=1 to 3 do begin                       {Magnetometer}
        w:=GetRegWle(ISTAdr, i*2+1);
        gridReg.Cells[1, i]:=IntToHex(w, 4);
        gridReg.Cells[2, i]:=IntToStr(w);
        gridReg.Cells[4, i]:=FormatFloat(tf, w);
      end;

      w:=GetRegWle(ISTAdr, $1C);                   {Temperature}
      gridReg.Cells[1, 4]:=IntToHex(w, 4);
      gridReg.Cells[2, 4]:=IntToStr(w);
      gridReg.Cells[4, 4]:=FormatFloat(tf, w/1000);
    end;

    if CompAdr=HMCadr then begin
      w:=GetRegWle(HMCAdr, 3);                     {X}
      gridReg.Cells[1, 1]:=IntToHex(w, 4);
      gridReg.Cells[2, 1]:=IntToStr(w);
      gridReg.Cells[4, 1]:=FormatFloat(tf, ConvHMC(w, gain));
      w:=GetRegWle(HMCAdr, 7);
      gridReg.Cells[1, 2]:=IntToHex(w, 4);
      gridReg.Cells[2, 2]:=IntToStr(w);
      gridReg.Cells[4, 2]:=FormatFloat(tf, ConvHMC(w, gain));
      w:=GetRegWle(HMCAdr, 5);
      gridReg.Cells[1, 3]:=IntToHex(w, 4);
      gridReg.Cells[2, 3]:=IntToStr(w);
      gridReg.Cells[4, 3]:=FormatFloat(tf, ConvHMC(w, gain));
    end;
    gridReg.EndUpdate;
  end;

  if PageControl.ActivePage=tsMag then begin       {Compass}
    if CompAdr=ISTadr then begin
      chMagLineX.SetYValue(samples, GetRegWle(ISTAdr, 3));
      chMagLineY.SetYValue(samples, GetRegWle(ISTAdr, 5));
      chMagLineZ.SetYValue(samples, GetRegWle(ISTAdr, 7));
      w:=GetRegWle(ISTAdr, $1C);                   {Temperature}
    end;

    if CompAdr=HMCadr then begin
      g:=ConvHMC(GetRegWle(HMCAdr, 3), gain);      {X}
      chMagLineX.SetYValue(samples, g);
      g:=ConvHMC(GetRegWle(HMCAdr, 7), gain);      {Y}
      chMagLineY.SetYValue(samples, g);
      g:=ConvHMC(GetRegWle(HMCAdr, 5), gain);      {Z}
      chMagLineZ.SetYValue(samples, g);
    end;
  end;

  if CompAdr=ISTadr then
    lblTemp.Caption:=FormatFloat(tf, w/1000)+tempunit;

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
      gridReg.Cells[6, 2]:=afsToStr(GetAFS_SEL);
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

procedure TForm1.btnAS5RegClick(Sender: TObject);  {AS5600 register only}
var
  i, b, x: byte;

begin
  gridReg.RowCount:=19;
  CommonRegHdr;
  gridReg.Cells[0, 1]:='Conf ZMO';
  gridReg.Cells[0, 2]:='Conf ZPOS_H';
  gridReg.Cells[0, 3]:='Conf ZPOS_L';
  gridReg.Cells[0, 4]:='Conf MPOS_H';
  gridReg.Cells[0, 5]:='CONF MPOS_L';
  gridReg.Cells[0, 6]:='CONF MANG_H';
  gridReg.Cells[0, 7]:='CONF MANG_L';
  gridReg.Cells[0, 8]:='CONF_H';
  gridReg.Cells[0, 9]:='CONF_L';
  gridReg.Cells[0, 10]:='RAW_ANGLE_H';
  gridReg.Cells[0, 11]:='RAW_ANGLE_L';
  gridReg.Cells[0, 12]:='ANGLE_H';
  gridReg.Cells[0, 13]:='ANGLE_L';
  gridReg.Cells[0, 14]:='STATUS';
  gridReg.Cells[0, 15]:='AGC';
  gridReg.Cells[0, 16]:='MAGNITUDE_H';
  gridReg.Cells[0, 17]:='MAGNITUDE_L';
  gridReg.Cells[0, 18]:='BURN';                    {Burn_Angle=$80, Burn_Setting=$40}

  for i:=0 to 8 do begin
    b:=GetReg(AS5adr, i);                          {Configuration registers}
    x:=i+1;
    gridReg.Cells[1, x]:=IntToHex(i, 2);
    gridReg.Cells[2, x]:=Format(df, [i]);
    gridReg.Cells[4, x]:=IntToBin(b, 8);
    gridReg.Cells[5, x]:=IntToHex(b, 2);
    gridReg.Cells[6, x]:=Format(df, [b]);
  end;
  for i:=12 to 15 do begin                         {Output registers}
    b:=GetReg(AS5adr, i);
    x:=i-2;
    gridReg.Cells[1, x]:=IntToHex(i, 2);
    gridReg.Cells[2, x]:=Format(df, [i]);
    gridReg.Cells[4, x]:=IntToBin(b, 8);
    gridReg.Cells[5, x]:=IntToHex(b, 2);
    gridReg.Cells[6, x]:=Format(df, [b]);
  end;
  b:=GetReg(AS5adr, 11);                           {Status registers}
  gridReg.Cells[1, 14]:='0B';
  gridReg.Cells[2, 14]:='11';
  gridReg.Cells[4, 14]:=IntToBin(b, 8);
  gridReg.Cells[5, 14]:=IntToHex(b, 2);
  gridReg.Cells[6, 14]:=Format(df, [b]);
  for i:=26 to 28 do begin                         {AGC, Magnitude}
    b:=GetReg(AS5adr, i);
    x:=i-11;
    gridReg.Cells[1, x]:=IntToHex(i, 2);
    gridReg.Cells[2, x]:=Format(df, [i]);
    gridReg.Cells[4, x]:=IntToBin(b, 8);
    gridReg.Cells[5, x]:=IntToHex(b, 2);
    gridReg.Cells[6, x]:=Format(df, [b]);
  end;
  b:=GetReg(AS5adr, 255);                          {Burn_Angle=$80, Burn_Setting=$40}
  gridReg.Cells[1, 18]:='FF';
  gridReg.Cells[2, 18]:='255';
  gridReg.Cells[4, 18]:=IntToBin(b, 8);
  gridReg.Cells[5, 18]:=IntToHex(b, 2);
  gridReg.Cells[6, 18]:=Format(df, [b]);
end;

procedure TForm1.ADCstop;                          {Stop cyclic measuremen}
begin
  TimerADC.Enabled:=false;
  btnSinus.Tag:=0;
  knDAC.Enabled:=true;
end;

procedure TForm1.btnADCstopClick(Sender: TObject);
begin
  ADCstop;
end;

procedure TForm1.btnADCstartClick(Sender: TObject);
begin
  btnSinus.Tag:=0;
  knDAC.Enabled:=true;
  TimerADC.Enabled:=true;
end;

procedure TForm1.btnADCSingleClick(Sender: TObject); {ADC single measurement}
var
  i: integer;
  w: byte;

begin
  ADCstop;
  for i:=1 to 4 do
    gridADC.Cells[3, i]:='';                         {Delete volatage to identify what was changed at single measurement}
  w:=ReadADC(hexidc+IntToHex(tsADC.Tag, 2), rgADCchan.ItemIndex);
  gridADC.Cells[1, rgADCchan.ItemIndex+1]:=IntToHex(w, 2);
  gridADC.Cells[2, rgADCchan.ItemIndex+1]:=IntToStr(w);
  gridADC.Cells[3, rgADCchan.ItemIndex+1]:=FormatFloat(gf, GetVolt(w));
end;

procedure TForm1.btnISTReadClick(Sender: TObject);
var
  b, i: byte;

begin
  btnWriteTable.Enabled:=true;
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  lblChipAdr.Caption:=CompAdr;
  PageControl.ActivePage:=tsTable;
  gridReg.BeginUpdate;
  if CompAdr=ISTadr then begin
    ISTRegHdr;
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode for temp}
    lblTemp.Caption:=FormatFloat(tf, GetRegWle(ISTAdr, $1C)/1000)+tempunit;
    ISTreset;                                      {Control register2 Soft reset}
    if cbISTsingle.Checked then
      SetReg(ISTAdr, 10, 1);                       {Single measurement mode}
    if cbISTdren.Checked then
      SetReg(ISTAdr, 11, 8);                         {DREN}
    if cbISTSTR.Checked then
      SetReg(ISTAdr, 12, 64);                        {Self test mode}

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
  end;
  if CompAdr=HMCadr then begin
    HMCRegHdr;
    for i:=0 to 12 do begin
      b:=GetReg(HMCAdr, i);
      gridReg.Cells[1, i+1]:=IntToHex(i, 2);
      gridReg.Cells[2, i+1]:=Format(df, [i]);
      gridReg.Cells[4, i+1]:=IntToBin(b, 8);
      gridReg.Cells[5, i+1]:=IntToHex(b, 2);
      gridReg.Cells[6, i+1]:=Format(df, [b]);
    end;
  end;
  gridReg.EndUpdate;
end;

procedure TForm1.btnISTcycClick(Sender: TObject);
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  lblChipAdr.Caption:=CompAdr;
  ISTValHDR;
  PageControl.ActivePage:=tsTable;
  if CompAdr=ISTadr then begin
    ISTreset;                                      {Control register2 Soft reset}
    if cbISTdren.Checked  then
      SetReg(ISTAdr, 11, 8);                       {DREN}
  end else
    if CompAdr=HMCadr then
      HMCinit;
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

procedure TForm1.btnSinusClick(Sender: TObject);
begin
  TimerADC.Enabled:=true;
  btnSinus.Tag:=1;
  knDAC.Enabled:=false;
end;

procedure TForm1.btnStopClick(Sender: TObject);    {Stop all cyclic work}
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerST.Enabled:=false;                          {Self test timer}
  btnWrAdr.Enabled:=true;
  ADCstop;
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
     ((a in rwIST) and (lblChipAdr.Caption=ISTadr)) or
     ((a in rwHMC) and (lblChipAdr.Caption=HMCadr)) then begin
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
    TimerST.Enabled:=false;
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

  if lblChipAdr.Caption=HMCadr then begin          {HMC5883}
    for i:=0 to 2 do                               {Conf / Mode register}
      SetReg(HMCadr, i, StrToIntDef(gridReg.Cells[6, i+1], 0));
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
    TimerST.Enabled:=false;
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

  if lblChipAdr.Caption=HMCadr then begin          {HMC5883}
    for i:=0 to 2 do                               {Conf / Mode register}
      SetReg(HMCadr, i, 0);
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

