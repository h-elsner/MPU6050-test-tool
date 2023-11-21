{********************************************************}
{                                                        }
{     Test common sensors on I²C bus at Raspberry PI     }
{                                                        }
{       Copyright (c) 2019-2022    Helmut Elsner         }
{                                                        }
{       Compiler: FPC 3.2.2   /    Lazarus 2.3.0         }
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

AS5600 Programming added at 15.11.2023

*)


unit imu_test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, ComCtrls, Menus, MKnob, indGnouMeter, TAGraph, TASources, TASeries,
  strutils, mpu_ctrl;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnAddSlave: TButton;
    btnAS5Reg: TButton;
    btnBurnCali: TButton;
    btnReadCONF: TButton;
    btnScan: TButton;
    btnSelftest: TButton;
    btnISTRead: TButton;
    btnISTcyc: TButton;
    btnMPURead: TButton;
    btnMPUcyc: TButton;
    btnClose: TButton;
    btnOTP: TButton;
    btnWrAdr: TButton;
    btnWriteCONF: TButton;
    btnWriteTable: TButton;
    btnSave: TButton;
    btnStop: TButton;
    btnWrZero: TButton;
    btnADCstart: TButton;
    btnADCstop: TButton;
    btnSinus: TButton;
    btnADCSingle: TButton;
    btnBurnCONF: TButton;
    btnSetZero: TButton;
    btnSetMax: TButton;
    btnReadCycl: TButton;
    cbISTdren: TCheckBox;
    cbISTsingle: TCheckBox;
    cbISTSTR: TCheckBox;
    chADC: TChart;
    ADCin0: TLineSeries;
    ADCin1: TLineSeries;
    ADCin2: TLineSeries;
    ADCin3: TLineSeries;
    cgSensors: TCheckGroup;
    cbMH: TCheckBox;
    cbML: TCheckBox;
    cbMD: TCheckBox;
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
    cbPM: TComboBox;
    cbHYST: TComboBox;
    cbOUTS: TComboBox;
    cbPWMF: TComboBox;
    cbSF: TComboBox;
    cbFTH: TComboBox;
    cbWD: TComboBox;
    edAdr: TEdit;
    edMANGH: TEdit;
    edMANGL: TEdit;
    edMANGD: TEdit;
    edValue: TEdit;
    gridReg: TStringGrid;
    gbWrReg: TGroupBox;
    gbIST8310: TGroupBox;
    gbMPU6050: TGroupBox;
    gbTimer: TGroupBox;
    gbHexBin: TGroupBox;
    gbScan: TGroupBox;
    gbConf: TGroupBox;
    gbCali: TGroupBox;
    slAng: TindGnouMeter;
    slRaw: TindGnouMeter;
    Label1: TLabel;
    Label2: TLabel;
    lblBurnC: TLabel;
    lblMoveM: TLabel;
    lblMoveZ: TLabel;
    lblHexLow: TLabel;
    lblHexHigh: TLabel;
    lblMANG: TLabel;
    lblMstat: TLabel;
    lblWD: TLabel;
    lblFTH: TLabel;
    lblSF: TLabel;
    lblPWMF: TLabel;
    lblOUTS: TLabel;
    lblHYST: TLabel;
    lblPM: TLabel;
    lblAS5: TLabel;
    lblDAC: TLabel;
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
    pnlRed: TPanel;
    pnlRed1: TPanel;
    rgMag: TRadioGroup;
    rgADCChan: TRadioGroup;
    rgISTtimer: TRadioGroup;
    rgMPUTimer: TRadioGroup;
    SaveDialog: TSaveDialog;
    gridADC: TStringGrid;
    gridCali: TStringGrid;
    TimerAS5: TTimer;
    tsAS5600: TTabSheet;
    TimerHMC: TTimer;
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
    procedure btnBurnCaliClick(Sender: TObject);
    procedure btnBurnCONFClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnISTReadClick(Sender: TObject);
    procedure btnISTcycClick(Sender: TObject);
    procedure btnMPUcycClick(Sender: TObject);
    procedure btnMPUReadClick(Sender: TObject);
    procedure btnOTPClick(Sender: TObject);
    procedure btnReadCONFClick(Sender: TObject);
    procedure btnReadCyclClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnSelftestClick(Sender: TObject);
    procedure btnSetMaxClick(Sender: TObject);
    procedure btnSetZeroClick(Sender: TObject);
    procedure btnSinusClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnAddSlaveClick(Sender: TObject);
    procedure btnWrAdrClick(Sender: TObject);
    procedure btnWriteCONFClick(Sender: TObject);
    procedure btnWriteTableClick(Sender: TObject);
    procedure btnWrZeroClick(Sender: TObject);
    procedure cgSensorsItemClick(Sender: TObject; Index: integer);
    procedure edAdrChange(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure gridADCMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure gridCaliPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure gridRegPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure knDACChange(Sender: TObject; AValue: Longint);
    procedure lblAddrClick(Sender: TObject);
    procedure leBinKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure leDecKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure leHexKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PageControlChange(Sender: TObject);
    procedure rgISTtimerClick(Sender: TObject);
    procedure rgMPUTimerClick(Sender: TObject);
    procedure TimerADCTimer(Sender: TObject);      {Cyclic read data ADC}
    procedure TimerAS5Timer(Sender: TObject);
    procedure TimerHMCTimer(Sender: TObject);
    procedure TimerMPUTimer(Sender: TObject);      {Cyclic read register IMU}
    procedure TimerISTTimer(Sender: TObject);
    procedure TimerSTTimer(Sender: TObject);
    procedure FillMANGD(Sender: TObject);          {Show Maximum Angle}
  private
    procedure ReadSensors;                         {Look for active i2c addresses}
    procedure CommonRegHdr;                        {Common header for register dump}
    procedure CommonValHdr;                        {Common header read values}
    procedure MPURegHdr;
    procedure MPUValHdr;
    procedure ISTRegHdr;                           {Header IST8310 register}
    procedure HMCRegHdr;                           {Header HMC5883 register}
    procedure MagValHdr(adr: string);
    procedure DACValHdr;                           {DAC header for value dump}
    procedure SetTimer;
    procedure RefreshSensor;
    procedure SetVolt(w: byte);                    {Send voltage to DAC}
    procedure ADCstop;                             {Stop cyclic measuremen}
    procedure ReadAS5600Reg;                       {AS5600 register only}
    procedure ConfHints;
    function  ConvAngle(d: integer): string;       {ToDo conversion Maximum Angle to °}
    procedure ReadAS5ro;                           {Read As5600 output register}
  public
  end;

var
  Form1: TForm1;
  fs_sel, afs_sel: byte;
  samples, UsedChip: integer;

  {UsedChip: 0 - undef, 1 - MPU, 2 - HMC, 3 - IST, 4 - AS5, 5 - ADC}

type
  TWdVal  = array[0..6] of int16;

const
  tf='0.0';
  gf='0.000';
  df='%2.2d';                                      {Format two digits with leading zero}
  maxSamples=100;
  ziff=['0'..'9'];
  clRW=clMoneyGreen;
  tab1=' ';
  tempunit='°C';
  hexid='$';
  PGOpin=17;                                       {GPIO17 - pin 11}

resourcestring
  appHdr='Read/write register from';
  capMstat='Status';
  hntMStat='Status magnet / AGC';
  hntBurn='Be careful!';
  hntBurn1=' Do it only if you know what you do.';
  hntBurnC='Write configuration permanently into OTP. ';
  hntBurnA='Write angle values permanently into OTP. ';
  hntCali='Read manual for calibration procedure.';


implementation

{$R *.lfm}

{ TForm1 }

function UsedChipToAdr(uc: byte): string;          {UsedChip: 0 - undef, 1 - MPU, 2 - HMC, 3 - IST, 4 - AS5, 5 - ADC}
begin
  result:='0xFF';
  case uc of
    1: result:=MPUadr;
    2: result:=HMCadr;
    3: result:=ISTadr;
    4: result:=AS5adr;
    5: result:=ADCadr;
  end;
end;

procedure TForm1.CommonRegHdr;                     {Common header for register dump}
begin
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

procedure TForm1.MPURegHdr;                        {MPU Header and register names}
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

procedure TForm1.MPUValHdr;                        {MPU Header and shown values}
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

procedure TForm1.ISTRegHdr;                        {Mag Header IST8310 register}
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

procedure TForm1.HMCRegHdr;                        {Mag Header HMC5883 register}
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

procedure TForm1.MagValHdr(adr: string);           {Mag Header and shown values}
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
  if Adr=HMCadr then begin
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

procedure TForm1.RefreshSensor;                    {Check for active and selected Sensors}
var
  i, k: integer;

begin
  if cgSensors.Items.Count>0 then begin
    lblTemp.Caption:='';
    UsedChip:=0;

    btnISTRead.Enabled:=false;                     {Grey out all buttons}
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
    btnAS5Reg.Enabled:=false;
    knDac.Enabled:=false;
    lblAS5.Caption:='';
    tsADC.Enabled:=false;

    for i:=0 to MaxSamples do begin                {Clear all charts}
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
    Caption:=AppHdr+tab1+intfac;                   {Init, try sensors}


    for k:=0 to cgSensors.Items.Count-1 do begin   {Refresh all found sensors}
      if cgSensors.Checked[k] then begin           {but only checked}

        if (pos(MPUadr, cgSensors.Items[k])>0) and GetAdrStrMPU then begin                 {IMU}
          caption:=Caption+tab1+AdrToChip(MPUadr);
          gbMPU6050.Enabled:=true;
          TimerIST.Enabled:=false;
          TimerHMC.Enabled:=false;
          TimerAS5.Enabled:=false;
          MPURegHdr;
          lblTemp.Caption:=TempToStr;
          btnMPURead.Enabled:=true;
          btnMPUcyc.Enabled:=true;
          btnWrZero.Enabled:=true;
          btnWrAdr.Enabled:=true;
          Continue;
        end;

        if (rgMag.ItemIndex=1) and (pos(ISTadr, cgSensors.Items[k])>0) and GetAdrStrIST then begin
          TimerMPU.Enabled:=false;
          TimerAS5.Enabled:=false;
          TimerHMC.Enabled:=false;
          ISTRegHdr;
          SetReg(ISTAdr, 10, 1);                   {Single measurement mode for temp}
          lblTemp.Caption:=FormatFloat(tf, GetRegWle(ISTAdr, $1C)/1000)+tempunit;
          btnISTRead.Enabled:=true;
          btnISTcyc.Enabled:=true;
          btnWrZero.Enabled:=true;
          btnWrAdr.Enabled:=true;
          gbIST8310.Enabled:=true;
          Caption:=Caption+tab1+AdrToChip(ISTadr);
          Continue;
        end;

        if (rgMag.ItemIndex=0) and (pos(HMCadr, cgSensors.Items[k])>0) and GetAdrStrHMC then begin
          TimerMPU.Enabled:=false;
          TimerAS5.Enabled:=false;
          TimerIST.Enabled:=false;
          HMCRegHdr;
          btnISTRead.Enabled:=true;
          btnISTcyc.Enabled:=true;
          btnWrZero.Enabled:=true;
          btnWrAdr.Enabled:=true;
          btnAddSlave.Enabled:=true;
          Caption:=Caption+tab1+AdrToChip(HMCadr);
          Continue;
        end;

        if (pos(AS5adr, cgSensors.Items[k])>0) and GetAdrStrAS5 then begin  {Rotary sensor}
          Caption:=Caption+tab1+AdrToChip(AS5adr);
          btnAS5Reg.Enabled:=true;
          lblAS5.Caption:=AS5adr;
          PageControl.ActivePage:=tsAS5600;
          Continue;
        end;

        tsADC.Tag:=XtoByte(ADCadr);                {ADC addresses}
        for i:=0 to 7 do begin
          if GetAdrStrADC(hexidc+IntToHex(tsADC.Tag+i, 2)) then begin
            tsADC.Tag:=tsADC.Tag+i;                {Save valid address}
            knDac.Enabled:=true;
            SetVolt(0);
            Caption:=Caption+tab1+AdrToChip(ADCadr);
            tsADC.Enabled:=true;
            PageControl.ActivePage:=tsADC;
            break;
          end;
        end;

      end;
    end;
  end;
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
  lblTemp.Caption:='';
  Caption:=AppHdr+tab1+intfac;                     {Init, try sensors}
  DACValHdr;                                       {DAC header for value dump}
  UsedChip:=0;                                     {UsedChip: 0 - undef}
  lblMstat.Caption:=capMStat;
  lblMStat.Hint:=hntMStat;
  btnBurnCali.Hint:=hntBurnA+hntBurn+hntBurn1;
  btnBurnCONF.Hint:=hntBurnC+hntBurn+hntBurn1;
  lblBurnC.Caption:=hntBurn;
  ConfHints;
  gridCali.Cells[0, 0]:='Reg';
  gridCali.Cells[1, 0]:='High';
  gridCali.Cells[2, 0]:='Low';
  gridCali.Cells[0, 1]:='Raw Angle';
  gridCali.Cells[0, 2]:='Zero Pos';
  gridCali.Cells[0, 3]:='Max Pos';
  gbConf.Enabled:=false;
  gbCali.Enabled:=false;
  gbCali.Hint:=hntCali;
  gridCali.Hint:='';
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  ReadSensors;                                     {Create a list of available sensors}
  RefreshSensor;
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
        1: gridADC.Hint:=hexid+gridADC.Cells[sp, zl];
        2: gridADC.Hint:=gridADC.Cells[sp, zl];
        3: gridADC.Hint:=gridADC.Cells[sp, zl]+' V';
      end;
    end;
  end;
end;

procedure TForm1.gridCaliPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if (aCol=0) and ((aRow=2) or (aRow=3)) then
    gridCali.Canvas.Brush.Color:=clRW;
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
    case UsedChip of                               {UsedChip: 0 - undef, 1 - MPU, 2 - HMC, 3 - IST, 4 - AS5, 5 - ADC}
      1: if r in rwregs then gridReg.Canvas.Brush.Color:=clRW;
      2: if r in rwHMC  then gridReg.Canvas.Brush.Color:=clRW;
      3: if r in rwIST  then gridReg.Canvas.Brush.Color:=clRW;
      4: if r in rwpAS5 then gridReg.Canvas.Brush.Color:=clRW;
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

procedure TForm1.knDACChange(Sender: TObject; AValue: Longint);  {Knob for DAC moved}
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

procedure TForm1.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage=tsADC then
    UsedChip:=5;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
end;

procedure TForm1.SetTimer;
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerAS5.Enabled:=false;
  TimerHMC.Enabled:=false;
  TimerADC.Enabled:=false;
  TimerMPU.Interval:=StrToInt(rgMPUTimer.Items[rgMPUTimer.ItemIndex]);
  TimerIST.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
  TimerHMC.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
  TimerADC.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
  TimerAS5.Interval:=StrToInt(rgISTTimer.Items[rgISTTimer.ItemIndex]);
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

procedure TForm1.TimerAS5Timer(Sender: TObject);
begin
  ReadAS5ro;                                       {Read As5600 output register}
end;

procedure TForm1.TimerHMCTimer(Sender: TObject);
var
  w: int16;
  g: double;
  gain: byte;

begin
  gain:=88;                                        {invalid}
  gain:=getGain;
  gridReg.Cells[6, 1]:=GainToStr(gain);
  if PageControl.ActivePage=tsTable then begin     {Write to table}
    gridReg.BeginUpdate;

    w:=GetRegWle(HMCAdr, 3);                       {X}
    gridReg.Cells[1, 1]:=IntToHex(w, 4);
    gridReg.Cells[2, 1]:=IntToStr(w);
    gridReg.Cells[4, 1]:=FormatFloat(tf, ConvHMC(w, gain));
    w:=GetRegWle(HMCAdr, 7);
    gridReg.Cells[1, 2]:=IntToHex(w, 4);           {Y}
    gridReg.Cells[2, 2]:=IntToStr(w);
    gridReg.Cells[4, 2]:=FormatFloat(tf, ConvHMC(w, gain));
    w:=GetRegWle(HMCAdr, 5);                       {Z}
    gridReg.Cells[1, 3]:=IntToHex(w, 4);
    gridReg.Cells[2, 3]:=IntToStr(w);
    gridReg.Cells[4, 3]:=FormatFloat(tf, ConvHMC(w, gain));

    gridReg.EndUpdate;
  end;

  if PageControl.ActivePage=tsMag then begin       {Compass}
    g:=ConvHMC(GetRegWle(HMCAdr, 3), gain);        {X}
    chMagLineX.SetYValue(samples, g);
    g:=ConvHMC(GetRegWle(HMCAdr, 7), gain);        {Y}
    chMagLineY.SetYValue(samples, g);
    g:=ConvHMC(GetRegWle(HMCAdr, 5), gain);        {Z}
    chMagLineZ.SetYValue(samples, g);
  end;

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
  TimerAS5.Enabled:=false;
  TimerHMC.Enabled:=false;
  btnWriteTable.Enabled:=true;
  MPURegHdr;
  lblTemp.Caption:=TempToStr;
  ADCstop;
  PageControl.ActivePage:=tsTable;
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
  UsedChip:=1;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
end;

procedure TForm1.btnOTPClick(Sender: TObject);
begin
  SetReg(AS5adr, $FF, $01);
  SetReg(AS5adr, $FF, $11);
  SetReg(AS5adr, $FF, $10);
  sleep(5);
  ReadAS5600Reg;
end;

procedure TForm1.btnMPUcycClick(Sender: TObject);  {Start reading values}
begin
  UsedChip:=1;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
  ADCstop;
  if PageControl.ActivePageIndex>2 then
    PageControl.ActivePage:=tsTable;
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerHMC.Enabled:=false;
  TimerAS5.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  fs_sel:=GetFS_SEL;                               {Gyro Scale 0..3}
  afs_sel:=GetAFS_SEL;                             {Acc Scale 0..3}
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

procedure TForm1.TimerISTTimer(Sender: TObject);   {Mag chips}
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
  Close;
end;

procedure TForm1.btnReadCONFClick(Sender: TObject);
begin
  ReadAS5600Reg;
end;

procedure TForm1.btnReadCyclClick(Sender: TObject);  {Cyclic read AS5600 read-Only register}
begin
  TimerAS5.Enabled:=true;
  PageControl.ActivePage:=tsTable;
end;

procedure TForm1.btnAS5RegClick(Sender: TObject);    {AS5600 read register to table}
begin
  PageControl.ActivePage:=tsTable;
  ReadAS5600Reg;
end;

procedure TForm1.btnBurnCaliClick(Sender: TObject);  {Burn_Angle command}
begin
  SetReg(AS5adr, $FF, $80);
end;

function SetHintCB (cb: TComboBox; defaulthint: string=''): string;
begin
  if cb.ItemIndex<0 then
    result:=defaulthint
  else
    result:=cb.Items[cb.ItemIndex];
end;

function TForm1.ConvAngle(d: integer): string;       {Conversion Maximum Angle to °}
var
  w: single;
begin
  if d=0 then
    exit ('360°');                                   {?????? not scaled?}
  w:=d*360/4095;
  if w<18 then begin                                 {must be greater than 18° ($CD)}
    result:='invalid';
    btnBurnCONF.Enabled:=false;
  end else begin
    result:=FormatFloat('0.0', w)+'°';
    btnBurnCONF.Enabled:=true;
  end;
end;

procedure TForm1.ConfHints;
begin
  cbPM.Hint:=SetHintCB(cbPM, 'Power Mode');
  cbHYST.Hint:='Hysteresis';
  cbOUTS.Hint:=SetHintCB(cbOUTS, 'Output Stage');
  cbPWMF.Hint:=SetHintCB(cbPWMF, 'PWM Frequency');
  cbSF.Hint:='Slow Filter';
  cbFTH.Hint:=SetHintCB(cbFTH, 'Fast Filter Threshold');
  cbWD.Hint:='Watchdog';
end;

procedure TForm1.btnBurnCONFClick(Sender: TObject);    {Burn_Setting command}
begin
  SetReg(AS5adr, $FF, $40);
end;

procedure TForm1.FillMANGD(Sender: TObject);           {Show Maximum Angle}
var
  v: integer;

begin
  v:=0;
  if edMANGL.Text<>'' then begin
    v:=StrToInt(hexid+edMANGL.Text) and 255;
  end;
  if edMANGH.Text<>'' then begin
    v:=(StrToInt(hexid+edMANGH.Text) and 15)*256+v;
  end;
  edMANGD.Text:=ConvAngle(v);
end;

procedure TForm1.ReadAS5ro;                        {Read As5600 output register}
var
  i, x, raw, ang: integer;
  b: byte;

begin
  for i:=12 to 15 do begin                         {Output registers}
    b:=GetReg(AS5adr, i);
    x:=i-2;
    gridReg.Cells[1, x]:=IntToHex(i, 2);
    gridReg.Cells[2, x]:=Format(df, [i]);
    gridReg.Cells[4, x]:=IntToBin(b, 8);
    gridReg.Cells[5, x]:=IntToHex(b, 2);
    gridReg.Cells[6, x]:=Format(df, [b]);
    case x of
      10: raw:=(b and 15) *256;                    {Raw high}
      11: raw:=raw+b;
      12: ang:=(b and 15) *256;
      13: ang:=ang+b;
    end;
  end;
  gridCali.Cells[1, 1]:=gridReg.Cells[5, 10];      {Raw Angle, needed for calibration}
  gridCali.Cells[2, 1]:=gridReg.Cells[5, 11];
  slAng.Value:=ang;
  slRaw.Value:=raw;
end;

// More: https://github.com/RobTillaart/AS5600
procedure TForm1.ReadAS5600Reg;  {AS5600 register only}
var
  i, b, x: byte;

begin
  UsedChip:=4;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
  gridReg.RowCount:=19;
  CommonRegHdr;
  gridReg.Cells[0, 1]:='Conf ZMCO';
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

  for i:=0 to 6 do begin
    b:=GetReg(AS5adr, i);                          {Configuration registers}
    x:=i+1;
    gridReg.Cells[1, x]:=IntToHex(i, 2);           {Addr hex}
    gridReg.Cells[2, x]:=Format(df, [i]);          {Addr dec}
    gridReg.Cells[4, x]:=IntToBin(b, 8);           {Value bin}
    gridReg.Cells[5, x]:=IntToHex(b, 2);           {Value hex}
    gridReg.Cells[6, x]:=Format(df, [b]);          {Value dec}
  end;

  gridCali.Cells[1, 2]:=gridReg.Cells[5, 2];       {ZPOS}
  gridCali.Cells[2, 2]:=gridReg.Cells[5, 3];
  gridCali.Cells[1, 3]:=gridReg.Cells[5, 4];       {MPOS}
  gridCali.Cells[2, 3]:=gridReg.Cells[5, 5];

  edMANGH.Text:=gridReg.Cells[5, 5];               {Maximum Angle}
//  edMangH.Tag:=StrToInt(hexid+gridReg.Cells[5, 5]) and 15;
  edMANGL.Text:=gridReg.Cells[5, 6];
//  edMANGL.Tag:=b;                                  {Tags keep the original values from Read}

  b:=GetReg(AS5adr, 7);                            {CONF_H}
  gridReg.Cells[1, 8]:='07';                       {Addr hex}
  gridReg.Cells[2, 8]:=gridReg.Cells[1, 8];        {Addr dec}
  gridReg.Cells[4, 8]:=IntToBin(b, 8);             {Value bin}
  gridReg.Cells[5, 8]:=IntToHex(b, 2);             {Value hex}
  gridReg.Cells[6, 8]:=Format(df, [b]);            {Value dec}

  cbSF.ItemIndex:=b and 3;                         {Set controls for programming (CONF high)}
  b:=b shr 2;
  cbFTH.ItemIndex:=b and 7;
  b:=b shr 3;
  cbWD.ItemIndex:=b and 1;

  b:=GetReg(AS5adr, 8);                            {CONF_L}
  gridReg.Cells[1, 9]:='08';                       {Addr hex}
  gridReg.Cells[2, 9]:=gridReg.Cells[1, 9];        {Addr dec}
  gridReg.Cells[4, 9]:=IntToBin(b, 8);             {Value bin}
  gridReg.Cells[5, 9]:=IntToHex(b, 2);             {Value hex}
  gridReg.Cells[6, 9]:=Format(df, [b]);            {Value dec}

  cbPM.ItemIndex:=b and 3;                         {Set controls for programming (CONF low)}
  b:=b shr 2;
  cbHYST.ItemIndex:=b and 3;
  b:=b shr 2;
  cbOUTS.ItemIndex:=b and 3;
  b:=b shr 2;
  cbPWMF.ItemIndex:=b and 3;

  ReadAS5ro;

  b:=GetReg(AS5adr, 11);                           {Status registers}
  gridReg.Cells[1, 14]:='0B';
  gridReg.Cells[2, 14]:='11';
  gridReg.Cells[4, 14]:=IntToBin(b, 8);
  gridReg.Cells[5, 14]:=IntToHex(b, 2);
  gridReg.Cells[6, 14]:=Format(df, [b]);
  if (b and 56)=32 then begin                      {Check status bits}
    lblMStat.Caption:=capMStat+' OK';
  end;
  cbMH.Checked:=(b and 8) > 0;                     {AGC minimum gain overflow, magnet too strong}
  cbML.Checked:=(b and 16) > 0;                    {AGC maximum gain overflow, magnet too weak}
  cbMD.Checked:=(b and 32) > 0;                    {Magnet was detected}

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

  gbConf.Enabled:=true;
  gbCali.Enabled:=true;
  ConfHints;
end;

procedure TForm1.ADCstop;                          {Stop cyclic measuremen}
begin
  TimerADC.Enabled:=false;
  btnSinus.Tag:=0;
  knDAC.Enabled:=true;
  if PageControl.ActivePage=tsADC then
    samples:=0;
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
  TimerAS5.Enabled:=false;
  TimerHMC.Enabled:=false;
  ADCstop;
  PageControl.ActivePage:=tsTable;
  gridReg.BeginUpdate;
  if rgMag.ItemIndex=1 then begin
    UsedChip:=3;                                   {UsedChip: 2 - HMC, 3 - IST}
    ISTRegHdr;
    SetReg(ISTAdr, 10, 1);                         {Single measurement mode for temp}
    lblTemp.Caption:=FormatFloat(tf, GetRegWle(ISTAdr, $1C)/1000)+tempunit;
    ISTreset;                                      {Control register2 Soft reset}
    if cbISTsingle.Checked then
      SetReg(ISTAdr, 10, 1);                       {Single measurement mode}
    if cbISTdren.Checked then
      SetReg(ISTAdr, 11, 8);                       {DREN}
    if cbISTSTR.Checked then
      SetReg(ISTAdr, 12, 64);                      {Self test mode}

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

    b:=GetReg(ISTAdr, $1C);                        {Temperature}
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

    b:=GetReg(ISTAdr, $41);                        {AVG control}
    gridReg.Cells[1, 15]:='41';
    gridReg.Cells[2, 15]:='65';
    gridReg.Cells[4, 15]:=IntToBin(b, 8);
    gridReg.Cells[5, 15]:=IntToHex(b, 2);
    gridReg.Cells[6, 15]:=Format(df, [b]);
    b:=GetReg(ISTAdr, $42);                        {PD control}
    gridReg.Cells[1, 16]:='42';
    gridReg.Cells[2, 16]:='66';
    gridReg.Cells[4, 16]:=IntToBin(b, 8);
    gridReg.Cells[5, 16]:=IntToHex(b, 2);
    gridReg.Cells[6, 16]:=Format(df, [b]);
  end;
  if rgMag.ItemIndex=0 then begin
    HMCRegHdr;
    UsedChip:=2;                                   {UsedChip: 2 - HMC, 3 - IST}
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
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
end;

procedure TForm1.btnISTcycClick(Sender: TObject);
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerHMC.Enabled:=false;
  TimerAS5.Enabled:=false;
  btnWriteTable.Enabled:=false;
  btnWrAdr.Enabled:=false;
  ADCstop;
  if Pagecontrol.ActivePage<>tsMag then
    PageControl.ActivePage:=tsTable;
  if rgMag.ItemIndex=1 then begin
    UsedChip:=2;                                   {UsedChip: 2 - HMC, 3 - IST}
    MagValHDR(ISTadr);
    ISTreset;                                      {Control register2 Soft reset}
    if cbISTdren.Checked  then
      SetReg(ISTAdr, 11, 8);                       {DREN}
    TimerIST.Enabled:=true;
  end;
  if rgMag.ItemIndex=0 then begin
    MagValHDR(HMCadr);
    UsedChip:=2;                                   {UsedChip: 2 - HMC, 3 - IST}
    HMCinit;
    TimerHMC.Enabled:=true;
  end;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  SaveDialog.FileName:='Register_'+AdrToChip(UsedChipToAdr(UsedChip))+'.csv';
  if SaveDialog.Execute then
    gridReg.SaveToCSVFile(SaveDialog.FileName, ';');
end;

procedure TForm1.ReadSensors;                      {Look for active i2c addresses}
var
  s: string;
  i: integer;

begin
  Screen.Cursor:=crHourglass;
  cgSensors.Items.Clear;
  try
    s:=trim(ScanI2C);
    if s='' then begin
      lblChipAdr.Caption:='none';
    end else begin
      lblChipAdr.Caption:=s;
      cgSensors.Items.AddDelimitedText(s, ' ', true);
      for i:=0 to cgSensors.Items.Count-1 do begin
        s:=cgSensors.Items[i];
        if s=HMCadr then
          rgMag.ItemIndex:=0;
        if s=ISTadr then
          rgMag.ItemIndex:=1;
        cgSensors.Items[i]:=s+' '+AdrToChip(s);
        cgSensors.Checked[i]:=true;
      end;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.btnScanClick(Sender: TObject);    {Look for active i2c addresses}
begin
  ReadSensors;
end;

procedure TForm1.btnSelftestClick(Sender: TObject); {Start self test}
begin
  UsedChip:=1;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
  if btnMPUread.Enabled then begin
    TimerIST.Enabled:=false;
    TimerAS5.Enabled:=false;
    TimerHMC.Enabled:=false;
    btnWriteTable.Enabled:=false;
    btnWrAdr.Enabled:=false;
    PageControl.ActivePage:=tsTable;
    MPUValHdr;
    samples:=0;
    btnSelfTest.Tag:=0;
    TimerST.Interval:=200;
    TimerST.Enabled:=true;
    TimerMPU.Enabled:=true;
    lblAS5.Caption:='';
  end;
end;

procedure TForm1.btnSetMaxClick(Sender: TObject);
begin
  ReadAS5ro;                                       {Read AS5600 output register}
  gridCali.Cells[1, 3]:=gridCali.Cells[1, 1];
  SetReg(AS5adr, 3, StrToInt(hexid+gridCali.Cells[1, 3]) and 15);  {High}
  sleep(5);
  gridCali.Cells[2, 3]:=gridCali.Cells[2, 1];
  SetReg(AS5adr, 4, StrToInt(hexid+gridCali.Cells[2, 3]));         {low}
end;

procedure TForm1.btnSetZeroClick(Sender: TObject);
begin
  ReadAS5ro;                                       {Read AS5600 output register}
  gridCali.Cells[1, 2]:=gridCali.Cells[1, 1];
  SetReg(AS5adr, 1, StrToInt(hexid+gridCali.Cells[1, 2]) and 15);  {High}
  sleep(5);
  gridCali.Cells[2, 2]:=gridCali.Cells[2, 1];
  SetReg(AS5adr, 2, StrToInt(hexid+gridCali.Cells[2, 2]));         {low}
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
  TimerHMC.Enabled:=false;
  TimerST.Enabled:=false;                          {Self test timer}
  TimerAS5.Enabled:=false;
  btnWrAdr.Enabled:=true;
  ADCstop;
  RefreshSensor;
end;

procedure TForm1.btnAddSlaveClick(Sender: TObject);
var
  b: byte;

begin
  UsedChip:=1;
  lblChipAdr.Caption:=UsedChipToAdr(UsedChip);
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
  case usedChip of
    1: if a in rwregs then begin
         lblError.Caption:=hexidc+IntToHex(a, 2);
       end else begin
         lblError.Caption:='Invalid register address';
         btnWrAdr.Enabled:=false;
       end;
    2: if a in rwHMC then begin
         lblError.Caption:=hexidc+IntToHex(a, 2);
       end else begin
         lblError.Caption:='Invalid register address';
         btnWrAdr.Enabled:=false;
       end;
    3: if a in rwIST then begin
         lblError.Caption:=hexidc+IntToHex(a, 2);
       end else begin
         lblError.Caption:='Invalid register address';
         btnWrAdr.Enabled:=false;
       end;
    5: begin
         lblError.Caption:='Write value to DAC';
         btnWrAdr.Enabled:=true;
       end;
  end;
end;

procedure TForm1.btnWrAdrClick(Sender: TObject);   {Write one byte to a register}
var
  a: byte;

begin
  a:=StrToIntDef(edAdr.Text, 0) and $FF;
  case usedChip of
    1: if a in rwregs then begin
         MPUWakeUp;                                {Wake up MPU6050}
    end;
    2: SetReg(HMCadr, a, btnWrAdr.Tag and $FF);
    3: if a in rwIST then begin
      ISTreset;                                    {Reset IST8310}
      SetReg(ISTadr, a, btnWrAdr.Tag and $FF);
    end;
    5: SetDAC(hexidc+IntToHex(tsADC.Tag, 2), btnWrAdr.Tag);
  end;
  lblError.Caption:=IntToStr(btnWrAdr.Tag)+' written to '+IntToStr(a);
end;

procedure TForm1.btnWriteCONFClick(Sender: TObject);
var
  b: byte;

begin
  b:=cbWD.ItemIndex;                               {CONF High}
  b:=b shl 3;
  b:=b or cbFTH.ItemIndex;
  b:=b shl 2;
  b:=b or cbSF.ItemIndex;
  SetReg(AS5adr, 7, (b and 63));
  sleep(5);

  b:=cbPWMF.ItemIndex;                             {CONF low}
  b:=b shl 2;
  b:=b or cbOUTS.ItemIndex;
  b:=b shl 2;
  b:=b or cbHYST.ItemIndex;
  b:=b shl 2;
  b:=b or cbPM.ItemIndex;
  SetReg(AS5adr, 8, b);
end;

procedure TForm1.btnWriteTableClick(Sender: TObject);
var
  b: byte;

  procedure wrMPU;
  var
    i: integer;

  begin
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

  procedure wrIST;
  var
    i: integer;

  begin
    for i:=10 to 12 do                             {Control register, Self test}
      SetReg(ISTadr, i, StrToIntDef(gridReg.Cells[6, i], 0));
    SetReg(ISTadr, 65, StrToIntDef(gridReg.Cells[6, 15], 0));
    SetReg(ISTadr, 66, StrToIntDef(gridReg.Cells[6, 16], 0));
  end;

  procedure wrHMC;
  var
    i: integer;

  begin
    for i:=0 to 2 do                               {Conf / Mode register}
      SetReg(HMCadr, i, StrToIntDef(gridReg.Cells[6, i+1], 0));
  end;

begin
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerAS5.Enabled:=false;
  TimerHMC.Enabled:=false;
  case UsedChip of
    1: wrMPU;                                      {MPU6050}
    2: wrHMC;                                      {IST8310}
    3: wrIST;                                      {HMC5883}
    5: SetDAC(hexidc+IntToHex(tsADC.Tag, 2), 0);   {reset ADC}
  end;
end;

procedure TForm1.btnWrZeroClick(Sender: TObject);  {Write zero to all R/W registers}

  procedure wr0MPU;
  var
    i, x, b: byte;

  begin
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

  procedure wr0HMC;
  var
    i: byte;

  begin
    for i:=0 to 2 do                               {Conf / Mode register}
      SetReg(HMCadr, i, 0);
  end;

  procedure wr0IST;
  var
    i: byte;

  begin
    for i:=10 to 12 do                             {Control register, Self test}
      SetReg(ISTadr, i, 0);
    SetReg(ISTadr, 65, 0);                         { $41 Avarage timer control}
    SetReg(ISTadr, 66, 0);                         { $42 Pulse duration control}
  end;

begin
  btnWriteTable.Enabled:=false;
  TimerMPU.Enabled:=false;
  TimerIST.Enabled:=false;
  TimerAS5.Enabled:=false;
  TimerHMC.Enabled:=false;

  case UsedChip of
    1: wr0MPU;
    2: wr0HMC;
    3: wr0IST;
    5: SetDAC(hexidc+IntToHex(tsADC.Tag, 2), 0);
  end;
end;

procedure TForm1.cgSensorsItemClick(Sender: TObject; Index: integer);
begin
  RefreshSensor;
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

