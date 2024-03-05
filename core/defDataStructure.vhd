library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library mylib;
use mylib.defChannel.all;
use mylib.defTDC.all;

package defDataStructure is

  -- WIDTH of DATA
  constant kWidthData         : integer  := 64;    -- width of the data

  -- Data type -----------------------------------------------------------------------------
  constant kWidthDataType     : integer  := 6;
  constant kDatatypeTDCData           : std_logic_vector (kWidthDataType-1 downto 0) := "001011"; -- TDC leading ata
  constant kDatatypeTDCDataT          : std_logic_vector (kWidthDataType-1 downto 0) := "001101"; -- TDC trailing ata
  constant kDataTypeIThrottleT1Start  : std_logic_vector (kWidthDataType-1 downto 0) := "011001"; -- Input Throttling Type2 Start timing
  constant kDataTypeIThrottleT1End    : std_logic_vector (kWidthDataType-1 downto 0) := "010001"; -- Input Throttling Type2 End timing
  constant kDataTypeIThrottleT2Start  : std_logic_vector (kWidthDataType-1 downto 0) := "011010"; -- Input Throttling Type2 Start timing
  constant kDataTypeIThrottleT2End    : std_logic_vector (kWidthDataType-1 downto 0) := "010010"; -- Input Throttling Type2 End timing

  constant kDatatypeHeartbeat         : std_logic_vector (kWidthDataType-1 downto 0) := "011100"; -- heartbeat delimiter

  --Delifiter flag definition ---------------------------------------------------------------
  constant kWidthDelimiterFlag    : integer  := 16;  -- Delimiter flag width

  constant kIndexSOS              : integer  := 15;  -- 16th, SOS signal to software
  constant kIndexRadiationURE     : integer  := 14;  -- 15th, Uncorrectable error happens by radiation
  constant kIndexMikumariError    : integer  := 13;  -- 14th, Some communication error happens in MIKUMARI-link
  --constnat kReserve             : integer  := 12;  -- 13th,
  constant kIndexOverflow         : integer  := 11;  -- 12th, IncomingFIFO almost-full or full flags goes high
  constant kIndexGHbfNumMismatch  : integer  := 10;  -- 11th,  Stcp HbfNumber and local HbfNumber mismatch
  constant kIndexLHbfNumMismatch  : integer  := 9;   -- 10th,  Hbf numbers mismatch in between two mezzanines (Exists only in HR-TDC)
  --constnat kReserve             : integer  := 8;   -- 9th,
  constant kIndexInThrottlingT1   : integer  := 7;  -- 8th,  Input throttling type1 is working
  constant kIndexInThrottlingT2   : integer  := 6;  -- 7th,  Input throttling type2 is working
  constant kIndexOutThrottling    : integer  := 5;  -- 6th,  Output throttling is working
  constant kIndexHbfThrottling    : integer  := 4;  -- 5th,  Hbf throttlinng is working
  --constnat kReserve             : integer  := 3;  -- 4th,
  --constnat kReserve             : integer  := 2;  -- 3th,
  --constnat kReserve             : integer  := 1;  -- 2nd,
  --constnat kReserve             : integer  := 0;  -- 1st,

  function genFlagVector(index : integer; flag : std_logic) return std_logic_vector;

  -- Heartbeat frame, counter ----------------------------------------------------------------
  constant kWidthHBFrame      : integer  := 24;  -- Heartbeat frame number
  constant kWidthHBCount      : integer  := 16;  -- Heartbeat counter value

  constant kMaxHBFrame        : std_logic_vector (kWidthHBFrame-1 downto 0) := X"FFFFFF"; -- heartbeat max frame number
  constant kMaxHBCount        : std_logic_vector (kWidthHBCount-1 downto 0) := X"FFFF";   -- heartbeat max count number

  -- Data bit position ---------------------------------------------------------------------
  constant kMSBDataType     : integer  := kWidthData-1;
  constant kLSBDataType     : integer  := kMSBDataType-kWidthDataType+1;

  -- TDC data --
  constant kMSBChannel      : integer  := kMSBDataType-kWidthDataType;
  constant kLSBChannel      : integer  := kMSBChannel-kWidthChannel+1;
  constant kMSBTOT          : integer  := kMSBChannel-kWidthChannel;
  constant kLSBTOT          : integer  := kMSBTOT-kWidthTOT+1;
  constant kMSBTiming       : integer  := kMSBTOT-kWidthTOT;
  constant kLSBTiming       : integer  := kMSBTiming-kWidthTiming+1;

  -- Delimiter data --
  -- 1st delimiter: 6-bit data-type, 2-bit reserve, 16-bit flag,    16-bit Time offset,         24-bit HBF number
  -- 2nd delimiter: 6-bit data-type, 2-bit reserve, 16-bit reserve, 20-bit generated ddta size, 20-bit transferred data size

  -- 1st delimiter --
  constant kPosHbdDataType  : std_logic_vector(kWidthData-1          downto kWidthData          -kWidthDataType);
  constant kPosHbdReserve1  : std_logic_vector(kPosHbdDataType'low-1 downto kPosHbdDataType'low -2);
  constant kPosHbdFlag      : std_logic_vector(kPosHbdReserve1'low-1 downto kPosHbdReserve1'low -kWidthDelimiterFlag);
  constant kPosHbdOffset    : std_logic_vector(kPosHbdFlag'low-1     downto kPosHbdFlag'low     -16);
  constant kPosHbdHBFrame   : std_logic_vector(kPosHbdOffset'low-1   downto kPosHbdOffset'low   -kWidthHBFrame);

  -- 2nd delimiter --
  constant kPosHbdReserve2  : std_logic_vector(kPosHbdReserve1'low-1 downto kPosHbdReserve1'low -16);
  constant kPosHbdGenSize   : std_logic_vector(kPosHbdReserve2'low-1 downto kPosHbdReserve2'low -20);
  constant kPosHbdTransSize : std_logic_vector(kPosHbdGenSize'low-1  downto kPosHbdGenSize'low  -20);

  function checkDelimiter(data_type : std_logic_vector) return boolean;

  -- Data array definition ------------------------------------------------------------------
  type dFineCountType is array (integer range kNumStrInput-1 downto 0) of std_logic_vector(kWidthFineCount-1 downto 0);
  type dTimingType    is array (integer range kNumStrInput-1 downto 0) of std_logic_vector(kWidthTiming-1 downto 0);
  type dTOTType       is array (integer range kNumStrInput-1 downto 0) of std_logic_vector(kWidthTOT-1 downto 0);
  type dDataType      is array (integer range kNumStrInput-1 downto 0) of std_logic_vector(kWidthData-1 downto 0);

end package defDataStructure;
-- ----------------------------------------------------------------------------------
-- Package body
-- ----------------------------------------------------------------------------------
package body defDataStructure is
  function genFlagVector(index : integer; flag : std_logic) return std_logic_vector is
    variable result   : std_logic_vector(kWidthDelimiterFlag-1 downto 0) := (others => '0');
  begin
    result(index)  := flag;
    return result;
  end genFlagVector;

  function checkDelimiter(data_type : std_logic_vector) return boolean is
  begin
    if(data_type = kDatatypeHeartbeat) then
      return true;
    else
      return false;
    end if;
  end checkDelimiter;

end package body defDataStructure;
