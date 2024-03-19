library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library mylib;
use mylib.defHeartBeatUnit.all;
use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;

package defTDC is

  -- TDC data structure --
  constant kPosChannel  : std_logic_vector(kPosHbdDataType'low-1 downto kPosHbdDataType'low -7)  :=(others => '0');
  constant kPosTot      : std_logic_vector(kPosHbdReserve1'low-1 downto kPosHbdReserve1'low -16) := (others => '0');
  constant kPosTiming   : std_logic_vector(kPosHbdFlag'low-1     downto kPosHbdFlag'low     -19) := (others => '0');

  -- FirstFDCEs parameters -------------------------------------------------------------
  constant kNumTdcClock     : positive  := 4;

  -- FineCounterDecoder parameters -----------------------------------------------------
  constant kWidthFineCount  : positive  := 3;

  -- LT paring -------------------------------------------------------------------------
  constant kLengthParing    : positive  := 32;

  constant kMaxToT          : integer   := 4000; -- Unit ns
  constant kMaxPairingCount : integer   := kMaxToT/8;
  constant kWidthTOT        : positive  := 16; -- ToT value

end package;