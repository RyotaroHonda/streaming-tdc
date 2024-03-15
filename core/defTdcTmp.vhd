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

  -- TDC value --
  constant kWidthFineCount  : integer:= 3;

end package;