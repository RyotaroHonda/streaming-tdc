library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

package defDataBusAbst is

  -- WIDTH of DATA --
  constant kWidthData         : integer  := 64;    -- width of the data
  type DataArrayType is array(natural range <> ) of std_logic_vector(kWidthData-1 downto 0);

end package;