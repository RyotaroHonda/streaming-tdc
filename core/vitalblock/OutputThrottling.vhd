library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

library mylib;
use mylib.defDataStructure.all;

entity OutputThrottling is
  generic (
    enDEBUG : boolean := false
  );
  port(
    rst     : in STD_LOGIC; --user reset (asynchronous)
    clk     : in STD_LOGIC; --base cloc

    -- status input --
    intputThrottlingOn  : in std_logic; -- Signal indicating InputThrottlingType2 is active
    pfullLinkIn         : in std_logic; -- Programmable full flag from LinkBuffer
    emptyLinkIn         : in std_logic; -- Empty flag from LinkBuffer

    -- Status output --
    isWorking           : out std_logic; -- The signal indicating that this module is throttling data

    -- Data In --
    validIn             : in std_logic;
    dIn                 : in std_logic_vector(kWidthData-1 downto 0);

    -- Data Out --
    validOut            : out std_logic;
    dOut                : out std_logic_vector(kWidthData-1 downto 0)

  );
end OutputThrottling;

architecture Behavioral of OutputThrottling is

  -- System --

  signal data_type                : std_logic_vector(kWidthDataType-1 downto 0);
  signal throttling_is_working    : std_logic;

  signal is_delimiter             : std_logic;

  -- Debug --
  attribute mark_debug : boolean;
  attribute mark_debug of throttling_is_working : signal is enDEBUG;
  attribute mark_debug of is_delimiter          : signal is enDEBUG;

begin
  -- =======================================================================
  --                              Body
  -- =======================================================================

  data_type   <= dIn(kMSBDataType downto kLSBDataType);
  isWorking   <= throttling_is_working;

  -- Throttle status -------------------------------------------------------
  u_state : process(rst, clk)
  begin
    if(rst = '1') then
      throttling_is_working <= '0';
    elsif(clk'event and clk = '1') then
      if(intputThrottlingOn = '1' and pfullLinkIn = '1') then
        throttling_is_working  <= '1';
      elsif(emptyLinkIn = '1') then
        throttling_is_working  <= '0';
      end if;
    end if;
  end process;

  -- Delete TDC data -------------------------------------------------------
  u_delete : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(checkDelimiter(data_type) = true) then
        is_delimiter  <= '1';
      else
        is_delimiter  <= '0';
      end if;

      if(validIn = '1') then
        if(throttling_is_working = '1') then
          dOut      <= dIn;
          if(checkDelimiter(data_type) = true) then
            -- Delimiter data --
            validOut                            <= '1';
            dOut(kMSBFlag downto kLSBFlag)      <= dIn(kMSBFlag downto kLSBFlag) or genFlagVector(kIndexOutThrottling, '1') or genFlagVector(kIndexDataLost, '1');
          else
            -- TDC data --
            validOut                            <= '0';
          end if;
        else
          dOut      <= dIn;
          validOut  <= '1';
        end if;
      else
        validOut    <= '0';
      end if;
    end if;
  end process;


end Behavioral;
