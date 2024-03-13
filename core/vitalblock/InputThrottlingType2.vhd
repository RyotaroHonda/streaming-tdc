library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

library mylib;
use mylib.defTDC.all;
use mylib.defChannel.all;
use mylib.defDataStructure.all;

entity InputThrottlingType2 is
  generic (
    kChannel  : integer:= 0;
    enDEBUG   : boolean:= false
  );
  port(
    rst     : in STD_LOGIC; --user reset (asynchronous)
    clk     : in STD_LOGIC; --base cloc

    -- status input --
    ibufProgFullIn      : in std_logic; -- Signal indicating InputThrottlingType2 is active
    emptyIbufIn         : in std_logic; -- Empty fram from a incomingFIFO

    -- Heartbeat count for TDC
    hbCount             : in  std_logic_vector(kWidthHBCount-1 downto 0); -- Heartbeat count

    -- Status output --
    isWorking           : out std_logic; -- The signal indicating that this module is throttling data
    t2startReq          : out std_logic;
    t2startAck          : out std_logic;
    t2endReq            : out std_logic;
    t2endAck            : out std_logic;

    -- Data In --
    validIn             : in std_logic;
    dIn                 : in std_logic_vector(kWidthData-1 downto 0);

    -- Data Out --
    validOut            : out std_logic;
    dOut                : out std_logic_vector(kWidthData-1 downto 0)

  );
end InputThrottlingType2;

architecture Behavioral of InputThrottlingType2 is

  -- System --

  signal data_type                : std_logic_vector(kWidthDataType-1 downto 0);
  signal throttling_is_working, reg_throttling_is_working   : std_logic;

  signal t2start_insert_request, t2start_insert_ack         : std_logic;
  signal t2end_insert_request, t2end_insert_ack             : std_logic;

  -- Debug --
  attribute mark_debug : boolean;
  --attribute mark_debug of throttling_is_working : signal is enDEBUG;

begin
  -- =======================================================================
  --                              Body
  -- =======================================================================

  t2startReq  <= t2start_insert_request;
  t2startAck  <= t2start_insert_ack;
  t2endReq    <= t2end_insert_request;
  t2endAck    <= t2end_insert_ack;

  data_type   <= dIn(kMSBDataType downto kLSBDataType);
  isWorking   <= throttling_is_working;

  -- Throttle status -------------------------------------------------------
  u_state : process(rst, clk)
  begin
    if(rst = '1') then
      throttling_is_working     <= '0';
      reg_throttling_is_working <= '0';
    elsif(clk'event and clk = '1') then
      reg_throttling_is_working <= throttling_is_working;
      if(ibufProgFullIn = '1') then
        throttling_is_working  <= '1';
      elsif(emptyIbufIn = '1') then
        throttling_is_working  <= '0';
      end if;
    end if;
  end process;

  -- T2Start/End insertion request -----------------------------------------
  u_request : process(clk, rst)
  begin
    if(rst = '1') then
      t2start_insert_request    <= '0';
      t2end_insert_request      <= '0';
    elsif(clk'event and clk = '1') then
      if(throttling_is_working = '1' and  reg_throttling_is_working = '0') then
        -- Throttling starts --
        t2start_insert_request  <= '1';
      elsif(t2start_insert_ack = '1') then
        t2start_insert_request  <= '0';
      end if;

      if(throttling_is_working = '0' and  reg_throttling_is_working = '1') then
        -- Throttling ends --
        t2end_insert_request  <= '1';
      elsif(t2end_insert_ack = '1') then
        t2end_insert_request  <= '0';
      end if;
    end if;
  end process;


  -- Delete TDC data -------------------------------------------------------
  u_delete : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(validIn = '1' ) then -- Data is comming --
        dOut      <= dIn;
--        t2start_insert_ack  <= '0';
--        t2end_insert_ack    <= '0';
        if(checkDelimiter(data_type) = true) then
          -- Delimiter data --
          validOut  <= '1';
          dOut(kMSBFlag downto kLSBFlag)      <= dIn(kMSBFlag downto kLSBFlag) or genFlagVector(kIndexDataLost, '1');
        else
          -- Not delimiter data --
          if(throttling_is_working = '1' or reg_throttling_is_working = '1' or t2end_insert_request = '1') then
--          if(throttling_is_working = '1') then
            -- TDC data, Discard --
            validOut  <= '0';
          else
            -- TDC data, through --
            validOut  <= '1';
          end if;
        end if;
      else -- Data is not comming --
        if(t2start_insert_request = '1' and t2start_insert_ack = '0') then
          validOut    <= '1';
          dOut(kMSBDataType downto kLSBDataType)  <= kDataTypeIThrottleT2Start;
          dOut(kMSBChannel  downto kLSBChannel)   <= std_logic_vector(to_unsigned(kChannel, kWidthChannel));
          dOut(kMSBTOT      downto kLSBTOT)       <= (others => '0');
          dOut(kMSBTiming   downto kLSBTiming)    <= hbCount & std_logic_vector(to_unsigned(0, kWidthFineCount));

          t2start_insert_ack  <= '1';
          t2end_insert_ack    <= '0';
        elsif(t2end_insert_request = '1' and t2end_insert_ack = '0') then
          validOut    <= '1';
          dOut(kMSBDataType downto kLSBDataType) <= kDataTypeIThrottleT2End;
          dOut(kMSBChannel  downto kLSBChannel)  <= std_logic_vector(to_unsigned(kChannel, kWidthChannel));
          dOut(kMSBTOT      downto kLSBTOT)      <= (others => '0');
          dOut(kMSBTiming   downto kLSBTiming)   <= hbCount & std_logic_vector(to_unsigned(0, kWidthFineCount));

          t2start_insert_ack  <= '0';
          t2end_insert_ack    <= '1';
        else
          dOut                <= dIn;
          validOut            <= '0';

          t2start_insert_ack  <= '0';
          t2end_insert_ack    <= '0';
        end if;
      end if;
    end if;
  end process;



--  u_delete : process(clk)
--  begin
--    if(clk'event and clk = '1') then
--      if(throttling_is_working = '1') then
--        if(reg_throttling_is_working = '0') then
--          validOut    <= '1';
--          dOut(kMSBDataType downto kLSBDataType)  <= kDataTypeIThrottleT2Start;
--          dOut(kMSBChannel  downto kLSBChannel)   <= std_logic_vector(to_unsigned(kChannel, kWidthChannel));
--          dOut(kMSBTOT      downto kLSBTOT)       <= (others => '0');
--          dOut(kMSBTiming   downto kLSBTiming)    <= hbCount & std_logic_vector(to_unsigned(0, kWidthFineCount));
--
--        elsif(validIn = '1') then
--          dOut      <= dIn;
--          if(checkDelimiter(data_type) = true) then
--            -- Delimiter data --
--            validOut  <= '1';
--            dOut(kMSBFlag downto kLSBFlag)      <= dIn(kMSBFlag downto kLSBFlag) or genFlagVector(kIndexDataLost, '1');
--          else
--            -- TDC data --
--            validOut  <= '0';
--          end if;
--        else
--          validOut    <= '0';
--        end if;
--      else
--        if(reg_throttling_is_working = '1') then
--          validOut    <= '1';
--          dOut(kMSBDataType downto kLSBDataType) <= kDataTypeIThrottleT2End;
--          dOut(kMSBChannel  downto kLSBChannel)  <= std_logic_vector(to_unsigned(kChannel, kWidthChannel));
--          dOut(kMSBTOT      downto kLSBTOT)      <= (others => '0');
--          dOut(kMSBTiming   downto kLSBTiming)   <= hbCount & std_logic_vector(to_unsigned(0, kWidthFineCount));
--        else
--          dOut      <= dIn;
--          validOut  <= validIn;
--        end if;
--      end if;
--    end if;
--  end process;


end Behavioral;
