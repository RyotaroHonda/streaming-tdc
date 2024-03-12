library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

library mylib;
use mylib.defDelimiter.all;
use mylib.defLaccp.all;
--use mylib.defODPBlock.all;


entity DelimiterGenerator is
  generic(
    -- DEBUG --
    enDEBUG       : boolean := false
  );
  port(
    rst               : in std_logic;
    clk               : in std_logic;

    -- Status input ----------------------------------
    flagsIn           : in std_logic_vector(kWidthDelimiterFlag-1 downto 0);

    -- LACCP -----------------------------------------
    hbCount           : in std_logic_vector(kWidthStrHbc-1 downto 0);
    hbfNumber         : in std_logic_vector(kWidthStrHbf-1 downto 0);
    LaccpFineOffset   : in signed(kWidthLaccpFineOffset-1 downto 0);

    -- For ODP block ----------------------------------
    -- Delimiter data output --
    validDelimiter    : out std_logic;
    dOutDelimiter     : out std_logic_vector(kWidthData-1 downto 0)
  );
end DelimiterGenerator;

architecture RTL of DelimiterGenerator is
  -- System --
  signal sync_reset     : std_logic;

  -- Signal decralation ---------------------------------------------
  -- Heartbeat delimiter --
  signal hb_delimiter_wren        : std_logic;
  signal hb_delimiter             : std_logic_vector(dOutDelimiter'range);

  -- flag --
  signal reg_flags                : std_logic_vector(flagsIn'range);
  signal edge_wren                : std_logic_vector(1 downto 0);

  -- Debug --
  attribute mark_debug : boolean;
  attribute mark_debug of hb_delimiter_wren : signal is enDEBUG;
  attribute mark_debug of hb_delimiter      : signal is enDEBUG;
  attribute mark_debug of reg_flags         : signal is enDEBUG;
  attribute mark_debug of flagsIn           : signal is enDEBUG;

begin
  -- =========================== body ===============================

  validDelimiter      <= hb_delimiter_wren;
  dOutDelimiter       <= hb_delimiter;

  -- Generate heartbeat delimiter
  u_delimiter_gen : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      hb_delimiter_wren       <= '0';
    elsif(clk'event and clk = '1') then
      -- Generate two delimiter words, 1st at kMaxHBCount-1, 2nd at KmaxHBCount --
      if(unsigned(hbCount) >= unsigned(kMaxHBCount)-1)then
        hb_delimiter_wren                     <= '1';
        hb_delimiter(kPosHbdDataType'range)   <= kDatatypeHeartbeat;
        hb_delimiter(kPosHbdReserve1'range)   <= (others => '0');
        hb_delimiter(kPosHbdFlag'range)       <= reg_flags;
        hb_delimiter(kPosHbdOffset'range)     <= std_logic_vector(LaccpFineOffset);
        hb_delimiter(kPosHbdHBFrame'range)    <= hbfNumber;
      else
        hb_delimiter_wren                     <= '0';
      end if;
    end if;
  end process;

  -- Flag record --
  u_flag_recoard : process(sync_reset, clk)
  begin
    if(sync_reset = '1') then
      reg_flags  <= (others => '0');
    elsif(clk'event and clk = '1') then
      edge_wren <= edge_wren(0) & hb_delimiter_wren;

      for i in 0 to kWidthDelimiterFlag-1 loop
        if(flagsIn(i) = '1') then
          reg_flags(i)  <= '1';
        elsif(edge_wren = "10") then
          reg_flags(i)  <= '0';
        end if;
      end loop;
    end if;
  end process;

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clk, sync_reset);

end RTL;
