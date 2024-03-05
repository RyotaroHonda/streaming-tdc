library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

library mylib;
use mylib.defDataStructure.all;

use mylib.defODPBlock.all;

entity DelimiterGenerator is
  generic(
    -- DEBUG --
    enDEBUG       : boolean := false
  );
  port(
    rst               : in std_logic;
    clk               : in std_logic;

    -- LACCP -----------------------------------------
    hbCount           : in std_logic_vector(kWidthHBCount-1 downto 0);
    hbfNumber         : in HbNumberType;

    -- HbfFlags --
    lHbfNumMismatchIn   : in std_logic;
    gHbfNumMismatchOut  : out std_logic;

    -- Throttling status -----------------------------
    inThrottlingT2On    : in std_logic;

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
  signal valid_delimiter      : std_logic;
  signal final_delimiter_data : std_logic_vector(kWidthData-1 downto 0);
  signal is_working           : std_logic;

  -- work --
  signal daq_run_state    : std_logic;
  signal daq_gate_state   : std_logic;
  signal veto_state       : std_logic;

  -- DAQ gate number --
  signal daq_gate_number  : GateNumberType;

  -- Heartbeat counter --
  signal hb_frame         : std_logic_vector(kWidthHBFrame-1 downto 0);
  signal hb_count         : std_logic_vector(kWidthHBCount-1 downto 0);

  -- Heartbeat delimiter --
  signal hb_delimiter_wren        : std_logic;
  signal hb_delimiter_1st_data    : std_logic_vector(kWidthData-1 downto 0);
  signal hb_delimiter_2nd_data    : std_logic_vector(kWidthData-1 downto 0);

  -- DAQ gate delimiter --
  signal gate_delimiter_wren      : std_logic;
  signal gate_delimiter_1st_data  : std_logic_vector(kWidthData-1 downto 0);
  signal gate_delimiter_2nd_data  : std_logic_vector(kWidthData-1 downto 0);
  signal gate_delimiter_ack       : std_logic;

  -- second delimiter --
  signal delimiter_2nd_wren       : std_logic;
  signal delimiter_2nd_data       : std_logic_vector(kWidthData-1 downto 0);

  -- flag --
  signal global_hbf_num_mismatch  : std_logic;
  signal reg_itt2_is_on           : std_logic;

  -- Debug --
  attribute mark_debug : boolean;
  attribute mark_debug of daq_run_state       : signal is enDEBUG;
  attribute mark_debug of daq_gate_state      : signal is enDEBUG;
  attribute mark_debug of veto_state          : signal is enDEBUG;

  attribute mark_debug of delimiter_2nd_wren   : signal is enDEBUG;
  attribute mark_debug of gate_delimiter_wren  : signal is enDEBUG;
  attribute mark_debug of hb_delimiter_wren    : signal is enDEBUG;
  attribute mark_debug of is_working           : signal is enDEBUG;
  attribute mark_debug of valid_delimiter      : signal is enDEBUG;
  --attribute mark_debug of final_delimiter_data : signal is enDEBUG;

begin
  -- =========================== body ===============================

  gHbfNumMismatchOut  <= global_hbf_num_mismatch;
  runState            <= daq_run_state;
  gateState           <= daq_gate_state;
  vetoState           <= veto_state;

  validDelimiter      <= valid_delimiter;
  dOutDelimiter       <= final_delimiter_data;
  isWorkingOut        <= is_working;

  hbCount                 <=  hb_count;
  valid_delimiter         <=  '1' when (delimiter_2nd_wren = '1' or hb_delimiter_wren = '1' or gate_delimiter_wren = '1') else '0';
  final_delimiter_data    <=  delimiter_2nd_data        when( delimiter_2nd_wren = '1') else
                              hb_delimiter_1st_data     when( hb_delimiter_wren ='1' ) else
                              gate_delimiter_1st_data   when( gate_delimiter_wren = '1') else (others=>'0');
  gen_tdc       : if kWorkingType = "TDC" generate begin
    is_working            <=  '1' when (daq_run_state='1' and daq_gate_state='1' and veto_state='0' and lHbfNumMismatchIn='0' and tcpActive='1') else '0';
  end generate;
  gen_mikumari  : if kWorkingType = "MIKUMARI" generate begin
    is_working            <=  '1' when (daq_run_state='1' and daq_gate_state='1' and tcpActive='1' ) else '0';
  end generate;

  -- Daq_run_state
  u_run_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      daq_run_state <= '0';
    elsif(clk'event and clk = '1') then
      if(stcpCommand(kRunStart.id) = '1')then
        daq_run_state <= '1';
      elsif(stcpCommand(kRunEnd.id) = '1')then
        daq_run_state <= '0';
      end if;
    end if;
  end process;

  -- Daq_gate_state
  u_gate_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      daq_gate_state  <= '0';
    elsif(clk'event and clk = '1') then
      if(stcpPulse(kGateStart.id ) ='1')then
        daq_gate_state  <= '1';
      elsif(stcpPulse(kGateEnd.id) = '1')then
        daq_gate_state  <= '0';
      end if;
    end if;
  end process;

  -- Veto_state
  u_veto_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      veto_state <= '0';
    elsif(clk'event and clk = '1') then
      if(stcpPulse(kVetoStart.id) = '1')then
        veto_state <= '1';
      elsif(stcpPulse(kVetoEnd.id) = '1')then
        veto_state <= '0';
      end if;
    end if;
  end process;

  -- DAQ gate number
  u_gate_number_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      daq_gate_number  <= (others=>'0');
    elsif(clk'event and clk = '1') then
      if(stcpCommand(kGateNum.id) = '1')then
        daq_gate_number  <= gateNumber;
      end if;
    end if;
  end process;

  -- Heartbeat counter
  u_counter_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      hb_frame <= (others=>'0');
      hb_count <= (others=>'0');
    elsif(clk'event and clk = '1') then
      if(stcpPulse(kHbCounterReset.id) = '1')then
        hb_frame <= (others=>'0');
        hb_count <= (others=>'0');
      else
        hb_count <= std_logic_vector(unsigned(hb_count) + 1);
        -- Heartbeat!!! --
        if(hb_count = kMaxHBCount)then
          hb_frame <= std_logic_vector(unsigned(hb_frame) + 1);
        end if;
      end if;
    end if;
  end process;

  -- Generate heartbeat delimiter
  u_hb_delimiter_delimiter_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      hb_delimiter_wren       <= '0';
      hb_delimiter_1st_data   <= (others => '0');
      hb_delimiter_2nd_data   <= (others => '0');
    elsif(clk'event and clk = '1') then
      -- Generate two delimiter words, 1st at kMaxHBCount-1, 2nd at KmaxHBCount --
      if(unsigned(hb_count) >= unsigned(kMaxHBCount)-1 and daq_gate_state = '1' and daq_run_state = '1' and tcpActive = '1')then
        hb_delimiter_wren  <= '1';
        hb_delimiter_1st_data(kMSBDataType    downto kLSBDataType)  <= kDatatypeHeartbeat;
        hb_delimiter_1st_data(kMSBFlag        downto kLSBFlag)      <= genFlagVector(kIndexGHbfNumMismatch, global_hbf_num_mismatch) or genFlagVector(kIndexInThrottlingT2, reg_itt2_is_on);
        hb_delimiter_1st_data(kMSBGateNumber  downto kLSBGateNumber)<= daq_gate_number;
        hb_delimiter_1st_data(kMSBHBFrame     downto kLSBHBFrame)   <= hb_frame;
        hb_delimiter_2nd_data(kMSBDataType    downto kLSBDataType)  <= kDatatypeHeartbeat;
        hb_delimiter_2nd_data(kMSBFlag        downto kLSBFlag)      <= genFlagVector(kIndexGHbfNumMismatch, global_hbf_num_mismatch) or genFlagVector(kIndexInThrottlingT2, reg_itt2_is_on);
        hb_delimiter_2nd_data(kMSBGateNumber  downto kLSBGateNumber)<= daq_gate_number;
        hb_delimiter_2nd_data(kMSBHBFrame     downto kLSBHBFrame)   <= hb_frame;
      else
        hb_delimiter_wren       <= '0';
        hb_delimiter_1st_data   <= (others => '0');
        hb_delimiter_2nd_data   <= (others => '0');
      end if;
    end if;
  end process;

  -- Generate gate delimiter
  gate_delimiter_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      gate_delimiter_wren       <= '0';
      gate_delimiter_1st_data   <= (others => '0');
      gate_delimiter_2nd_data   <= (others => '0');
    elsif(clk'event and clk = '1') then
      if(gate_delimiter_wren = '1' and gate_delimiter_ack = '1') then
        gate_delimiter_wren       <= '0';
        gate_delimiter_1st_data   <= (others => '0');
        gate_delimiter_2nd_data   <= (others => '0');
      elsif(stcpPulse(kGateStart.id) = '1' and daq_run_state = '1' and tcpActive = '1') then  -- daq_gate_state on
        gate_delimiter_wren  <= '1';
        gate_delimiter_1st_data(kMSBDataType    downto kLSBDataType)    <= kDatatypeGateOn;
        gate_delimiter_1st_data(kMSBFlag        downto kLSBFlag)        <= genFlagVector(kIndexGHbfNumMismatch, global_hbf_num_mismatch) or genFlagVector(kIndexInThrottlingT2, reg_itt2_is_on);
        gate_delimiter_1st_data(kMSBGateNumber  downto kLSBGateNumber)  <= daq_gate_number;
        gate_delimiter_1st_data(kMSBHBCount     downto kLSBHBCount)     <= hb_frame;
        gate_delimiter_2nd_data(kMSBDataType    downto kLSBDataType)    <= kDatatypeGateOn;
        gate_delimiter_2nd_data(kMSBFlag        downto kLSBFlag)        <= genFlagVector(kIndexGHbfNumMismatch, global_hbf_num_mismatch) or genFlagVector(kIndexInThrottlingT2, reg_itt2_is_on);
        gate_delimiter_2nd_data(kMSBGateNumber  downto kLSBGateNumber)  <= daq_gate_number;
        gate_delimiter_2nd_data(kMSBHBCount     downto kLSBHBCount)     <= hb_count;
      elsif(stcpPulse(kGateEnd.id) = '1' and daq_run_state = '1' and tcpActive = '1')then     -- daq_gate_state off
        gate_delimiter_wren  <= '1';
        gate_delimiter_1st_data(kMSBDataType    downto kLSBDataType)    <= kDatatypeGateOff;
        gate_delimiter_1st_data(kMSBFlag        downto kLSBFlag)        <= genFlagVector(kIndexGHbfNumMismatch, global_hbf_num_mismatch) or genFlagVector(kIndexInThrottlingT2, reg_itt2_is_on);
        gate_delimiter_1st_data(kMSBGateNumber  downto kLSBGateNumber)  <= daq_gate_number;
        gate_delimiter_1st_data(kMSBHBCount     downto kLSBHBCount)     <= hb_frame;
        gate_delimiter_2nd_data(kMSBDataType    downto kLSBDataType)    <= kDatatypeGateOff;
        gate_delimiter_2nd_data(kMSBFlag        downto kLSBFlag)        <= genFlagVector(kIndexGHbfNumMismatch, global_hbf_num_mismatch) or genFlagVector(kIndexInThrottlingT2, reg_itt2_is_on);
        gate_delimiter_2nd_data(kMSBGateNumber  downto kLSBGateNumber)  <= daq_gate_number;
        gate_delimiter_2nd_data(kMSBHBCount     downto kLSBHBCount)     <= hb_count;
      end if;
    end if;
  end process;

  -- Second delimiter data insert --
  delimiter_second_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      delimiter_2nd_wren   <= '0';
      delimiter_2nd_data  <= (others => '0');
      gate_delimiter_ack  <= '0';
    elsif(clk'event and clk = '1') then
      if(delimiter_2nd_wren = '1') then      -- second delimiter word
        delimiter_2nd_wren   <= '0';
        delimiter_2nd_data  <= (others => '0');
        gate_delimiter_ack  <= '0';
      elsif(hb_delimiter_wren='1') then     -- heartbeat delimiter
        delimiter_2nd_wren   <= '1';
        delimiter_2nd_data  <= hb_delimiter_2nd_data;
        gate_delimiter_ack  <= '0';
      elsif(gate_delimiter_wren='1') then   -- daq_gate_state delimiter
        delimiter_2nd_wren   <= '1';
        delimiter_2nd_data  <= gate_delimiter_2nd_data;
        gate_delimiter_ack  <= '1';
      else                                  -- idle
        delimiter_2nd_wren   <= '0';
        delimiter_2nd_data  <= (others => '0');
        gate_delimiter_ack  <= '0';
      end if;
    end if;
  end process;

  -- Global hbf misthmath flag --
  flag_process : process(sync_reset,clk)
  begin
    if(sync_reset = '1') then
      global_hbf_num_mismatch  <= '0';
    elsif(clk'event and clk = '1') then
      if(stcpPulse(kHbCounterReset.id) = '1') then
        global_hbf_num_mismatch  <= '0';
      elsif(stcpCommand(kHbFrameNum.id) = '1') then
        if(hbfNumber = hb_frame)then
          global_hbf_num_mismatch  <= '0';
        else
          global_hbf_num_mismatch  <= '1';
        end if;
      end if;
    end if;
  end process;

  -- Input Throttling Type2 Record --
  u_itt2_recoard : process(sync_reset, clk)
  begin
    if(sync_reset = '1') then
      reg_itt2_is_on  <= '0';
    elsif(clk'event and clk = '1') then
      if(inThrottlingT2On = '1') then
        reg_itt2_is_on  <= '1';
      elsif(delimiter_2nd_wren = '1') then
        reg_itt2_is_on  <= '0';
      else
        null;
      end if;
    end if;
  end process;

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clk, sync_reset);

end RTL;
