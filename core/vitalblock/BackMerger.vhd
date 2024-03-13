library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defDataStructure.all;
use mylib.defMerger.all;

entity BackMerger is
  generic (
    -- DEBUG --
    enDEBUG : boolean := false
  );
  port (
    -- System --
    rst                 : in STD_LOGIC;   -- Ascynchronous reset in, synchronous reset out
    clk                 : in STD_LOGIC;   -- System clock
    progFullFifo        : out std_logic;  -- Back Merger FIFO programmed full signal
    hbfNumMismatch      : out std_logic;  -- Heartbeat frame number mismatch among input channel.


    -- Merger input --
    inputReadEnableOut  : out STD_LOGIC_VECTOR (kNumChBack-1 downto 0); --input fifo read enable
    inputDoutIn         : in  dMGRBackType;                             --input fifo data out
    inputEmptyIn        : in  STD_LOGIC_VECTOR (kNumChBack-1 downto 0); --input fifo empty flag
    inputAlmostEmptyIn  : in  STD_LOGIC_VECTOR (kNumChBack-1 downto 0); --input fifo almost empty flag
    inputValidIn        : in  STD_LOGIC_VECTOR (kNumChBack-1 downto 0); --input fifo valid flag

    -- Merger output --
    outputReadEnableIn  : in  STD_LOGIC;                                --output fifo read enable
    outputDoutOut       : out STD_LOGIC_VECTOR (kWidthData-1 downto 0); --output fifo data out
    outputEmptyOut      : out STD_LOGIC;                                --output fifo empty flag
    outputAlmostEmptyOut: out STD_LOGIC;                                --output fifo almost empty flag
    outputValidOut      : out STD_LOGIC                                 --output fifo valid flag
  );
end BackMerger;

architecture Behavioral of BackMerger is
  constant kZero                : std_logic_vector(kNumChBack-1 downto 0):= (others => '0');

  -- read input fifo --
  signal rden_inputfifo         : std_logic_vector(kNumChBack-1 downto 0);
  signal is_read_inputfifo      : std_logic_vector(kNumChBack-1 downto 0);

  -- Data input --
  signal din_merger             : std_logic_vector(kWidthData-1 downto 0);

  -- Flag for delimiter words --
  signal mask_delimiter         : std_logic_vector(kNumChBack-1 downto 0);
  signal flag_last1stdelimiter  : boolean;
  signal flag_wait2nddelimiter  : std_logic_vector(kNumChBack-1 downto 0);

  -- Flag in delimiter words --
  signal delimiter1st_flagbits      : std_logic_vector(kWidthDelimiterFlag-1 downto 0);
  signal delimiter2nd_flagbits      : std_logic_vector(kWidthDelimiterFlag-1 downto 0);

  -- Hb frame check --
  signal reg_hbfnum             : std_logic_vector(kMSBHBFrame-kLSBHBFrame downto 0);
  signal hbfnum_mismatch        : std_logic;
  signal hbfnum_is_registered   : std_logic;

  -- full --
  signal reg_full               : std_logic;
  signal needread_full          : std_logic_vector(kNumChBack-1 downto 0);

  -- write output fifo --
  signal wren_outputfifo        : std_logic;
  signal din_outputfifo         : std_logic_vector(kWidthData-1 downto 0);
  signal full_outputfifo        : std_logic;
  signal almost_empty_outputfifo: std_logic;
  signal prog_full_outputfifo   : std_logic;

  component mergerBackFifo is
    PORT(
      clk         : in  STD_LOGIC;
      srst        : in  STD_LOGIC;

      wr_en       : in  STD_LOGIC;
      din         : in  STD_LOGIC_VECTOR (kWidthData-1 DOWNTO 0);
      full        : out STD_LOGIC;
      almost_full : out STD_LOGIC;

      rd_en       : in  STD_LOGIC;
      dout        : out STD_LOGIC_VECTOR (kWidthData-1 DOWNTO 0);
      empty       : out STD_LOGIC;
      almost_empty: out STD_LOGIC;
      valid       : out STD_LOGIC;

      prog_full   : out STD_LOGIC
    );
    end component;

  -- function get_rightmost --
  function get_rightmost( din : std_logic_vector  ) return std_logic_vector is
  begin
    --return din and ((not din)+"1");
    return din and std_logic_vector(unsigned((not din)) + 1 );
  end get_rightmost;

  -- function check_delimiter --
  function check_delimiter(din: std_logic_vector) return boolean is
    variable data_type  : std_logic_vector(kWidthDataType-1 downto 0);
  begin
    data_type := din(kMSBDataType downto kLSBDataType);
    return data_type=kDatatypeHeartbeat or data_type=kDatatypeGateOn or data_type=kDatatypeGateOff;
  end check_delimiter;

  -- function get_needread --
  function get_needread(  remainder             : std_logic_vector;
                          valid                 : std_logic_vector;
                          rden                  : std_logic_vector;
                          rd                    : std_logic_vector;
                          mask                  : std_logic_vector;
                          din                   : std_logic_vector;
                          flag_wait2nddelimiter : std_logic_vector;
                          flag_last1stdelimiter : boolean           ) return std_logic_vector is
    variable flag_delimiter : boolean;
  begin
    flag_delimiter  := check_delimiter(din);
    --if(remainder/="0")then
    if(remainder /= kZero)then
      return remainder;
    --elsif((flag_delimiter and (not (rd or mask)) = "0") or flag_last1stdelimiter)then   --last delimiter
    elsif((flag_delimiter and (not (rd or mask)) = kZero) or flag_last1stdelimiter)then   --last delimiter
      return valid and (not rden);
    --elsif((flag_delimiter and (rd and mask) = "0") or flag_wait2nddelimiter=rden) then  --other delimiter
    elsif((flag_delimiter and (rd and mask) = kZero) or flag_wait2nddelimiter=rden) then  --other delimiter
      return valid and (not rden) and (not rd) and (not mask);
    else                                        --normal
      return valid and (not rden) and (not mask);
    end if;
  end get_needread;

  -- check_hbfnum_mismatch --
  function check_hbfnum_mismatch(  hbf_number0 : std_logic_vector;
                          hbf_number1 : std_logic_vector ) return std_logic is
    variable result   : std_logic;
  begin
    if(hbf_number0 = hbf_number1) then
      result  := '0';
    else
      result  := '1';
    end if;
    return result;
  end check_hbfnum_mismatch;

  -- debug --
  signal  debug_valid     : std_logic_vector(kNumChBack-1 downto 0);
  signal  debug_needread  : std_logic_vector(kNumChBack-1 downto 0);
  signal  debug_remainder : std_logic_vector(kNumChBack-1 downto 0);

  attribute mark_debug : boolean;
  attribute mark_debug of mask_delimiter        : signal is enDEBUG;
  attribute mark_debug of flag_last1stdelimiter : signal is enDEBUG;
  attribute mark_debug of flag_wait2nddelimiter : signal is enDEBUG;
  attribute mark_debug of prog_full_outputfifo  : signal is enDEBUG;
  attribute mark_debug of hbfnum_mismatch       : signal is enDEBUG;
  attribute mark_debug of din_merger            : signal is enDEBUG;
  attribute mark_debug of reg_hbfnum            : signal is enDEBUG;

  -- Stop optimization --
  attribute keep  : string;
  attribute keep of mask_delimiter        : signal is "true";
  attribute keep of flag_last1stdelimiter : signal is "true";
  attribute keep of flag_wait2nddelimiter : signal is "true";
  attribute keep of prog_full_outputfifo  : signal is "true";
  attribute keep of debug_valid           : signal is "true";
  attribute keep of debug_needread        : signal is "true";
  attribute keep of debug_remainder       : signal is "true";
  attribute keep of rden_inputfifo        : signal is "true";
  attribute keep of is_read_inputfifo     : signal is "true";

begin
  -- ======================================================================
  --                                 body
  -- ======================================================================

  hbfNumMismatch  <= hbfnum_mismatch;
  progFullFifo    <= prog_full_outputfifo;

  --input fifo read
  rden_inputfifo_process : process(rst,clk)
    variable needread   : std_logic_vector(kNumChBack-1 downto 0);
    variable rden       : std_logic_vector(kNumChBack-1 downto 0);
    variable remainder  : std_logic_vector(kNumChBack-1 downto 0);
  begin
    if(rst = '1')then
      needread      := (others=>'0');
      rden          := (others=>'0');
      remainder     := (others=>'0');
      rden_inputfifo<= (others=>'0');
      reg_full      <= '0';
      needread_full <= (others=>'0');
    elsif(clk'event and clk = '1')then
      if((prog_full_outputfifo='0') and reg_full='1')then                                     -- end of full mode
        needread  := needread_full;
      --elsif((check_delimiter(din_merger) and (is_read_inputfifo and mask_delimiter) = "0" and flag_last1stdelimiter=false) or flag_wait2nddelimiter=rden_inputfifo)then  -- read the first delimiter or read the 2nd delimiter word when full mode
      elsif((check_delimiter(din_merger) and (is_read_inputfifo and mask_delimiter) = kZero and flag_last1stdelimiter=false) or flag_wait2nddelimiter=rden_inputfifo)then  -- read the first delimiter or read the 2nd delimiter word when full mode
        needread  := get_needread(remainder,not inputEmptyIn,rden_inputfifo,is_read_inputfifo,mask_delimiter,din_merger,flag_wait2nddelimiter,flag_last1stdelimiter);
      --elsif((inputAlmostEmptyIn and rden_inputfifo)/="0")then                                 --read input fifo is almost empty
      elsif((inputAlmostEmptyIn and rden_inputfifo) /= kZero)then                                 --read input fifo is almost empty
        needread  := get_needread(remainder,not inputEmptyIn,rden_inputfifo,is_read_inputfifo,mask_delimiter,din_merger,flag_wait2nddelimiter,flag_last1stdelimiter);
      --elsif(rden="0")then                                                                     --rden_inputfifo is empty (idle, not rden run)
      elsif(rden = kZero)then                                                                     --rden_inputfifo is empty (idle, not rden run)
        needread  := get_needread(remainder,not inputEmptyIn,rden_inputfifo,is_read_inputfifo,mask_delimiter,din_merger,flag_wait2nddelimiter,flag_last1stdelimiter);
      else
        needread  := needread;
      end if;
      if(prog_full_outputfifo='1')then  -- full
        if(reg_full='0')then              -- full start
          needread_full <= needread;      -- record "needread" static until the output fifo is not full
        end if;
        needread  := (others=>'0');
      else
        needread_full <= (others=>'0');
      end if;
      rden            := get_rightmost(needread);
      remainder       := needread and (not rden);
      rden_inputfifo  <= rden;
      reg_full        <= prog_full_outputfifo;
      -- debug
      debug_valid     <= not inputEmptyIn;
      debug_needread  <= needread;
      debug_remainder <= remainder;
    end if;
  end process;

  inputReadEnableOut  <= rden_inputfifo;

  --Merger input
  is_read_inputfifo <= inputValidIn;
--  rd_inputfifo_process : process(rst,clk)
--  begin
--    if(rst = '1') then
--      is_read_inputfifo  <= (others=>'0');
--    elsif(clk'event and clk = '1') then
--      is_read_inputfifo  <= rden_inputfifo;
--    end if;
--  end process;

  for_switch_input_to_output : for i in kNumChBack-1 downto 0 generate
    din_merger  <= inputDoutIn(i) when (is_read_inputfifo(i)='1') else (others=>'Z');
  end generate for_switch_input_to_output;

  --mask
  mask_process : process(rst,clk)
  begin
    if(rst = '1') then
      mask_delimiter        <= (others=>'0');
      flag_wait2nddelimiter <= (others=>'0');
      flag_last1stdelimiter <= false;
    elsif(clk'event and clk = '1')then
      if(check_delimiter(din_merger))then -- delimiter
        --if((not (is_read_inputfifo or mask_delimiter))="0")then  -- last 1st delimiter
        if((not (is_read_inputfifo or mask_delimiter)) = kZero)then  -- last 1st delimiter
          mask_delimiter        <= (others=>'0');
          flag_wait2nddelimiter <= is_read_inputfifo;
          flag_last1stdelimiter <= true;
        elsif(flag_last1stdelimiter)then                    -- last 2nd delimiter
          mask_delimiter        <= (others=>'0');
          flag_wait2nddelimiter <= (others=>'0');
          flag_last1stdelimiter <= false;
        --elsif((is_read_inputfifo and mask_delimiter) = "0")then    -- 1st delimiter
        elsif((is_read_inputfifo and mask_delimiter) = kZero)then    -- 1st delimiter
          mask_delimiter        <= mask_delimiter or is_read_inputfifo;
          flag_wait2nddelimiter <= is_read_inputfifo;
          flag_last1stdelimiter <= false;
        else                                                -- 2nd delimiter
          mask_delimiter        <= mask_delimiter;
          flag_wait2nddelimiter <= (others=>'0');
          flag_last1stdelimiter <= false;
        end if;
      end if;
    end if;
  end process;

  --output fifo
  outputfifo_process : process(rst,clk)
  begin
    if(rst = '1') then
      wren_outputfifo       <= '0';
      din_outputfifo        <= (others=>'Z');
      delimiter1st_flagbits <= (others=>'0');
      delimiter2nd_flagbits <= (others=>'0');

      hbfnum_is_registered  <= '0';
      hbfnum_mismatch  <= '0';
    elsif(clk'event and clk = '1')then
      if(check_delimiter(din_merger))then -- delimiter
        --if((not (is_read_inputfifo or mask_delimiter))="0" or flag_last1stdelimiter)then --- last delimiter
        if((not (is_read_inputfifo or mask_delimiter)) = kZero or flag_last1stdelimiter)then --- last delimiter
          wren_outputfifo <= '1';
          din_outputfifo  <= din_merger;
          --if(flag_wait2nddelimiter = "0")then   -- flag
          if(flag_wait2nddelimiter = kZero)then   -- flag
            --din_outputfifo(kMSBFlag downto kLSBFlag)  <= din_merger(kMSBFlag downto kLSBFlag) or delimiter1st_flagbits;
            din_outputfifo(kMSBFlag downto kLSBFlag)  <= din_merger(kMSBFlag downto kLSBFlag) or genFlagVector(kIndexLHbfNumMismatch, check_hbfnum_mismatch(din_merger(kMSBHBFrame downto kLSBHBFrame), reg_hbfnum) or hbfnum_mismatch);
--            din_outputfifo(kLSBFlag+0)  <= din_merger(kLSBFlag+0) or delimiter1st_flagbits(0);
--            din_outputfifo(kLSBFlag+1)  <= din_merger(kLSBFlag+1) or delimiter1st_flagbits(1);
--            din_outputfifo(kLSBFlag+2)  <= din_merger(kLSBFlag+2) or delimiter1st_flagbits(2);
--            din_outputfifo(kLSBFlag+3)  <= din_merger(kLSBFlag+3) or delimiter1st_flagbits(3);
--            din_outputfifo(kLSBFlag+4)  <= din_merger(kLSBFlag+4) or delimiter1st_flagbits(4);
--            din_outputfifo(kLSBFlag+5)  <= din_merger(kLSBFlag+5) or delimiter1st_flagbits(5);
--            din_outputfifo(kLSBFlag+6)  <= din_merger(kLSBFlag+6) or delimiter1st_flagbits(6);
--            din_outputfifo(kLSBFlag+7)  <= din_merger(kLSBFlag+7) or delimiter1st_flagbits(7) or check_hbfnum_mismatch(din_merger(kMSBHBFrame downto kLSBHBFrame), reg_hbfnum) or hbfnum_mismatch;
--            din_outputfifo(kLSBFlag+8)  <= din_merger(kLSBFlag+8) or delimiter1st_flagbits(8);
--            din_outputfifo(kLSBFlag+9)  <= din_merger(kLSBFlag+9) or delimiter1st_flagbits(9);

            delimiter1st_flagbits                         <= (others=>'0');

            hbfnum_mismatch  <= check_hbfnum_mismatch(din_merger(kMSBHBFrame downto kLSBHBFrame), reg_hbfnum) or hbfnum_mismatch;
          else
            din_outputfifo(kMSBFlag downto kLSBFlag)  <= din_merger(kMSBFlag downto kLSBFlag) or genFlagVector(kIndexLHbfNumMismatch, hbfnum_mismatch);
            --din_outputfifo(kMSBFlag downto kLSBFlag)  <= din_merger(kMSBFlag downto kLSBFlag) or delimiter2nd_flagbits;
            --din_outputfifo(kLSBFlag+0)  <= din_merger(kLSBFlag+0) or delimiter2nd_flagbits(0);
            --din_outputfifo(kLSBFlag+1)  <= din_merger(kLSBFlag+1) or delimiter2nd_flagbits(1);
            --din_outputfifo(kLSBFlag+2)  <= din_merger(kLSBFlag+2) or delimiter2nd_flagbits(2);
            --din_outputfifo(kLSBFlag+3)  <= din_merger(kLSBFlag+3) or delimiter2nd_flagbits(3);
            --din_outputfifo(kLSBFlag+4)  <= din_merger(kLSBFlag+4) or delimiter2nd_flagbits(4);
            --din_outputfifo(kLSBFlag+5)  <= din_merger(kLSBFlag+5) or delimiter2nd_flagbits(5);
            --din_outputfifo(kLSBFlag+6)  <= din_merger(kLSBFlag+6) or delimiter2nd_flagbits(6);
            --din_outputfifo(kLSBFlag+7)  <= din_merger(kLSBFlag+7) or delimiter2nd_flagbits(7) or hbfnum_mismatch;
            --din_outputfifo(kLSBFlag+8)  <= din_merger(kLSBFlag+8) or delimiter2nd_flagbits(8);
            --din_outputfifo(kLSBFlag+9)  <= din_merger(kLSBFlag+9) or delimiter2nd_flagbits(9);

            delimiter2nd_flagbits                         <= (others=>'0');

            hbfnum_is_registered  <= '0';
          end if;
        else                                                                        --- no last delimiter
          wren_outputfifo <= '0';
          din_outputfifo  <= (others=>'Z');
          --if(flag_wait2nddelimiter="0")then   -- record flag
          if(flag_wait2nddelimiter = kZero)then   -- record flag
            delimiter1st_flagbits <= din_merger(kMSBFlag downto kLSBFlag) or delimiter1st_flagbits;

            if(hbfnum_is_registered = '1') then
              hbfnum_mismatch  <= check_hbfnum_mismatch(din_merger(kMSBHBFrame downto kLSBHBFrame), reg_hbfnum) or hbfnum_mismatch;
            else
              hbfnum_is_registered  <= '1';
              reg_hbfnum            <= din_merger(kMSBHBFrame downto kLSBHBFrame);
            end if;
          else
            delimiter2nd_flagbits <= din_merger(kMSBFlag downto kLSBFlag) or delimiter2nd_flagbits;
          end if;
        end if;
      --elsif(is_read_inputfifo/="0")then        -- TDC data
      elsif(is_read_inputfifo /= kZero)then        -- TDC data
          wren_outputfifo <= '1';
          din_outputfifo  <= din_merger;
      else                                -- not data
        wren_outputfifo <= '0';
        din_outputfifo  <= (others=>'Z');
      end if;
    end if;
  end process;

  u_mergerBackFifo: mergerBackFifo port map(
    clk         => clk,
    srst        => rst,

    wr_en       => wren_outputfifo,
    din         => din_outputfifo,
    full        => full_outputfifo,
    almost_full => almost_empty_outputfifo,

    rd_en       => outputReadEnableIn,
    dout        => outputDoutOut,
    empty       => outputEmptyOut,
    almost_empty=> outputAlmostEmptyOut,
    valid       => outputValidOut,

    prog_full   => prog_full_outputfifo
  );

end Behavioral;
