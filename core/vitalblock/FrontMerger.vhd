library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defDataStructure.all;
use mylib.defMerger.all;

entity FrontMerger is
  generic (
    -- DEBUG --
    enDEBUG : boolean := false
  );
  port (
    clk     : in STD_LOGIC;   --base clock
    rst     : in STD_LOGIC;   --base reset

    inputReadEnableOut  : out STD_LOGIC_VECTOR (kNumChFront-1 downto 0);  --input fifo read enable
    inputDoutIn         : in  dMGRFrontType;                              --input fifo data out
    inputEmptyIn        : in  STD_LOGIC_VECTOR (kNumChFront-1 downto 0);  --input fifo empty flag
    inputAlmostEmptyIn  : in  STD_LOGIC_VECTOR (kNumChFront-1 downto 0);  --input fifo almost empty flag
    inputValidIn        : in  STD_LOGIC_VECTOR (kNumChFront-1 downto 0);  --input fifo valid flag

    outputReadEnableIn  : in  STD_LOGIC;                                  --output fifo read enable
    outputDoutOut       : out STD_LOGIC_VECTOR (kWidthData-1 downto 0);   --output fifo data out
    outputEmptyOut      : out STD_LOGIC;                                  --output fifo empty flag
    outputAlmostEmptyOut: out STD_LOGIC;                                  --output fifo almost empty flag
    outputValidOut      : out STD_LOGIC                                   --output fifo valid flag
  );
end FrontMerger;

architecture Behavioral of FrontMerger is
  constant kZero                : std_logic_vector(kNumChFront-1 downto 0):= (others => '0');

  -- read input fifo --
  signal rden_inputfifo         : std_logic_vector(kNumChFront-1 downto 0);
  signal is_read_inputfifo      : std_logic_vector(kNumChFront-1 downto 0);

  -- data input --
  signal din_merger             : std_logic_vector(kWidthData-1 downto 0);

  -- Flag for delimiter work --
  signal mask_delimiter         : std_logic_vector(kNumChFront-1 downto 0);
  signal flag_last1stdelimiter  : boolean;
  signal flag_wait2nddelimiter  : std_logic_vector(kNumChFront-1 downto 0);

  -- Flag in delimiter words --
  signal delimiter1st_flagbits  : std_logic_vector(kWidthDelimiterFlag-1 downto 0);
  signal delimiter2nd_flagbits  : std_logic_vector(kWidthDelimiterFlag-1 downto 0);

  -- full --
  signal reg_full               : std_logic;
  signal needread_full          : std_logic_vector(kNumChFront-1 downto 0);

  -- write output fifo --
  signal wren_outputfifo        : std_logic;
  signal din_outputfifo         : std_logic_vector(kWidthData-1 downto 0);
  signal full_outputfifo        : std_logic;
  signal almost_empty_outputfifo: std_logic;
  signal prog_full_outputfifo   : std_logic;

  component mergerFrontFifo is
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
    return din and std_logic_vector(unsigned((not din)) + 1);
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
    if(remainder /= kZero)then
      return remainder;
    elsif((flag_delimiter and (not (rd or mask)) = kZero) or flag_last1stdelimiter)then   --last delimiter
      return valid and (not rden);
    elsif((flag_delimiter and (rd and mask) = kZero) or flag_wait2nddelimiter=rden) then  --other delimiter
      return valid and (not rden) and (not rd) and (not mask);
    else                                        --normal
      return valid and (not rden) and (not mask);
    end if;
  end get_needread;

  -- debug --
  signal  debug_valid     : std_logic_vector(kNumChFront-1 downto 0);
  signal  debug_needread  : std_logic_vector(kNumChFront-1 downto 0);
  signal  debug_remainder : std_logic_vector(kNumChFront-1 downto 0);

  attribute mark_debug : boolean;
  attribute mark_debug of mask_delimiter        : signal is enDEBUG;
  attribute mark_debug of flag_last1stdelimiter : signal is enDEBUG;
  attribute mark_debug of flag_wait2nddelimiter : signal is enDEBUG;
  attribute mark_debug of prog_full_outputfifo  : signal is enDEBUG;
  attribute mark_debug of debug_valid           : signal is enDEBUG;
  attribute mark_debug of debug_needread        : signal is enDEBUG;
  attribute mark_debug of debug_remainder       : signal is enDEBUG;
  attribute mark_debug of rden_inputfifo        : signal is enDEBUG;
  attribute mark_debug of is_read_inputfifo     : signal is enDEBUG;

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

  --input fifo read
  rden_inputfifo_process : process(rst,clk)
    variable needread   : std_logic_vector(kNumChFront-1 downto 0);
    variable rden       : std_logic_vector(kNumChFront-1 downto 0);
    variable remainder  : std_logic_vector(kNumChFront-1 downto 0);
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
      elsif((check_delimiter(din_merger) and (is_read_inputfifo and mask_delimiter) = kZero and flag_last1stdelimiter=false) or flag_wait2nddelimiter=rden_inputfifo)then  -- read the first delimiter or read the 2nd delimiter word when full mode
        needread  := get_needread(remainder,not inputEmptyIn,rden_inputfifo,is_read_inputfifo,mask_delimiter,din_merger,flag_wait2nddelimiter,flag_last1stdelimiter);
      elsif((inputAlmostEmptyIn and rden_inputfifo )/= kZero)then                                 --read input fifo is almost empty
        needread  := get_needread(remainder,not inputEmptyIn,rden_inputfifo,is_read_inputfifo,mask_delimiter,din_merger,flag_wait2nddelimiter,flag_last1stdelimiter);
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

  -- Merge input
  is_read_inputfifo <= inputValidIn;
--  rd_inputfifo_process : process(rst,clk)
--  begin
--    if(rst = '1') then
--      is_read_inputfifo  <= (others=>'0');
--    elsif(clk'event and clk = '1') then
--      is_read_inputfifo  <= rden_inputfifo;
--    end if;
--  end process;


  for_switch_input_to_output : for i in kNumChFront-1 downto 0 generate
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
        if((not (is_read_inputfifo or mask_delimiter)) = kZero)then  -- last 1st delimiter
          mask_delimiter        <= (others=>'0');
          flag_wait2nddelimiter <= is_read_inputfifo;
          flag_last1stdelimiter <= true;
        elsif(flag_last1stdelimiter)then                    -- last 2nd delimiter
          mask_delimiter        <= (others=>'0');
          flag_wait2nddelimiter <= (others=>'0');
          flag_last1stdelimiter <= false;
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
      wren_outputfifo   <= '0';
      din_outputfifo    <= (others=>'Z');
      delimiter1st_flagbits <= (others=>'0');
      delimiter2nd_flagbits <= (others=>'0');
    elsif(clk'event and clk = '1')then
      if(check_delimiter(din_merger))then -- delimiter
        if((not (is_read_inputfifo or mask_delimiter)) = kZero or flag_last1stdelimiter)then --- last delimiter
          wren_outputfifo <= '1';
          din_outputfifo  <= din_merger;
          if(flag_wait2nddelimiter = kZero)then   -- flag
            din_outputfifo(kMSBFlag downto kLSBFlag)  <= din_merger(kMSBFlag downto kLSBFlag) or delimiter1st_flagbits;
            delimiter1st_flagbits                     <= (others=>'0');
          else
            din_outputfifo(kMSBFlag downto kLSBFlag)  <= din_merger(kMSBFlag downto kLSBFlag) or delimiter2nd_flagbits;
            delimiter2nd_flagbits                     <= (others=>'0');
          end if;
        else                                                                        --- no last delimiter
          wren_outputfifo <= '0';
          din_outputfifo  <= (others=>'Z');
          if(flag_wait2nddelimiter = kZero)then   -- record flag
            delimiter1st_flagbits <= din_merger(kMSBFlag downto kLSBFlag) or delimiter1st_flagbits;
          else
            delimiter2nd_flagbits <= din_merger(kMSBFlag downto kLSBFlag) or delimiter2nd_flagbits;
          end if;
        end if;
      elsif(is_read_inputfifo /= kZero)then        -- TDC data
          wren_outputfifo <= '1';
          din_outputfifo  <= din_merger;
      else                                -- not data
        wren_outputfifo <= '0';
        din_outputfifo  <= (others=>'Z');
      end if;
    end if;
  end process;

  u_mergerFrontFifo: mergerFrontFifo port map(
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
