library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defChannel.all;
use mylib.defDelimiter.all;

entity IncomingBuffer is
  generic (
    -- DEBUG --
    enDEBUG : boolean := false
  );
  port (
    clk     : in  STD_LOGIC;  -- base clock
    rst     : in  STD_LOGIC;  -- base reset

    -- input  (ODP block)
    ODPWriteEnableIn    : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); -- TDC data write enable
    ODPDinIn            : in  dDataType;                                  -- TDC data data in

    -- flag   (system)
    bufferProgFull      : out STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --incomming FIFO prog full

    -- output (merger unit)
    outputReadEnableIn  : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo read enable
    outputDoutOut       : out dDataType;                                  --input fifo data out
    outputEmptyOut      : out STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo empty flag
    outputAlmostEmptyOut: out STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo almost empty flag
    outputValidOut      : out STD_LOGIC_VECTOR (kNumStrInput-1 downto 0)  --input fifo valid flag
  );
end IncomingBuffer;

architecture Behavioral of IncomingBuffer is

  -- Flag --
  signal flag_1st_delimiter : std_logic_vector(kNumStrInput-1 downto 0);  -- indicate that waitting for the 2nd delimiter
  signal flag_data_lost     : std_logic_vector(kNumStrInput-1 downto 0);  -- indicate that the TDC data is lost in this delimiter frame
  signal is_delimiter       : std_logic_vector(kNumStrInput-1 downto 0);  -- indicate that the TDC data is lost in this delimiter frame

  -- Incoming FIFO --
  signal wren_fifo         : std_logic_vector(kNumStrInput-1 downto 0);
  signal din_fifo          : dDataType;
  signal full_fifo         : std_logic_vector(kNumStrInput-1 downto 0);
  signal almost_full_fifo  : std_logic_vector(kNumStrInput-1 downto 0);
  signal wr_ack_fifo       : std_logic_vector(kNumStrInput-1 downto 0);

  constant kNumBitDepthIncomingFifo : positive  := 6;
  type dDateCountIncomingFifoType is array ( integer range kNumStrInput-1 downto 0) of std_logic_vector(kNumBitDepthIncomingFifo-1 downto 0); -- for count the data count of incoming FIFO
  signal data_count_fifo   : dDateCountIncomingFifoType;
  signal prog_full_fifo    : std_logic_vector(kNumStrInput-1 downto 0);

  component incomingFifo is
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

      data_count  : out STD_LOGIC_VECTOR (kNumBitDepthIncomingFifo-1 DOWNTO 0);
      prog_full   : out STD_LOGIC
    );
    end component;

  -- function check_delimiter --
  function check_delimiter( dInDataType : std_logic_vector  ) return boolean is
  begin
    if(dInDataType=kDatatypeHeartbeat)then   -- heartbeat
        return true;
    else
        return false;
    end if;
  end check_delimiter;

  attribute mark_debug : boolean;
  --attribute mark_debug of flag_1st_delimiter   : signal is enDEBUG;
  --attribute mark_debug of flag_data_lost       : signal is enDEBUG;
  attribute mark_debug of wren_fifo            : signal is enDEBUG;
  attribute mark_debug of is_delimiter         : signal is enDEBUG;
  --attribute mark_debug of din_fifo             : signal is enDEBUG;
  --attribute mark_debug of data_count_fifo      : signal is enDEBUG;
  attribute mark_debug of prog_full_fifo       : signal is enDEBUG;
  attribute mark_debug of full_fifo      : signal is enDEBUG;
  attribute mark_debug of almost_full_fifo      : signal is enDEBUG;

begin

  --bufferProgFull  <= '0' when unsigned(prog_full_fifo) = 0 else '1';
  bufferProgFull  <= prog_full_fifo;

  for_process :for i in kNumStrInput-1 downto 0 generate
  begin

    -- 1st/2nd delimiter
    delimiter_1st_process : process(rst,clk)
    begin
      if(rst = '1') then
        flag_1st_delimiter(i)  <= '0';
      elsif(clk'event and clk = '1') then
        if(check_delimiter(ODPDinIn(i)(kPosHbdDataType'range))) then  -- delimiter word
          if(flag_1st_delimiter(i) = '1')then  -- 2nd delimiter
            flag_1st_delimiter(i)  <='0';
          else                          -- 1st delimiter
            flag_1st_delimiter(i)  <='1';
          end if;
        end if;
      end if;
    end process;

    -- outputfifo
    outputfifo_process : process(rst,clk)
    begin
      if(rst = '1') then
        flag_data_lost(i) <= '0';
        wren_fifo(i) <= '0';
        din_fifo(i)  <= (others=>'0');
      elsif(clk'event and clk = '1') then
        if(ODPWriteEnableIn(i) = '1') then -- There are data from the ODP block
          if(check_delimiter(ODPDinIn(i)(kMSBDataType downto kLSBDataType)))then -- delimiter word
            is_delimiter(i) <= '1';

            if(flag_1st_delimiter(i) = '1') then  -- deassert lost flag when 2nd delimiter
              flag_data_lost(i) <= '0';
            end if;
            wren_fifo(i) <= '1';
            din_fifo(i)  <= ODPDinIn(i);
            if(unsigned(flag_data_lost) /= 0) then             -- insert lost flag
              din_fifo(i)(kIndexDataLost + kLSBFlag)  <= '1';
            end if;
          elsif(prog_full_fifo(i) /= '0') then  -- incoming FIFO is almost full
            flag_data_lost(i) <= '1';               -- assert lost flag
            wren_fifo(i)      <= '0';
            din_fifo(i)       <= (others=>'0');
          else                          -- TDC data
            wren_fifo(i) <= '1';
            din_fifo(i)  <= ODPDinIn(i);
          end if;
        else
          is_delimiter(i) <= '0';                        -- no data
          wren_fifo(i) <= '0';
          din_fifo(i)  <= (others=>'0');
        end if;
      end if;
    end process;

    -- incoming FIFO
    u_incomingFifo: incomingFifo port map(
      clk         => clk,
      srst        => rst,

      wr_en       => wren_fifo(i),
      din         => din_fifo(i),
      full        => full_fifo(i),
      almost_full => almost_full_fifo(i),

      rd_en       => outputReadEnableIn(i),
      dout        => outputDoutOut(i),
      empty       => outputEmptyOut(i),
      almost_empty=> outputAlmostEmptyOut(i),
      valid       => outputValidOut(i),

      data_count  => data_count_fifo(i),
      prog_full   => prog_full_fifo(i)
    );

  end generate for_process;

end Behavioral;
