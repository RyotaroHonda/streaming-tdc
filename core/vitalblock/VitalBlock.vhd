library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

library mylib;
use mylib.defChannel.all;
use mylib.defDataStructure.all;

entity VitalBlock is
  generic (
    -- Type definition
    kTdcType : string := "LRTDC"; -- "LRTDC" or "HRTDC"
    -- DEBUG --
    enDEBUG : boolean := false
  );
  port (
    rst                 : in STD_LOGIC;  -- User reset (asynchronous)
    clk                 : in STD_LOGIC;  -- Base clock
    lhbfNumMismatch     : out std_logic; -- Local heartbeat frame num mismatch

    -- ODPBlock input --
    ODPWriteEnableIn    : in  std_logic_vector(kNumStrInput-1 downto 0); -- TDC data write enable
    ODPDinIn            : in  dDataType;                                  -- TDC data data in
    hbCount             : in  std_logic_vector(kWidthHBCount-1 downto 0);

    -- Stcp flag --
    bufferProgFull      : out std_logic;                                  -- Incomming buffer prog full flag

    -- Throttling status --
    outThrottlingOn     : out std_logic;                                  -- Output throttling status
    inThrottlingT2On    : out std_logic;                                  -- Input throttling Type2 status

    -- Link buf status --
    pfullLinkBufIn      : in std_logic;
    emptyLinkInBufIn    : in std_logic;

    -- output --
    outputReadEnableIn  : in  STD_LOGIC;                                  --output fifo read enable
    outputDoutOut       : out STD_LOGIC_VECTOR (kWidthData-1 downto 0);   --output fifo data out
    outputEmptyOut      : out STD_LOGIC;
    outputAlmostEmptyOut: out STD_LOGIC;
    outputValidOut      : out STD_LOGIC                                   --output fifo valid flag
  );
end VitalBlock;

architecture Behavioral of VitalBlock is

  -- System --
  signal sync_reset             : std_logic;
  signal incoming_buf_pfull     : std_logic_vector(kNumStrInput-1 downto 0);
  signal input_throttling_type2_on : std_logic;
  signal output_throttling_on   : std_logic;
  signal local_hbf_num_mismatch : std_logic;

  -- Input Throttling Type2 --
  signal valid_ithrottling      : std_logic_vector(kNumStrInput-1 downto 0);
  signal dout_ithrottling       : dDataType;
  signal inthrottling_is_working : std_logic_vector(kNumStrInput-1 downto 0);

  signal t2start_insert_request, t2start_insert_ack   : std_logic_vector(kNumStrInput-1 downto 0);
  signal t2end_insert_request, t2end_insert_ack   : std_logic_vector(kNumStrInput-1 downto 0);


  -- incoming unit -> merger unit
  signal rden_incoming          : std_logic_vector(kNumStrInput-1 downto 0);
  signal dout_incoming          : dDataType;
  signal empty_incoming         : std_logic_vector(kNumStrInput-1 downto 0);
  signal almost_empty_incoming  : std_logic_vector(kNumStrInput-1 downto 0);
  signal valid_incoming         : std_logic_vector(kNumStrInput-1 downto 0);

  -- Merger to OutputThrottling
  signal dout_merger_out        : STD_LOGIC_VECTOR (kWidthData-1 downto 0);   --output fifo data out
  signal valid_merger_out       : STD_LOGIC;
  signal read_enable_to_merger  : STD_LOGIC;

  attribute mark_debug : boolean;
--  attribute mark_debug of rden_incoming   : signal is enDEBUG;
--  attribute mark_debug of empty_incoming  : signal is enDEBUG;
  attribute mark_debug of inthrottling_is_working  : signal is enDEBUG;

  attribute mark_debug of valid_ithrottling  : signal is enDEBUG;
  attribute mark_debug of t2start_insert_request  : signal is enDEBUG;
  attribute mark_debug of t2start_insert_ack  : signal is enDEBUG;
  attribute mark_debug of t2end_insert_request  : signal is enDEBUG;
  attribute mark_debug of t2end_insert_ack  : signal is enDEBUG;

begin

  lhbfNumMismatch <= local_hbf_num_mismatch;

  bufferProgFull  <= '0' when(unsigned(incoming_buf_pfull) = 0) else '1';
  inThrottlingT2On  <= input_throttling_type2_on;
  outThrottlingOn   <= output_throttling_on;

  input_throttling_type2_on   <= '0' when(unsigned(inthrottling_is_working) = 0) else '1';


  -- Input Throttling Type2 --
  gen_ithrottle_t2 : for i in 0 to kNumStrInput-1 generate
  begin
    u_ITT2 : entity mylib.InputThrottlingType2
      generic map(
        kChannel  => i,
        enDEBUG   => false
      )
      port map(
        rst     => sync_reset,
        clk     => clk,

        -- status input --
        ibufProgFullIn      => incoming_buf_pfull(i),
        emptyIbufIn         => empty_incoming(i),

        -- Heartbeat count for TDC
        hbCount             => hbCount,

        -- Status output --
        isWorking           => inthrottling_is_working(i),
        t2startReq          => t2start_insert_request(i),
        t2startAck          => t2start_insert_ack(i),
        t2endReq            => t2end_insert_request(i),
        t2endAck            => t2end_insert_ack(i),

        -- Data In --
        validIn             => ODPWriteEnableIn(i),
        dIn                 => ODPDinIn(i),

        -- Data Out --
        validOut            => valid_ithrottling(i),
        dOut                => dout_ithrottling(i)

      );
  end generate;


  -- IncomingBuffer --
  u_incoming_buffer: entity mylib.IncomingBuffer
    generic map(
      enDEBUG => false
    )
    port map(
      clk     => clk,
      rst     => sync_reset,

      ODPWriteEnableIn    => ODPWriteEnableIn,
      ODPDinIn            => ODPDinIn,

      bufferProgFull      => incoming_buf_pfull,

      outputReadEnableIn  => rden_incoming,
      outputDoutOut       => dout_incoming,
      outputEmptyOut      => empty_incoming,
      outputAlmostEmptyOut=> almost_empty_incoming,
      outputValidOut      => valid_incoming

    );

  -- MergerBlock --
  gen_hrtdc : if kTdcType = "HRTDC" generate
  begin

    output_throttling_on  <= '0';
    outputValidOut          <= valid_merger_out;
    outputDoutOut           <= dout_merger_out;


    u_merger_block: entity mylib.MergerMznBlock
      generic map(
        enDEBUG => false
      )
      port map(
        clk    => clk,
        rst    => sync_reset,

        inputReadEnableOut  => rden_incoming,
        inputDoutIn         => dout_incoming,
        inputEmptyIn        => empty_incoming,
        inputAlmostEmptyIn  => almost_empty_incoming,
        inputValidIn        => valid_incoming,

        outputReadEnableIn  => outputReadEnableIn,
        outputDoutOut       => dout_merger_out,
        outputEmptyOut      => outputEmptyOut,
        outputAlmostEmptyOut=> outputAlmostEmptyOut,
        outputValidOut      => valid_merger_out
      );
  end generate;

  gen_lrtdc : if kTdcType = "LRTDC" generate
  begin

    read_enable_to_merger   <= '1' when(output_throttling_on = '1') else outputReadEnableIn;

    u_merger_block: entity mylib.MergerBlock
      generic map(
        enDEBUG => false
      )
      port map(
        clk                 => clk,
        rst                 => sync_reset,
        hbfNumMismatch      => local_hbf_num_mismatch,

        inputReadEnableOut  => rden_incoming,
        inputDoutIn         => dout_incoming,
        inputEmptyIn        => empty_incoming,
        inputAlmostEmptyIn  => almost_empty_incoming,
        inputValidIn        => valid_incoming,

        outputReadEnableIn  => read_enable_to_merger,
        outputDoutOut       => dout_merger_out,
        outputEmptyOut      => outputEmptyOut,
        outputAlmostEmptyOut=> outputAlmostEmptyOut,
        outputValidOut      => valid_merger_out
      );

    u_OutThrottle: entity mylib.OutputThrottling
      generic map(
        enDEBUG => false
      )
      port map(
        rst     => sync_reset,
        clk     => clk,

        -- status input --
        intputThrottlingOn  => input_throttling_type2_on,
        pfullLinkIn         => pfullLinkBufIn,
        emptyLinkIn         => emptyLinkInBufIn,

        -- Status output --
        isWorking           => output_throttling_on,

        -- Data In --
        validIn             => valid_merger_out,
        dIn                 => dout_merger_out,

        -- Data Out --
        validOut            => outputValidOut,
        dOut                => outputDoutOut

      );
  end generate;




  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clk, sync_reset);

end Behavioral;
