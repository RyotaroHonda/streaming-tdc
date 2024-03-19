library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library mylib;
use mylib.defDataBusAbst.all;
use mylib.defDelimiter.all;

entity MergerBlock is
  generic (
    kNumInput         : integer:= 32;
    enDEBUG : boolean := false
  );
  port (
    clk                 : in STD_LOGIC;
    syncReset           : in STD_LOGIC;  --Synchronous reset
    hbfNumMismatch      : out std_logic; -- Local heartbeat number mismatch

    rdenOut             : out STD_LOGIC_VECTOR (kNumInput-1 downto 0); --input fifo read enable
    dataIn              : in  DataArrayType(kNumInput-1 downto 0);     --input fifo data out
    emptyIn             : in  STD_LOGIC_VECTOR (kNumInput-1 downto 0); --input fifo empty_from_front flag
    almostEmptyIn       : in  STD_LOGIC_VECTOR (kNumInput-1 downto 0); --input fifo almost empty flag
    validIn             : in  STD_LOGIC_VECTOR (kNumInput-1 downto 0); --input fifo valid flag

    rdenIn              : in  STD_LOGIC;                                  --output fifo read enable
    dataOut             : out STD_LOGIC_VECTOR (kWidthData-1 downto 0);   --output fifo data out
    emptyOut            : out STD_LOGIC;                                  --output fifo empty flag
    almostEmptyOut      : out STD_LOGIC;                                  --output fifo almost empty flag
    validOut            : out STD_LOGIC                                   --output fifo valid flag
  );
end MergerBlock;

architecture Behavioral of MergerBlock is

  -- System --
  constant kDivisionRatio         : integer:= 4;
  constant kNumFrontInput         : integer:= kNumInput/4;

  signal local_hbf_num_mismatch   : std_logic;
  signal local_hbf_num_mismatch_front   : std_logic;
  signal local_hbf_num_mismatch_back    : std_logic;

  -- between input and FrontMerger

  -- between FrontMerger and BackMerger
  signal rden_to_back             : std_logic_vector(kDivisionRatio-1 downto 0);
  signal dout_from_front          : DataArrayType(kDivisionRatio-1 downto 0);
  signal empty_from_front         : std_logic_vector(kDivisionRatio-1 downto 0);
  signal almost_empty_from_front  : std_logic_vector(kDivisionRatio-1 downto 0);
  signal valid_from_front         : std_logic_vector(kDivisionRatio-1 downto 0);

  attribute mark_debug : boolean;
  attribute mark_debug of rden_to_back  : signal is enDEBUG;
  attribute mark_debug of empty_from_front : signal is enDEBUG;

begin

  hbfNumMismatch  <= local_hbf_num_mismatch_front or local_hbf_num_mismatch_back;

  -- merger unit 32 to 1
  gen_mergerFront: for i in kDivisionRatio-1 downto 0 generate
  begin

    u_mergerFront: entity mylib.MergerUnit
    generic map(
      kType       => "Front",
      kNumInput   => kNumFrontInput,
      enDEBUG     => enDEBUG
    )
    port map(
      clk             => clk,
      syncReset       => syncReset,
      progFullFifo    => open,
      hbfNumMismatch  => local_hbf_num_mismatch_front,

      rdenOut         => rdenOut((i+1)*kNumFrontInput-1 downto i*kNumFrontInput),
      dataIn          => dataIn(kNumFrontInput*(i+1)-1 downto kNumFrontInput*i),
      emptyIn         => emptyIn((i+1)*kNumFrontInput-1 downto i*kNumFrontInput),
      almostEmptyIn   => almostEmptyIn((i+1)*kNumFrontInput-1 downto i*kNumFrontInput),
      validIn         => validIn((i+1)*kNumFrontInput-1 downto i*kNumFrontInput),

      rdenIn          => rden_to_back(i),
      dataOut         => dout_from_front(i),
      emptyOut        => empty_from_front(i),
      almostEmptyOut  => almost_empty_from_front(i),
      validOut        => valid_from_front(i)
    );
  end generate;

  -- merger unit 4 to 1
  u_mergerBack: entity mylib.MergerUnit
  generic map(
    kType       => "Back",
    kNumInput   => kNumFrontInput,
    enDEBUG     => enDEBUG
  )
  port map(
    clk                 => clk,
    syncReset           => syncReset,
    progFullFifo        => open,
    hbfNumMismatch      => local_hbf_num_mismatch_back,

    rdenOut             => rden_to_back,
    dataIn              => dout_from_front,
    emptyIn             => empty_from_front,
    almostEmptyIn       => almost_empty_from_front,
    validIn             => valid_from_front,

    rdenIn              => rdenIn,
    dataOut             => dataOut,
    emptyOut            => emptyOut,
    almostEmptyOut      => almostEmptyOut,
    validOut            => validOut
  );

end Behavioral;
