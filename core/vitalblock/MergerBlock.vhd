library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library mylib;
use mylib.defChannel.all;
use mylib.defDataStructure.all;
use mylib.defMerger.all;

entity MergerBlock is
  generic (
    -- DEBUG --
    enDEBUG : boolean := false
  );
  port (
    clk                 : in STD_LOGIC;  --base clock
    rst                 : in STD_LOGIC;  --base reset
    hbfNumMismatch      : out std_logic; -- Local heartbeat number mismatch

    inputReadEnableOut  : out STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo read enable
    inputDoutIn         : in  dDataType;                                  --input fifo data out
    inputEmptyIn        : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo empty_from_front flag
    inputAlmostEmptyIn  : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo almost empty flag
    inputValidIn        : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo valid flag

    outputReadEnableIn  : in  STD_LOGIC;                                  --output fifo read enable
    outputDoutOut       : out STD_LOGIC_VECTOR (kWidthData-1 downto 0);   --output fifo data out
    outputEmptyOut      : out STD_LOGIC;                                  --output fifo empty flag
    outputAlmostEmptyOut: out STD_LOGIC;                                  --output fifo almost empty flag
    outputValidOut      : out STD_LOGIC                                   --output fifo valid flag
  );
end MergerBlock;

architecture Behavioral of MergerBlock is

  -- System --
  signal local_hbf_num_mismatch   : std_logic;

  -- between input and FrontMerger
  type dMGRFrontArrayType is array ( integer range kNumMRGFront-1 downto 0) of dMGRFrontType; -- for dividing merger din interface
  signal din_to_front : dMGRFrontArrayType;

  -- between FrontMerger and BackMerger
  signal rden_to_back             : std_logic_vector(kNumChBack-1 downto 0);
  signal dout_from_front          : dMGRBackType;
  signal empty_from_front         : std_logic_vector(kNumChBack-1 downto 0);
  signal almost_empty_from_front  : std_logic_vector(kNumChBack-1 downto 0);
  signal valid_from_front         : std_logic_vector(kNumChBack-1 downto 0);

  function conversion_dMGRFrontType(  din     : dDataType;
                                      offset  : integer   ) return dMGRFrontType is
    variable dout : dMGRFrontType;
  begin
    for i in kNumChFront-1 downto 0 loop
      dout(i) := din(i+offset*kNumChFront);
    end loop;
    return dout;
  end conversion_dMGRFrontType;

  attribute mark_debug : boolean;
  attribute mark_debug of rden_to_back  : signal is enDEBUG;
  attribute mark_debug of empty_from_front : signal is enDEBUG;

begin

  -- merger unit 32 to 1
  for_mergerFront: for i in kNumMRGFront-1 downto 0 generate
  begin
    din_to_front(i)  <= conversion_dMGRFrontType(inputDoutIn,i); -- 128ch -> 32ch*4

    u_mergerFront: entity mylib.FrontMerger
    generic map(
      enDEBUG => enDEBUG
    )
    port map(
      clk     => clk,
      rst     => rst,

      inputReadEnableOut  => inputReadEnableOut((i+1)*kNumChFront-1 downto i*kNumChFront),
      inputDoutIn         => din_to_front(i),
      inputEmptyIn        => inputEmptyIn((i+1)*kNumChFront-1 downto i*kNumChFront),
      inputAlmostEmptyIn  => inputAlmostEmptyIn((i+1)*kNumChFront-1 downto i*kNumChFront),
      inputValidIn        => inputValidIn((i+1)*kNumChFront-1 downto i*kNumChFront),

      outputReadEnableIn  => rden_to_back(i),
      outputDoutOut       => dout_from_front(i),
      outputEmptyOut      => empty_from_front(i),
      outputAlmostEmptyOut=> almost_empty_from_front(i),
      outputValidOut      => valid_from_front(i)
    );
  end generate for_mergerFront;

  -- merger unit 4 to 1
  u_mergerBack: entity mylib.BackMerger
  generic map(
    enDEBUG => enDEBUG
  )
  port map(
    clk                 => clk,
    rst                 => rst,
    progFullFifo        => open,
    hbfNumMismatch      => local_hbf_num_mismatch,

    inputReadEnableOut  => rden_to_back,
    inputDoutIn         => dout_from_front,
    inputEmptyIn        => empty_from_front,
    inputAlmostEmptyIn  => almost_empty_from_front,
    inputValidIn        => valid_from_front,

    outputReadEnableIn  => outputReadEnableIn,
    outputDoutOut       => outputDoutOut,
    outputEmptyOut      => outputEmptyOut,
    outputAlmostEmptyOut=> outputAlmostEmptyOut,
    outputValidOut      => outputValidOut
  );

end Behavioral;
