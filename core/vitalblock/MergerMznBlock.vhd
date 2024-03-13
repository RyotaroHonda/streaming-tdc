library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library mylib;
use mylib.defChannel.all;
use mylib.defDataStructure.all;
use mylib.defMerger.all;

entity MergerMznBlock is
  generic (
    -- DEBUG --
    enDEBUG : boolean := false
  );
  port (
    clk     : in STD_LOGIC;   --base clock
    rst     : in STD_LOGIC;   --base reset

    inputReadEnableOut  : out STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo read enable
    inputDoutIn         : in  dDataType;                                  --input fifo data out
    inputEmptyIn        : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo empty flag
    inputAlmostEmptyIn  : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo almost empty flag
    inputValidIn        : in  STD_LOGIC_VECTOR (kNumStrInput-1 downto 0); --input fifo valid flag

    outputReadEnableIn  : in  STD_LOGIC;                                  --output fifo read enable
    outputDoutOut       : out STD_LOGIC_VECTOR (kWidthData-1 downto 0);   --output fifo data out
    outputEmptyOut      : out STD_LOGIC;                                  --output fifo empty flag
    outputAlmostEmptyOut: out STD_LOGIC;                                  --output fifo almost empty flag
    outputValidOut      : out STD_LOGIC                                   --output fifo valid flag
  );
end MergerMznBlock;

architecture Behavioral of MergerMznBlock is

  -- between input and mergerFront
  type dMGRFrontArrayType is array ( integer range kNumMRGFront-1 downto 0) of dMGRFrontType; -- for dividing merger din interface
  signal din_to_front  : dMGRFrontArrayType;

  -- between mergerFront and mergerBack
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
    din_to_front(i)  <= conversion_dMGRFrontType(inputDoutIn,i);

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

  rden_to_back(0)               <= outputReadEnableIn;
  outputDoutOut         <= dout_from_front(0);
  outputEmptyOut        <= empty_from_front(0);
  outputAlmostEmptyOut  <= almost_empty_from_front(0);
  outputValidOut        <= valid_from_front(0);

end Behavioral;
