library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library mylib;
use mylib.defChannel.all;
use mylib.defDelimiter.all;

package defMerger is

  -- Merger block structure for LR-TDC --
  --        delimiter unit
  --            ||||| 128 channel
  --  (front) 32-to-1 merger * 4
  --             |||  4   channel
  --  ( back)  4-to-1 merger * 1
  --              |   1   channel
  --

  -- Merger block structure for HR-TDC --
  --        delimiter unit
  --            ||||| 32 channel * 2 mezzanine cards
  --  (front) 32-to-1 merger * 2
  --             |||  2   channel
  --  ( back)  2-to-1 merger * 1
  --              |   1   channel
  --

  -- number of Channal
  constant kNumChFront  : positive  := 32;                        -- input channal number of mergerFront
  constant kNumMRGFront : positive  := kNumStrInput/kNumChFront;  -- number of mergerFront
  constant kNumChBack   : positive  := kNumMRGFront;              -- input channal number of mergerBack

  -- data type
  type dMGRFrontType    is array ( integer range kNumChFront-1  downto 0) of std_logic_vector(kWidthData-1 downto 0); -- mergerFront din interface
  type dMGRBackType     is array ( integer range kNumChBack-1   downto 0) of std_logic_vector(kWidthData-1 downto 0); -- mergerBack din interface


end package defMerger;
