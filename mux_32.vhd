library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity mux_32 is
    Port ( in0 : in  STD_LOGIC_VECTOR (31 downto 0);
           in1 : in  STD_LOGIC_VECTOR (31 downto 0);
           ctl : in  STD_LOGIC;
           result : out STD_LOGIC_VECTOR (31 downto 0));
end mux_32;

architecture Synthesizable of mux_32 is

begin

   with ctl select
      result <= in0 when '0',
                in1 when others;

end Synthesizable;
