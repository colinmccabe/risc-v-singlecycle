library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity computer is
    Port ( clk : in  STD_LOGIC;
           seg : out  STD_LOGIC_VECTOR (7 downto 0);
           an : out  STD_LOGIC_VECTOR (3 downto 0));
end computer;

architecture Behavioral of computer is

   signal div : STD_LOGIC_VECTOR(12 downto 0);

   COMPONENT sevenseg
      PORT(
         num : IN std_logic_vector(15 downto 0);
         clk : IN std_logic;          
         anodes : OUT std_logic_vector(3 downto 0);
         cathodes : OUT std_logic_vector(7 downto 0)
      );
   END COMPONENT;
   
begin

   Inst_sevenseg: sevenseg PORT MAP(
		num => ,
		anodes => ,
		cathodes => ,
		clk => div(12);
	);

end Behavioral;

