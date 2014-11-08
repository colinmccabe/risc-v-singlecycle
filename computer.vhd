library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity computer is
    Port ( clk : in  STD_LOGIC;
           seg : out  STD_LOGIC_VECTOR (7 downto 0);
           an : out  STD_LOGIC_VECTOR (3 downto 0));
end computer;

architecture Behavioral of computer is

   signal div : STD_LOGIC_VECTOR(12 downto 0) := (others => '0');
   signal seg_num : STD_LOGIC_VECTOR(15 downto 0);
   
   COMPONENT cpu
      PORT(
         clk : IN std_logic;          
         reg_peek : OUT std_logic_vector(15 downto 0)
      );
   END COMPONENT;

   COMPONENT sevenseg
      PORT(
         num : IN std_logic_vector(15 downto 0);
         clk : IN std_logic;          
         anodes : OUT std_logic_vector(3 downto 0);
         cathodes : OUT std_logic_vector(7 downto 0)
      );
   END COMPONENT;
   
begin

   Inst_cpu: cpu PORT MAP(
      clk => clk,
      reg_peek => seg_num
   );

   Inst_sevenseg: sevenseg PORT MAP(
		num => seg_num,
		anodes => an,
		cathodes => seg,
		clk => div(12)
	);
   
   process(clk)
      begin
         if rising_edge(clk) then
            div <= STD_LOGIC_VECTOR(UNSIGNED(div) + 1);
         end if;
      end process;

end Behavioral;
