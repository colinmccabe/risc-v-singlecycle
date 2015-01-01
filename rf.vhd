library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity rf is
    Port ( clk : in STD_LOGIC;
           we : in STD_LOGIC;
           rd  : in  STD_LOGIC_VECTOR (4 downto 0);
           data_in : in STD_LOGIC_VECTOR(31 downto 0);
           rs1 : in  STD_LOGIC_VECTOR (4 downto 0);
           rs2 : in  STD_LOGIC_VECTOR (4 downto 0);
           s1 : out  STD_LOGIC_VECTOR (31 downto 0);
           s2 : out  STD_LOGIC_VECTOR (31 downto 0);
           reg_peek : out STD_LOGIC_VECTOR(15 downto 0)
          );
end rf;

architecture Synthesizable of rf is

   type reg_array is array(1 to 31) of STD_LOGIC_VECTOR(31 downto 0);
   signal reg_file : reg_array := (others => (others => '0'));
   
begin

   reg_peek <= reg_file(1)(15 downto 0);
   
   with rs1 select
      s1 <= (others => '0') when "00000",
            reg_file(1) when "00001",
            reg_file(2) when "00010",
            reg_file(3) when "00011",
            reg_file(4) when "00100",
            reg_file(5) when "00101",
            reg_file(6) when "00110",
            reg_file(7) when "00111",
            reg_file(8) when "01000",
            reg_file(9) when "01001",
            reg_file(10) when "01010",
            reg_file(11) when "01011",
            reg_file(12) when "01100",
            reg_file(13) when "01101",
            reg_file(14) when "01110",
            reg_file(15) when "01111",
            reg_file(16) when "10000",
            reg_file(17) when "10001",
            reg_file(18) when "10010",
            reg_file(19) when "10011",
            reg_file(20) when "10100",
            reg_file(21) when "10101",
            reg_file(22) when "10110",
            reg_file(23) when "10111",
            reg_file(24) when "11000",
            reg_file(25) when "11001",
            reg_file(26) when "11010",
            reg_file(27) when "11011",
            reg_file(28) when "11100",
            reg_file(29) when "11101",
            reg_file(30) when "11110",
            reg_file(31) when others;
            
   with rs2 select
      s2 <= (others => '0') when "00000",
            reg_file(1) when "00001",
            reg_file(2) when "00010",
            reg_file(3) when "00011",
            reg_file(4) when "00100",
            reg_file(5) when "00101",
            reg_file(6) when "00110",
            reg_file(7) when "00111",
            reg_file(8) when "01000",
            reg_file(9) when "01001",
            reg_file(10) when "01010",
            reg_file(11) when "01011",
            reg_file(12) when "01100",
            reg_file(13) when "01101",
            reg_file(14) when "01110",
            reg_file(15) when "01111",
            reg_file(16) when "10000",
            reg_file(17) when "10001",
            reg_file(18) when "10010",
            reg_file(19) when "10011",
            reg_file(20) when "10100",
            reg_file(21) when "10101",
            reg_file(22) when "10110",
            reg_file(23) when "10111",
            reg_file(24) when "11000",
            reg_file(25) when "11001",
            reg_file(26) when "11010",
            reg_file(27) when "11011",
            reg_file(28) when "11100",
            reg_file(29) when "11101",
            reg_file(30) when "11110",
            reg_file(31) when others;

   process(clk, we)
      begin
         if rising_edge(clk) and we = '1' then
            if rd = "00001" then
               reg_file(1) <= data_in;
            elsif rd = "00010" then
               reg_file(2) <= data_in;
            elsif rd = "00011" then
               reg_file(3) <= data_in;
            elsif rd = "00100" then
               reg_file(4) <= data_in;
            elsif rd = "00101" then
               reg_file(5) <= data_in;
            elsif rd = "00110" then
               reg_file(6) <= data_in;
            elsif rd = "00111" then
               reg_file(7) <= data_in;
            elsif rd = "01000" then
               reg_file(8) <= data_in;
            elsif rd = "01001" then
               reg_file(9) <= data_in;
            elsif rd = "01010" then
               reg_file(10) <= data_in;
            elsif rd = "01011" then
               reg_file(11) <= data_in;
            elsif rd = "01100" then
               reg_file(12) <= data_in;
            elsif rd = "01101" then
               reg_file(13) <= data_in;
            elsif rd = "01110" then
               reg_file(14) <= data_in;
            elsif rd =  "01111" then
               reg_file(15) <= data_in;
            elsif rd = "10000" then
               reg_file(16) <= data_in;
            elsif rd = "10001" then
               reg_file(17) <= data_in;
            elsif rd = "10010" then
               reg_file(18) <= data_in;
            elsif rd = "10011" then
               reg_file(19) <= data_in;
            elsif rd = "10100" then
               reg_file(20) <= data_in;
            elsif rd = "10101" then
               reg_file(21) <= data_in;
            elsif rd = "10110" then
               reg_file(22) <= data_in;
            elsif rd = "10111" then
               reg_file(23) <= data_in;
            elsif rd = "11000" then
               reg_file(24) <= data_in;
            elsif rd = "11001" then
               reg_file(25) <= data_in;
            elsif rd = "11010" then
               reg_file(26) <= data_in;
            elsif rd = "11011" then
               reg_file(27) <= data_in;
            elsif rd = "11100" then
               reg_file(28) <= data_in;
            elsif rd = "11101" then
               reg_file(29) <= data_in;
            elsif rd = "11110" then
               reg_file(30) <= data_in;
            elsif rd = "11111" then
               reg_file(31) <= data_in;
            end if;
         end if;
      end process;
      
end Synthesizable;
