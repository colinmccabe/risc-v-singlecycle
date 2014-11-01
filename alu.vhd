library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu is
    Port ( x_in : in  STD_LOGIC_VECTOR (31 downto 0);
           y_vin : in  STD_LOGIC_VECTOR (31 downto 0);
           opcode5 : in  STD_LOGIC;
           funct3 : in  STD_LOGIC_VECTOR (2 downto 0);
           funct7 : in  STD_LOGIC_VECTOR (6 downto 0);
           output : out STD_LOGIC_VECTOR(31 downto 0)
           );
end alu;

architecture Synthesizable of alu is

   signal x, y, output_signed : SIGNED(31 downto 0);
   signal true_val : SIGNED(31 downto 0);

begin
   x <= SIGNED(x_in);
   y <= SIGNED(y_in);
   
   true_val <= (0 => '1', others => '0');

   output_signed <=
      x - y    when funct3 = "000" and opcode5 = '1' and funct7(5) = '1' else
      x + y    when funct3 = "000" else
      true_val when funct3 = "010" and x < y else
      true_val when funct3 = "011" and x_vec < y_vec else
      x xor y  when funct3 = "100" else
      x or y   when funct3 = "110" else
      x and y;
             
   output <= STD_LOGIC_VECTOR(output_signed);
             
end Synthesizable;
