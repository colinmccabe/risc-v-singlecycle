library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu is
    Port ( x_in : in  STD_LOGIC_VECTOR (31 downto 0);
           y_in : in  STD_LOGIC_VECTOR (31 downto 0);
           opcode : in  STD_LOGIC_VECTOR(6 downto 0);
           funct3 : in  STD_LOGIC_VECTOR (2 downto 0);
           shamti : in STD_LOGIC_VECTOR(4 downto 0);
           funct7 : in  STD_LOGIC_VECTOR (6 downto 0);
           output : out STD_LOGIC_VECTOR(31 downto 0)
           );
end alu;

architecture Synthesizable of alu is

   signal x, y, output_signed : SIGNED(31 downto 0);
   signal true_val, bshift_out : STD_LOGIC_VECTOR(31 downto 0);
   signal left, logical : STD_LOGIC;
   signal shamt : STD_LOGIC_VECTOR(4 downto 0);

   COMPONENT bshift
      PORT(
         left : IN std_logic;
         logical : IN std_logic;
         shift : IN std_logic_vector(4 downto 0);
         input : IN std_logic_vector(31 downto 0);          
         output : OUT std_logic_vector(31 downto 0)
      );
   END COMPONENT;

begin
   x <= SIGNED(x_in);
   y <= SIGNED(y_in);
   
   true_val <= (0 => '1', others => '0');

   output_signed <=
      x - y       when funct3 = "000" and opcode(5) = '1' and funct7(5) = '1' else
      x + y       when funct3 = "000" or opcode = "0000011"     -- load
                                          or opcode = "0100011"  -- stor
                                          or opcode = "1100111" -- jalr
                                          else
      x xor y     when funct3 = "100" else
      x or y      when funct3 = "110" else
      x and y;
             
   output <= bshift_out when (funct3 = "001" or funct3 = "101") and (opcode = "0010011" or opcode = "0110011") else 
             true_val    when funct3 = "010" and x < y else
             true_val    when funct3 = "011" and x_in < y_in else
             STD_LOGIC_VECTOR(output_signed);
   
   -- Bit shifter
   left <= not funct3(2);
   logical <= not funct7(5);
   
   shamt <= shamti when opcode = "0010011" else
            y_in(4 downto 0);
   
   Inst_bshift: bshift PORT MAP(
		left => left,
		logical => logical,
		shift => shamt,
		input => x_in,
		output => bshift_out
	);
             
end Synthesizable;
