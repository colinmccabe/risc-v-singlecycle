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
   signal bshift_out : STD_LOGIC_VECTOR(31 downto 0);
   signal left, logical, comp_instr : STD_LOGIC;
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
   
   function BOOL_TO_SL(X : boolean) return std_ulogic is
      begin
         if X then
            return '1';
         else
            return '0';
         end if;
      end BOOL_TO_SL;

begin
   -- Signed operations
   x <= SIGNED(x_in);
   y <= SIGNED(y_in);

   output_signed <=
      x - y       when funct3 = "000" and opcode = "0110011" and funct7(5) = '1' else
      x + y;
     
     
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
   
   
   -- Main ALU output mux
   comp_instr <= BOOL_TO_SL(opcode = "0010011" or opcode = "0110011");

   output <=
      bshift_out
         when comp_instr = '1' and (funct3 = "001" or funct3 = "101") -- shifts

      else (0 => BOOL_TO_SL(x_in < y_in), others => '0')
         when (comp_instr = '1' and funct3 = "011") -- sltu, sltiu
                     or (opcode = "1100011" and (funct3 = "110" or funct3 = "111")) -- bltu, bgeu

      else (0 => BOOL_TO_SL(x < y), others => '0')
         when (comp_instr = '1' and funct3 = "010") -- slt, slti
                  or (opcode = "1100011") -- blt, bge

      else (x_in xor y_in)
         when comp_instr = '1' and funct3 = "100"

      else (x_in or y_in)
         when comp_instr = '1' and funct3 = "110"

      else (x_in and y_in)
         when comp_instr = '1' and funct3 = "111"

      else STD_LOGIC_VECTOR(output_signed);
             
end Synthesizable;
