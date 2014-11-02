library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity cpu is
    Port ( clk : in  STD_LOGIC;
           reg_peek : out  STD_LOGIC_VECTOR (16 downto 0));
end cpu;

architecture Synthesizable of cpu is

   signal inst : STD_LOGIC_VECTOR(31 downto 0);
   signal pc : STD_LOGIC_VECTOR(12 downto 0);
   
   signal s1, s2 : STD_LOGIC_VECTOR(31 downto 0);
   
   signal rs1, rs2, rd : STD_LOGIC_VECTOR(4 downto 0);

   COMPONENT prog_mem
      PORT (
         clka : IN STD_LOGIC;
         addra : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
         douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
      );
   END COMPONENT;

   COMPONENT data_mem
      PORT (
         clka : IN STD_LOGIC;
         wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         addra : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
         dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
         douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
      );
   END COMPONENT;
   
   COMPONENT rf
      PORT(
         clk : IN std_logic;
         we : IN std_logic;
         rd : IN std_logic_vector(4 downto 0);
         data_in : IN std_logic_vector(31 downto 0);
         rs1 : IN std_logic_vector(4 downto 0);
         rs2 : IN std_logic_vector(4 downto 0);          
         s1 : OUT std_logic_vector(31 downto 0);
         s2 : OUT std_logic_vector(31 downto 0)
      );
   END COMPONENT;

   COMPONENT alu
      PORT(
         x_in : IN std_logic_vector(31 downto 0);
         y_in : IN std_logic_vector(31 downto 0);
         opcode5 : IN std_logic;
         funct3 : IN std_logic_vector(2 downto 0);
         funct7 : IN std_logic_vector(6 downto 0);          
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
   -- Instruction-derived control signals
   opcode <= inst(6 downto 0);
   
   rs1 <= inst(19 downto 15);
   rs2 <= inst(24 downto 20);
   rd <= inst(11 downto 7);
   
   funct3 <= inst(14 downto 12);
   funct7 <= inst(31 downto 25);
   
   lui <= BOOL_TO_SL(opcode = "0110111");
   jal <= BOOL_TO_SL(opcode = "1101111");
   jalr <= BOOL_TO_SL(opcode = "1100111");
   branch <= BOOL_TO_SL(opcode = "1100011");
   stor <= BOOL_TO_SL(opcode = "0000011");
   load <= BOOL_TO_SL(opcode = "0100011");
   arith_imm <= BOOL_TO_SL(opcode = "0010011");
   arith_reg <= BOOL_TO_SL(opcode = "0110011");
   
   I_type <= arith_imm;
   
   
   -- Immediates
   I_imm <= inst(31 downto 20);
   S_imm <= inst(31 downto 25) & inst(11 downto 7);
   SB_imm <= inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8);
   U_imm <= inst(31 downto 12);
   UJ_imm <= inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21);

   -- ALU operand selection
   x <= s1;
   
   y <= I_imm  when I_type = '1'  else
        S_imm  when S_type = '1' else
        SB_imm when SB_type = '1'  else
        s2;
        
   alu_output_true <= alu_out(0);


   -- Register file   
   rf_data_in <= x"00000" & pc when jal = '1' else
                 U_imm & x"000" when lui = 1' else
                 data_mem_out when load = '1' else
                 alu_out;
                 
   rf_we <= (load and load_2nd_cycle)
               or arith
               or jal
               or jalr
               or lui;


   -- PC calculation
   do_jump <= jump or (branch and alu_output_true);
   jmp_or_branch_addr <= SIGNED(pc) + SIGNED(SB_imm) when branch = '1' else
                         

   Inst_prog_mem : prog_mem PORT MAP (
      clka => clk,
      addra => pc,
      douta => inst
   );
  
   Inst_data_mem : data_mem PORT MAP (
      clka => clk,
      wea => stor,
      addra => alu_out,
      dina => s2,
      douta => data_mem_out
   );
   
   Inst_rf: rf PORT MAP(
		clk => clk,
		we => rf_we,
		rd => rd,
		data_in => rf_data_in,
		rs1 => rs1,
		rs2 => rs2,
		s1 => s1,
		s2 => s2
	);
   
   Inst_alu: alu PORT MAP(
		x_in => x,
		y_in => y,
		opcode => opcode,
		funct3 => funct3,
		funct7 => funct7,
		output => alu_out
	);
   
   process(clk)
      begin
         if rising_edge(clk) then
            if jump_or_branch = '1' and stall_l = '1' then
               stall_l <= '0';
               pc <= jmp_or_branch_addr;
            elsif load = '1' and load_2nd_cycle = '0' then
               load_2nd_cycle <= '1';
            else
               pc <= pc + 1;
               stall_l <= '1';
               load_2nd_cycle <= '0';
            end if;
         end if;
      end process;

end Synthesizable;
