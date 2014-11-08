library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cpu is
    Port ( clk : in  STD_LOGIC;
           reg_peek : out  STD_LOGIC_VECTOR (15 downto 0));
end cpu;

architecture Synthesizable of cpu is

   signal inst : STD_LOGIC_VECTOR(31 downto 0);
   signal pc : STD_LOGIC_VECTOR(12 downto 0) := (others => '0');
   signal jmp_or_branch_addr : STD_LOGIC_VECTOR(12 downto 0);
   
   signal s1, s2 : STD_LOGIC_VECTOR(31 downto 0);
   signal x, y : STD_LOGIC_VECTOR(31 downto 0);
   signal rf_data_in, alu_out, data_mem_out : STD_LOGIC_VECTOR(31 downto 0);
   
   signal rs1, rs2, rd : STD_LOGIC_VECTOR(4 downto 0);
   
   signal opcode : STD_LOGIC_VECTOR(6 downto 0);
   signal shamt : STD_LOGIC_VECTOR(4 downto 0);
   signal funct3 : STD_LOGIC_VECTOR(2 downto 0);
   signal funct7 : STD_LOGIC_VECTOR(6 downto 0);
   
   signal lui, jal, jalr, branch, load, arith_imm, arith_reg : STD_LOGIC;
   signal stor : STD_LOGIC_VECTOR(0 downto 0);
   signal I_type, S_type, SB_type : STD_LOGIC;
   signal do_jump : STD_LOGIC;
   
   signal load_2nd_cycle, stall_l : STD_LOGIC := '0';
   
   signal I_imm, S_imm : STD_LOGIC_VECTOR(11 downto 0);
   signal SB_imm : STD_LOGIC_VECTOR(11 downto 0);
   signal U_imm : STD_LOGIC_VECTOR(19 downto 0);
   signal UJ_imm : STD_LOGIC_VECTOR(19 downto 0);
   
   signal alu_output_true, rf_we : STD_LOGIC;

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
         s2 : OUT std_logic_vector(31 downto 0);
         reg_peek : out std_logic_vector(15 downto 0)
      );
   END COMPONENT;

   COMPONENT alu
      PORT(
         x_in : IN std_logic_vector(31 downto 0);
         y_in : IN std_logic_vector(31 downto 0);
         opcode : IN std_logic_vector(6 downto 0);
         funct3 : IN std_logic_vector(2 downto 0);
         shamt : IN std_logic_vector(4 downto 0);
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
   
   shamt <= inst(24 downto 20);
   
   funct3 <= inst(14 downto 12);
   funct7 <= inst(31 downto 25);
   
   lui <=       BOOL_TO_SL(opcode = "0110111");
   jal <=       BOOL_TO_SL(opcode = "1101111");
   jalr <=      BOOL_TO_SL(opcode = "1100111");
   branch <=    BOOL_TO_SL(opcode = "1100011");
   stor(0) <=   BOOL_TO_SL(opcode = "0000011");
   load <=      BOOL_TO_SL(opcode = "0100011");
   arith_imm <= BOOL_TO_SL(opcode = "0010011");
   arith_reg <= BOOL_TO_SL(opcode = "0110011");
   
   I_type <= arith_imm;
   S_type <= load;
   SB_type <= branch;
   
   -- Immediates
   I_imm <= inst(31 downto 20);
   S_imm <= inst(31 downto 25) & inst(11 downto 7);
   SB_imm <= inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8);
   U_imm <= inst(31 downto 12);
   UJ_imm <= inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21);

   -- ALU operand selection
   x <= s1;
   
   y <= STD_LOGIC_VECTOR(resize(SIGNED(I_imm), 32))  when I_type = '1'  else
        STD_LOGIC_VECTOR(resize(SIGNED(S_imm), 32))  when S_type = '1' else
        STD_LOGIC_VECTOR(resize(SIGNED(SB_imm), 32)) when SB_type = '1'  else
        s2;
        
   alu_output_true <= alu_out(0);


   -- Register file   
   rf_data_in <= STD_LOGIC_VECTOR(resize(UNSIGNED(pc), rf_data_in'length)) when jal = '1'  else
                 U_imm & x"000"                                            when lui = '1'   else
                 data_mem_out                                              when load = '1' else
                 alu_out;
                 
   rf_we <= (load and load_2nd_cycle)
               or arith_imm or arith_reg
               or jal
               or jalr
               or lui;


   -- PC calculation
   do_jump <= jal or jalr or (branch and alu_output_true);
   jmp_or_branch_addr <= STD_LOGIC_VECTOR(SIGNED(pc) + resize(SIGNED(SB_imm(11 downto 1)), pc'length))
                           when branch = '1' else
                         STD_LOGIC_VECTOR(signed(pc) + resize(SIGNED(UJ_imm(19 downto 1)), pc'length))
                           when jal = '1' else
                         STD_LOGIC_VECTOR(resize(SIGNED(s1), pc'length) + resize(SIGNED(I_imm(11 downto 2)), pc'length));

   Inst_prog_mem : prog_mem PORT MAP (
      clka => clk,
      addra => pc,
      douta => inst
   );
  
   Inst_data_mem : data_mem PORT MAP (
      clka => clk,
      wea => stor,
      addra => alu_out(12 downto 0),
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
		s2 => s2,
      reg_peek => reg_peek
	);
   
   Inst_alu: alu PORT MAP(
		x_in => x,
		y_in => y,
		opcode => opcode,
		funct3 => funct3,
      shamt => shamt,
		funct7 => funct7,
		output => alu_out
	);
   
   process(clk)
      begin
         if rising_edge(clk) then
            if do_jump = '1' and stall_l = '1' then
               stall_l <= '0';
               pc <= jmp_or_branch_addr;
            elsif load = '1' and load_2nd_cycle = '0' then
               load_2nd_cycle <= '1';
            else
               pc <= STD_LOGIC_VECTOR(UNSIGNED(pc) + 1);
               stall_l <= '1';
               load_2nd_cycle <= '0';
            end if;
         end if;
      end process;

end Synthesizable;
