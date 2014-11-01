library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity cpu is
    Port ( clk : in  STD_LOGIC;
           reg_peek : out  STD_LOGIC_VECTOR (16 downto 0));
end cpu;

architecture Synthesizable of cpu is

   signal instr : STD_LOGIC_VECTOR(31 downto 0);
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
   stor <= BOOL_TO_SL(opcode = "0000011");
   load <= BOOL_TO_SL(opcode = "0100011");
   
   
   
   jumping <= jump or (branch and alu_out(0));
   
   rf_we <= (load and (not load_2nd_cycle))
            or (not stall_l)
            or jumping;
   
   rs1 <= instr(19 downto 15);
   rs2 <= instr(24 downto 20);
   
   x <= s1;
   
   y <= instr(31 downto 25) & instr(11 downto 7) when S_type_instr = '1'  else
        instr(31 downto 20)                      when I_type_instr = '1'  else
                                                 when SB_type_instr = '1' else
        s2;
        
   rf_data_in <= pc when jal = '1' else
                 data_mem_out when load = '1' else
                 alu_out;
        
   jmp_or_branch_addr <= 

   Inst_prog_mem : prog_mem PORT MAP (
      clka => clk,
      addra => pc,
      douta => instr
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
