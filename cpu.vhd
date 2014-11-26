library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cpu is
    Port ( clk : in  STD_LOGIC;
           reg_peek : out  STD_LOGIC_VECTOR (15 downto 0));
end cpu;

architecture Synthesizable of cpu is

   constant NOP : STD_LOGIC_VECTOR(31 downto 0) := x"000000" & "00010011";

   signal inst1, prog_mem_out : STD_LOGIC_VECTOR(31 downto 0);
   signal inst2 : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
   signal pc : STD_LOGIC_VECTOR(14 downto 0) := (others => '0');
   signal jmp_or_branch_addr, data_addr : STD_LOGIC_VECTOR(14 downto 0);
   
   signal rf_out_x, rf_out_y, rf_data_in : STD_LOGIC_VECTOR(31 downto 0);
   signal x, y : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
   signal x_next, y_next : STD_LOGIC_VECTOR(31 downto 0);

   signal alu_out, load_data_word, load_data, stor_data,
          stor_byte, stor_hw : STD_LOGIC_VECTOR(31 downto 0);

   signal stor_data_reg : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
   signal load_hw, hw_to_stor : STD_LOGIC_VECTOR(15 downto 0);
   signal load_byte, byte_to_stor : STD_LOGIC_VECTOR(7 downto 0);
   
   signal rs1, rs2, rd : STD_LOGIC_VECTOR(4 downto 0);
   
   signal opcode, opcode1 : STD_LOGIC_VECTOR(6 downto 0);
   signal shamt : STD_LOGIC_VECTOR(4 downto 0);
   signal funct3, funct3_1 : STD_LOGIC_VECTOR(2 downto 0);
   signal funct7 : STD_LOGIC_VECTOR(6 downto 0);
   
   signal lui, jal, jalr, branch, load, comp_imm, comp_reg, memstall_1st_cycle,
            prog_mem_en, auipc, stor, storing_word, comparison_true, eq : STD_LOGIC;
            
   signal load1, jalr1, comp_imm1, stor1 : STD_LOGIC;

   signal data_mem_we : STD_LOGIC_VECTOR(0 downto 0);
   signal I_type_signed, S_type : STD_LOGIC;
   signal do_jump : STD_LOGIC;
   
   signal memstall_2nd_cycle, stall_l : STD_LOGIC := '0';
   
   signal I_imm, S_imm : STD_LOGIC_VECTOR(11 downto 0);
   signal SB_imm : STD_LOGIC_VECTOR(12 downto 0);
   signal U_imm : STD_LOGIC_VECTOR(31 downto 0);
   signal UJ_imm : STD_LOGIC_VECTOR(20 downto 0);
   
   signal alu_output_true, rf_we : STD_LOGIC;

   COMPONENT prog_mem
      PORT (
         clka : IN STD_LOGIC;
         ena : IN STD_LOGIC;
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
         out_x : OUT std_logic_vector(31 downto 0);
         out_y : OUT std_logic_vector(31 downto 0);
         reg_peek : out std_logic_vector(15 downto 0)
      );
   END COMPONENT;

   COMPONENT alu
      PORT(
         x_in : IN std_logic_vector(31 downto 0);
         y_in : IN std_logic_vector(31 downto 0);
         opcode : IN std_logic_vector(6 downto 0);
         funct3 : IN std_logic_vector(2 downto 0);
         shamti : IN std_logic_vector(4 downto 0);
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
   -- Control signals
   
   inst1 <= NOP when do_jump = '1' else
            prog_mem_out;
   
   opcode <= inst2(6 downto 0);
   opcode1 <= inst1(6 downto 0);

   rs1 <= inst1(19 downto 15);
   rs2 <= inst1(24 downto 20);
   rd <= inst2(11 downto 7);

   shamt <= inst2(24 downto 20);
   
   funct3 <= inst2(14 downto 12);
   funct3_1 <= inst1(14 downto 12);
   funct7 <= inst2(31 downto 25);
   
   lui <=      BOOL_TO_SL(opcode = "0110111");
   auipc <=    BOOL_TO_SL(opcode = "0010111");
   jal <=      BOOL_TO_SL(opcode = "1101111");
   jalr <=     BOOL_TO_SL(opcode = "1100111");
   branch <=   BOOL_TO_SL(opcode = "1100011");
   stor <=     BOOL_TO_SL(opcode = "0100011");
   load <=     BOOL_TO_SL(opcode = "0000011");
   comp_imm <= BOOL_TO_SL(opcode = "0010011");
   comp_reg <= BOOL_TO_SL(opcode = "0110011");

   -- 1st stage control signals, used to determine operands
   load1 <= BOOL_TO_SL(opcode1 = "0000011");
   jalr1 <= BOOL_TO_SL(opcode1 = "1100111");
   comp_imm1 <= BOOL_TO_SL(opcode1 = "0010011");

   storing_word <= stor and BOOL_TO_SL(funct3 = "010");
   
   I_type_signed <= load1
                     or jalr1
                     or (comp_imm1 and BOOL_TO_SL(funct3_1 = "000" or funct3_1 = "010"));
   S_type <= stor1;


   -- Immediates
   I_imm <= inst1(31 downto 20);
   S_imm <= inst1(31 downto 25) & inst1(11 downto 7);
   SB_imm <= inst1(31) & inst1(7) & inst1(30 downto 25) & inst1(11 downto 8) & "0";
   U_imm <= inst2(31 downto 12) & x"000";
   UJ_imm <= inst1(31) & inst2(19 downto 12) & inst1(20) & inst1(30 downto 21) & "0";


   -- ALU operands, are latched into x and y regs between stages 1 and 2
   x_next <= rf_out_x;
   
   y_next <= STD_LOGIC_VECTOR(resize(SIGNED(I_imm), 32))   when I_type_signed = '1'  else
        STD_LOGIC_VECTOR(resize(UNSIGNED(I_imm), 32)) when comp_imm1 = '1'  else
        STD_LOGIC_VECTOR(resize(SIGNED(S_imm), 32))   when S_type = '1'  else
        rf_out_y;


   -- Register file   
   rf_data_in <= STD_LOGIC_VECTOR(resize(UNSIGNED(pc), 32))
                     when jal = '1' or jalr = '1' else
                 U_imm
                     when lui = '1'   else
                 load_data
                     when load = '1' else
                 STD_LOGIC_VECTOR(resize(UNSIGNED(SIGNED(pc) + resize(SIGNED(U_imm), pc'length)), 32))
                     when auipc = '1' else                                         
                 alu_out;
                 
   rf_we <= stall_l and ((load and memstall_2nd_cycle)
                           or comp_imm or comp_reg
                           or jal
                           or jalr
                           or lui
                           or auipc);


   -- PC calculation
   eq <= BOOL_TO_SL(x = y);
   
   comparison_true <= BOOL_TO_SL((eq = '1' and funct3 = "000") -- beq
                                   or
                                 (eq = '0' and funct3 = "001") -- bne
                                   or
                                 (alu_out(0) = '1' and (funct3 = "100" or funct3 = "110")) -- blt/bltu
                                   or
                                 ((alu_out(0) = '0' or eq = '1') and (funct3 = "101" or funct3 = "111")) --bge/bgeu
                                );
   do_jump <= jal or jalr or (branch and comparison_true);
   
   jmp_or_branch_addr <= STD_LOGIC_VECTOR(SIGNED(pc) + resize(SIGNED(SB_imm), pc'length) - 4)
                           when branch = '1' else
                         STD_LOGIC_VECTOR(SIGNED(pc) + resize(SIGNED(UJ_imm), pc'length) - 4)
                           when jal = '1' else
                         alu_out(14 downto 0); -- jalr

   -- Memory stall
   memstall_1st_cycle <= (load or (stor and (not storing_word))) and (not memstall_2nd_cycle) and stall_l;

   -- Memory signals
   data_mem_we(0) <= ((stor and memstall_2nd_cycle) or storing_word) and stall_l;
   prog_mem_en <= not memstall_1st_cycle; -- Don't fetch inst on 1st cycle of memstall
   data_addr <= alu_out(14 downto 0);
   byte_to_stor <= stor_data_reg(7 downto 0);
   hw_to_stor <= stor_data_reg(15 downto 0);

   -- Memory load
   with funct3 select
      load_data <= STD_LOGIC_VECTOR(resize(SIGNED(load_byte), 32)) when "000",
                   STD_LOGIC_VECTOR(resize(SIGNED(load_hw), 32)) when "001",
                   STD_LOGIC_VECTOR(resize(UNSIGNED(load_byte), 32)) when "100",
                   STD_LOGIC_VECTOR(resize(UNSIGNED(load_hw), 32)) when "101",
                   load_data_word when others;

   with data_addr(1 downto 0) select
      load_byte <= load_data_word(7 downto 0) when "00",
                   load_data_word(15 downto 8) when "01",
                   load_data_word(23 downto 16) when "10",
                   load_data_word(31 downto 24) when others;

   with data_addr(1) select
      load_hw <= load_data_word(15 downto 0) when '0',
                 load_data_word(31 downto 16) when others;

   -- Memory stor
   with funct3 select
      stor_data <= stor_byte when "000",
                   stor_hw when "001",
                   stor_data_reg when others; -- stor entire reg

   with data_addr(1 downto 0) select
      stor_byte <= load_data_word(31 downto 8) & byte_to_stor when "00",
                   load_data_word(31 downto 16) & byte_to_stor & load_data_word(7 downto 0) when "01",
                   load_data_word(31 downto 24) & byte_to_stor & load_data_word(15 downto 0) when "10",
                   byte_to_stor & load_data_word(23 downto 0) when others;

   with data_addr(1) select
      stor_hw <= load_data_word(31 downto 16) & hw_to_stor when '0',
                 hw_to_stor & load_data_word(15 downto 0) when others;


   -- Modules
   Inst_prog_mem : prog_mem PORT MAP (
      clka => clk,
      ena => prog_mem_en,
      addra => pc(14 downto 2),
      douta => prog_mem_out
   );
  
   Inst_data_mem : data_mem PORT MAP (
      clka => clk,
      wea => data_mem_we,
      addra => data_addr(14 downto 2),
      dina => stor_data,
      douta => load_data_word
   );
   
   Inst_rf: rf PORT MAP(
		clk => clk,
		we => rf_we,
		rd => rd,
		data_in => rf_data_in,
		rs1 => rs1,
		rs2 => rs2,
		out_x => rf_out_x,
		out_y => rf_out_y,
      reg_peek => reg_peek
	);
   
   Inst_alu: alu PORT MAP(
		x_in => x,
		y_in => y,
		opcode => opcode,
		funct3 => funct3,
      shamti => shamt,
		funct7 => funct7,
		output => alu_out
	);
   
   process(clk)
      begin
         if rising_edge(clk) then
            if do_jump = '1' and stall_l = '1' then
               stall_l <= '0';

               inst2 <= inst1;

               pc <= jmp_or_branch_addr;
            elsif memstall_1st_cycle = '1' then
               memstall_2nd_cycle <= '1';
            else
               inst2 <= inst1;
               stor_data_reg <= rf_out_y;

               pc <= STD_LOGIC_VECTOR(UNSIGNED(pc) + 4);

               stall_l <= '1';

               x <= x_next;
               y <= y_next;

               memstall_2nd_cycle <= '0';
            end if;
         end if;
      end process;

end Synthesizable;
