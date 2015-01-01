library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cpu is
    Port ( clk : in  STD_LOGIC;
           reg_peek : out  STD_LOGIC_VECTOR (15 downto 0);
           -- Wishbone bus
           wb_rst_o : out  STD_LOGIC;
           wb_adr_o : out  STD_LOGIC_VECTOR (11 downto 0);
           wb_dat_i : in  STD_LOGIC_VECTOR (31 downto 0);
           wb_dat_o : out  STD_LOGIC_VECTOR (31 downto 0);
           wb_sel_o : out STD_LOGIC_VECTOR(3 downto 0);
           wb_tgd_o : out STD_LOGIC;
           wb_we_o : out  STD_LOGIC;
           wb_stb_o : out  STD_LOGIC;
           wb_cyc_o : out  STD_LOGIC;
           wb_ack_i : in  STD_LOGIC;
           -- Progmem
           prog_mem_en : out STD_LOGIC;
           prog_mem_addr : out STD_LOGIC_VECTOR(11 downto 0);
           prog_mem_inst : in STD_LOGIC_VECTOR(31 downto 0));
end cpu;

architecture Synthesizable of cpu is

   constant NOP : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
   constant INSTR_ADDR_WIDTH : integer := 14;
   constant DATA_ADDR_WIDTH : integer := 14;

   signal inst : STD_LOGIC_VECTOR(31 downto 0);
   signal pc : STD_LOGIC_VECTOR(INSTR_ADDR_WIDTH-1 downto 0) := (others => '0');
   signal jmp_or_branch_addr, progmem_addr : STD_LOGIC_VECTOR(INSTR_ADDR_WIDTH-1 downto 0);
   signal data_mem_addr : STD_LOGIC_VECTOR(DATA_ADDR_WIDTH-3 downto 0);
   
   signal s1, s2 : STD_LOGIC_VECTOR(31 downto 0);
   signal x, y : STD_LOGIC_VECTOR(31 downto 0);
   signal rf_data_in, alu_out, load_data, stor_src_reg : STD_LOGIC_VECTOR(31 downto 0);
   
   signal rs1, rs2, rd : STD_LOGIC_VECTOR(4 downto 0);
   
   signal opcode : STD_LOGIC_VECTOR(6 downto 0);
   signal shamt : STD_LOGIC_VECTOR(4 downto 0);
   signal funct3 : STD_LOGIC_VECTOR(2 downto 0);
   signal funct7 : STD_LOGIC_VECTOR(6 downto 0);
   
   signal lui, jal, jalr, branch, load, comp_imm, comp_reg, stall,
            auipc, stor, comparison_true, eq : STD_LOGIC;
   signal I_type_signed, S_type, SB_type : STD_LOGIC;
   signal do_jump : STD_LOGIC;
   
   signal mem_op, mem_done, mem_load_unsigned : STD_LOGIC;
   signal mem_sel, byte_sel, hw_sel : STD_LOGIC_VECTOR(3 downto 0);
   
   signal I_imm, S_imm : STD_LOGIC_VECTOR(11 downto 0);
   signal SB_imm : STD_LOGIC_VECTOR(12 downto 0);
   signal U_imm : STD_LOGIC_VECTOR(31 downto 0);
   signal UJ_imm : STD_LOGIC_VECTOR(20 downto 0);
   
   signal rf_we : STD_LOGIC;
   
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
   inst <= prog_mem_inst;
   -- Control signals
   opcode <= inst(6 downto 0);
   
   rs1 <= inst(19 downto 15);
   rs2 <= inst(24 downto 20);
   rd <= inst(11 downto 7);
   
   shamt <= inst(24 downto 20);
   
   funct3 <= inst(14 downto 12);
   funct7 <= inst(31 downto 25);
   
   lui <=      BOOL_TO_SL(opcode = "0110111");
   auipc <=    BOOL_TO_SL(opcode = "0010111");
   jal <=      BOOL_TO_SL(opcode = "1101111");
   jalr <=     BOOL_TO_SL(opcode = "1100111");
   branch <=   BOOL_TO_SL(opcode = "1100011");
   stor <=     BOOL_TO_SL(opcode = "0100011");
   load <=     BOOL_TO_SL(opcode = "0000011");
   comp_imm <= BOOL_TO_SL(opcode = "0010011");
   comp_reg <= BOOL_TO_SL(opcode = "0110011");
   
   I_type_signed <= load
                     or jalr
                     or (comp_imm and BOOL_TO_SL(funct3 = "000" or funct3 = "010"));
   S_type <= stor;
   SB_type <= branch;


   -- Immediates
   I_imm <= inst(31 downto 20);
   S_imm <= inst(31 downto 25) & inst(11 downto 7);
   SB_imm <= inst(31) & inst(7) & inst(30 downto 25) & inst(11 downto 8) & "0";
   U_imm <= inst(31 downto 12) & x"000";
   UJ_imm <= inst(31) & inst(19 downto 12) & inst(20) & inst(30 downto 21) & "0";


   -- ALU operand selection
   x <= s1;
   
   y <= STD_LOGIC_VECTOR(resize(SIGNED(I_imm), 32))   when I_type_signed = '1'  else
        STD_LOGIC_VECTOR(resize(UNSIGNED(I_imm), 32)) when comp_imm = '1'  else
        STD_LOGIC_VECTOR(resize(SIGNED(S_imm), 32))   when S_type = '1'  else
        s2;


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
                 
   rf_we <= (load and (not stall))
               or comp_imm or comp_reg
               or jal
               or jalr
               or lui
               or auipc;


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
                         alu_out(INSTR_ADDR_WIDTH-1 downto 0); -- jalr

   prog_mem_addr <= jmp_or_branch_addr(13 downto 2) when do_jump = '1' else
                    pc(13 downto 2);

   -- Memory stall
   mem_op <= load or stor;

   -- Memory signals
   prog_mem_en <= not stall; -- Don't fetch inst on 1st cycle of stall
   data_mem_addr <= alu_out(DATA_ADDR_WIDTH-1 downto 2);
   stor_src_reg <= s2;
   mem_load_unsigned <= BOOL_TO_SL(funct3 = "100" or funct3 = "101");
   
   wb_rst_o <= '0';
   wb_adr_o <= data_mem_addr;
   wb_dat_o <= stor_src_reg;
   wb_tgd_o <= mem_load_unsigned;
   wb_sel_o <= mem_sel;
   wb_we_o <= stor;
   wb_stb_o <= mem_op;
   wb_cyc_o <= mem_op;
   load_data <= wb_dat_i;
   mem_done <= wb_ack_i;
   
   stall <= mem_op and (not mem_done);
   
   with funct3 select
      mem_sel <=
         byte_sel when "000",
         byte_sel when "100",
         hw_sel when "001",
         hw_sel when "101",
         "1111" when others;
         
   with alu_out(1 downto 0) select
      byte_sel <=
         "0001" when "00",
         "0010" when "01",
         "0100" when "10",
         "1000" when others;
         
   with alu_out(1) select
      hw_sel <=
         "0011" when '0',
         "1100" when others;
   
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
      shamti => shamt,
		funct7 => funct7,
		output => alu_out
	);
   
   process(clk)
      begin
         if rising_edge(clk) then
            if do_jump = '1' then
               -- Set pc to 2nd inst after jump - 1st is already being fetched
               pc <= STD_LOGIC_VECTOR(UNSIGNED(jmp_or_branch_addr) + 4);
            elsif stall = '1' then
               -- Do nothing
            else
               pc <= STD_LOGIC_VECTOR(UNSIGNED(pc) + 4);
            end if;
         end if;
      end process;

end Synthesizable;
