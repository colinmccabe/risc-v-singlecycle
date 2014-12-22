library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity computer is
    Port ( clk : in  STD_LOGIC;
           seg : out  STD_LOGIC_VECTOR (7 downto 0);
           an : out  STD_LOGIC_VECTOR (3 downto 0));
end computer;

architecture Behavioral of computer is

   constant DATA_ADDR_WIDTH : integer := 13;

   signal div : STD_LOGIC_VECTOR(12 downto 0) := (others => '0');
   signal peek : STD_LOGIC_VECTOR(15 downto 0);
   -- Wishbone
   signal wb_adr : STD_LOGIC_VECTOR(10 downto 0);
   signal wb_dat_write, wb_dat_read : STD_LOGIC_VECTOR(31 downto 0);
   signal wb_sel : STD_LOGIC_VECTOR(3 downto 0);
   signal wb_rst, wb_stb, wb_cyc, wb_tgd, wb_we, wb_ack : STD_LOGIC;

   COMPONENT cpu
      PORT(
         clk : IN std_logic;
         wb_dat_i : IN std_logic_vector(31 downto 0);
         wb_ack_i : IN std_logic;
         reg_peek : OUT std_logic_vector(15 downto 0);
         wb_rst_o : OUT std_logic;
         wb_adr_o : OUT std_logic_vector(10 downto 0);
         wb_dat_o : OUT std_logic_vector(31 downto 0);
         wb_sel_o : OUT std_logic_vector(3 downto 0);
         wb_tgd_o : OUT std_logic;
         wb_we_o : OUT std_logic;
         wb_stb_o : OUT std_logic;
         wb_cyc_o : OUT std_logic
      );
   END COMPONENT;
   
   COMPONENT data_mem_wb
      PORT(
         wb_clk_i : IN std_logic;
         wb_rst_i : IN std_logic;
         wb_adr_i : IN std_logic_vector(DATA_ADDR_WIDTH-3 downto 0);
         wb_dat_i : IN std_logic_vector(31 downto 0);
         wb_sel_i : in STD_LOGIC_VECTOR(3 downto 0);
         wb_tgd_i : in STD_LOGIC;
         wb_we_i : IN std_logic;
         wb_stb_i : IN std_logic;
         wb_cyc_i : IN std_logic;
         wb_dat_o : OUT std_logic_vector(31 downto 0);
         wb_ack_o : OUT std_logic
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
      reg_peek => peek,
      wb_rst_o => wb_rst,
      wb_adr_o => wb_adr,
      wb_dat_i => wb_dat_read,
      wb_dat_o => wb_dat_write,
      wb_sel_o => wb_sel,
      wb_tgd_o => wb_tgd,
      wb_we_o => wb_we,
      wb_stb_o => wb_stb,
      wb_cyc_o => wb_cyc,
      wb_ack_i => wb_ack
   );
   
   Inst_data_mem_wb: data_mem_wb PORT MAP(
      wb_clk_i => clk,
      wb_rst_i => wb_rst,
      wb_adr_i => wb_adr,
      wb_dat_i => wb_dat_write,
      wb_dat_o => wb_dat_read,
      wb_sel_i => wb_sel,
      wb_tgd_i => wb_tgd,
      wb_we_i => wb_we,
      wb_stb_i => wb_stb,
      wb_cyc_i => wb_cyc,
      wb_ack_o => wb_ack
   );

   Inst_sevenseg: sevenseg PORT MAP(
		num => peek,
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
