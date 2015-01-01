library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity data_mem_wb is
    Port ( wb_clk_i : in  STD_LOGIC;
           wb_rst_i : in  STD_LOGIC;
           wb_adr_i : in  STD_LOGIC_VECTOR (11 downto 0);
           wb_dat_i : in  STD_LOGIC_VECTOR (31 downto 0);
           wb_dat_o : out  STD_LOGIC_VECTOR (31 downto 0);
           wb_sel_i : in STD_LOGIC_VECTOR(3 downto 0);
           wb_tgd_i : in STD_LOGIC;
           wb_we_i : in  STD_LOGIC;
           wb_stb_i : in  STD_LOGIC;
           wb_cyc_i : in  STD_LOGIC;
           wb_ack_o : out  STD_LOGIC;
           -- Progmem
           prog_mem_en : in STD_LOGIC;
           prog_mem_addr : in STD_LOGIC_VECTOR(11 downto 0);
           prog_mem_inst : out STD_LOGIC_VECTOR(31 downto 0));
end data_mem_wb;

architecture Behavioral of data_mem_wb is
   
   type STATE_TYPE is (CYCLE1, CYCLE2);
   
   signal state : STATE_TYPE := CYCLE1;
   signal byte_to_stor, byte_to_load : STD_LOGIC_VECTOR(7 downto 0);
   signal hw_to_stor, hw_to_load : STD_LOGIC_VECTOR(15 downto 0);
   signal stor_data, byte_stor_data, hw_stor_data, blk_ram_out :
      STD_LOGIC_VECTOR(31 downto 0);
   signal xfer_size : STD_LOGIC_VECTOR(1 downto 0);
   signal blk_ram_we : STD_LOGIC_VECTOR(0 downto 0);

   COMPONENT data_mem
      PORT (
         clka : IN STD_LOGIC;
         wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
         dina : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
         douta : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
         clkb : IN STD_LOGIC;
         enb : IN STD_LOGIC;
         web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         addrb : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
         dinb : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
         doutb : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
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

   wb_ack_o <= BOOL_TO_SL(state = CYCLE2);

   xfer_size <=
      STD_LOGIC_VECTOR(
         UNSIGNED("0" & wb_sel_i(3 downto 3))
         + UNSIGNED("0" & wb_sel_i(2 downto 2))
         + UNSIGNED("0" & wb_sel_i(1 downto 1))
         + UNSIGNED("0" & wb_sel_i(0 downto 0))
       );
                  
   blk_ram_we(0) <= BOOL_TO_SL(wb_we_i = '1' and state = CYCLE2);
 
   byte_to_stor <= wb_dat_i(7 downto 0);
   hw_to_stor <= wb_dat_i(15 downto 0);
   
   -- Memory load
   wb_dat_o <=
      STD_LOGIC_VECTOR(resize(SIGNED(byte_to_load), 32))
         when xfer_size = "01" and wb_tgd_i = '0' else
      STD_LOGIC_VECTOR(resize(SIGNED(hw_to_load), 32))
         when xfer_size = "10" and wb_tgd_i = '0' else
      STD_LOGIC_VECTOR(resize(UNSIGNED(byte_to_load), 32))
         when xfer_size = "01" else
      STD_LOGIC_VECTOR(resize(UNSIGNED(hw_to_load), 32))
         when xfer_size = "10" else
      blk_ram_out;

   with wb_sel_i select
      byte_to_load <=
         blk_ram_out(7 downto 0) when "0001",
         blk_ram_out(15 downto 8) when "0010",
         blk_ram_out(23 downto 16) when "0100",
         blk_ram_out(31 downto 24) when others;

   with wb_sel_i select
      hw_to_load <= blk_ram_out(15 downto 0) when "0011",
                    blk_ram_out(31 downto 16) when others;

   -- Memory stor
   with xfer_size select
      stor_data <=
         byte_stor_data when "01",
         hw_stor_data when "10",
         wb_dat_i when others;

   with wb_sel_i select
      byte_stor_data <=
         blk_ram_out(31 downto 8) & byte_to_stor when "0001",
         blk_ram_out(31 downto 16) & byte_to_stor & blk_ram_out(7 downto 0) when "0010",
         blk_ram_out(31 downto 24) & byte_to_stor & blk_ram_out(15 downto 0) when "0100",
         byte_to_stor & blk_ram_out(23 downto 0) when others;

   with wb_sel_i select
      hw_stor_data <= 
         blk_ram_out(31 downto 16) & hw_to_stor when "0011",
         hw_to_stor & blk_ram_out(15 downto 0) when others;
   
   
   Inst_data_mem : data_mem PORT MAP (
      clka => wb_clk_i,
      wea => blk_ram_we,
      addra => wb_adr_i,
      dina => stor_data,
      douta => blk_ram_out,
      clkb => wb_clk_i,
      enb => prog_mem_en,
      web => "0",
      addrb => prog_mem_addr,
      dinb => (others => '0'),
      doutb => prog_mem_inst
   );
   
   process(wb_clk_i)
      begin
         if rising_edge(wb_clk_i) then
            if state = CYCLE1 and wb_stb_i = '1' then
               state <= CYCLE2;
            else
               state <= CYCLE1;
            end if;
         end if;
      end process;

end Behavioral;
