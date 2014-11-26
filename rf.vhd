library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity rf is
    Port ( clk : in STD_LOGIC;
           we : in STD_LOGIC;
           rd  : in  STD_LOGIC_VECTOR (4 downto 0);
           data_in : in STD_LOGIC_VECTOR(31 downto 0);
           rs1 : in  STD_LOGIC_VECTOR (4 downto 0);
           rs2 : in  STD_LOGIC_VECTOR (4 downto 0);
           out_x : out  STD_LOGIC_VECTOR (31 downto 0);
           out_y : out  STD_LOGIC_VECTOR (31 downto 0);
           reg_peek : out STD_LOGIC_VECTOR(15 downto 0)
          );
end rf;

architecture Synthesizable of rf is

   signal r1,r2,r3,r4,r5,r6,r7,
          r8,r9,r10,r11,r12,r13,r14,r15,
          r16,r17,r18,r19,r20,r21,r22,r23,
          r24,r25,r26,r27,r28,r29,r30,r31
            : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
   
begin

   reg_peek <= r1(15 downto 0);
   
   with rs1 select
      out_x <= (others => '0') when "00000",
            r1 when "00001",
            r2 when "00010",
            r3 when "00011",
            r4 when "00100",
            r5 when "00101",
            r6 when "00110",
            r7 when "00111",
            r8 when "01000",
            r9 when "01001",
            r10 when "01010",
            r11 when "01011",
            r12 when "01100",
            r13 when "01101",
            r14 when "01110",
            r15 when "01111",
            r16 when "10000",
            r17 when "10001",
            r18 when "10010",
            r19 when "10011",
            r20 when "10100",
            r21 when "10101",
            r22 when "10110",
            r23 when "10111",
            r24 when "11000",
            r25 when "11001",
            r26 when "11010",
            r27 when "11011",
            r28 when "11100",
            r29 when "11101",
            r30 when "11110",
            r31 when others;
            
   with rs2 select
      out_y <= (others => '0') when "00000",
            r1 when "00001",
            r2 when "00010",
            r3 when "00011",
            r4 when "00100",
            r5 when "00101",
            r6 when "00110",
            r7 when "00111",
            r8 when "01000",
            r9 when "01001",
            r10 when "01010",
            r11 when "01011",
            r12 when "01100",
            r13 when "01101",
            r14 when "01110",
            r15 when "01111",
            r16 when "10000",
            r17 when "10001",
            r18 when "10010",
            r19 when "10011",
            r20 when "10100",
            r21 when "10101",
            r22 when "10110",
            r23 when "10111",
            r24 when "11000",
            r25 when "11001",
            r26 when "11010",
            r27 when "11011",
            r28 when "11100",
            r29 when "11101",
            r30 when "11110",
            r31 when others;

   process(clk, we)
      begin
         if rising_edge(clk) and we = '1' then
            if rd = "00001" then
               r1 <= data_in;
            elsif rd = "00010" then
               r2 <= data_in;
            elsif rd = "00011" then
               r3 <= data_in;
            elsif rd = "00100" then
               r4 <= data_in;
            elsif rd = "00101" then
               r5 <= data_in;
            elsif rd = "00110" then
               r6 <= data_in;
            elsif rd = "00111" then
               r7 <= data_in;
            elsif rd = "01000" then
               r8 <= data_in;
            elsif rd = "01001" then
               r9 <= data_in;
            elsif rd = "01010" then
               r10 <= data_in;
            elsif rd = "01011" then
               r11 <= data_in;
            elsif rd = "01100" then
               r12 <= data_in;
            elsif rd = "01101" then
               r13 <= data_in;
            elsif rd = "01110" then
               r14 <= data_in;
            elsif rd =  "01111" then
               r15 <= data_in;
            elsif rd = "10000" then
               r16 <= data_in;
            elsif rd = "10001" then
               r17 <= data_in;
            elsif rd = "10010" then
               r18 <= data_in;
            elsif rd = "10011" then
               r19 <= data_in;
            elsif rd = "10100" then
               r20 <= data_in;
            elsif rd = "10101" then
               r21 <= data_in;
            elsif rd = "10110" then
               r22 <= data_in;
            elsif rd = "10111" then
               r23 <= data_in;
            elsif rd = "11000" then
               r24 <= data_in;
            elsif rd = "11001" then
               r25 <= data_in;
            elsif rd = "11010" then
               r26 <= data_in;
            elsif rd = "11011" then
               r27 <= data_in;
            elsif rd = "11100" then
               r28 <= data_in;
            elsif rd = "11101" then
               r29 <= data_in;
            elsif rd = "11110" then
               r30 <= data_in;
            elsif rd = "11111" then
               r31 <= data_in;
            end if;
         end if;
      end process;
      
end Synthesizable;
