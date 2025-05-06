----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/03/2025 12:55:43 PM
-- Design Name: 
-- Module Name: sevenseg_decoder - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------
 
 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;
 
-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
 
entity sevenseg_decoder is
    Port ( i_D : in STD_LOGIC_VECTOR (3 downto 0);
           o_S : out STD_LOGIC_VECTOR (6 downto 0));
end sevenseg_decoder;
 
architecture Behavioral of sevenSegDecoder is
 
begin
 
with i_D select
    o_S <= "1000000" when x"0",
               "1111001" when x"1",
               "0100100" when x"2",
               "0110000" when x"3",
               "0011001" when x"4",
               "0010010" when x"5",
               "0000010" when x"6",
               "1111000" when x"7",
               "0000000" when x"8",
               "0011000" when x"9",
               "0001000" when x"A",
               "0000011" when x"B",
               "0100111" when x"C",
               "0100001" when x"D",
               "0000110" when x"E",
               "0001110" when x"F",
--0x1:0110000
--0x2:1101101
--0x3:1111001
--0x4:0110011
--0x5:1011011
--0x6:1011111
--0x7:1110000
--0x8:1111111
--0x9:1110011
--0x10:1110111
--0x11:0011111
--0x12:0100111
--0x13:0111101
--0x14:1001111
--0x15:1000111
                "1111111" when others;
end Behavioral;