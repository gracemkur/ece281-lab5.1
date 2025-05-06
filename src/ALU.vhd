--+----------------------------------------------------------------------------
--|
--| NAMING CONVENSIONS :
--|
--|    xb_<port name>           = off-chip bidirectional port ( _pads file )
--|    xi_<port name>           = off-chip input port         ( _pads file )
--|    xo_<port name>           = off-chip output port        ( _pads file )
--|    b_<port name>            = on-chip bidirectional port
--|    i_<port name>            = on-chip input port
--|    o_<port name>            = on-chip output port
--|    c_<signal name>          = combinatorial signal
--|    f_<signal name>          = synchronous signal
--|    ff_<signal name>         = pipeline stage (ff_, fff_, etc.)
--|    <signal name>_n          = active low signal
--|    w_<signal name>          = top level wiring signal
--|    g_<generic name>         = generic
--|    k_<constant name>        = constant
--|    v_<variable name>        = variable
--|    sm_<state machine type>  = state machine type definition
--|    s_<signal name>          = state name
--|
--+----------------------------------------------------------------------------
--|
--| ALU OPCODES:
--|
--|     ADD     000
--|     SUB     001
--|     AND     011
--|     OR      010
--|     LSHIFT  11X
--|     RSHIFT  10X
--|
--+----------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;



entity ALU is
    Port(
        i_A : in  STD_LOGIC_VECTOR (7 downto 0);
        i_B : in STD_LOGIC_VECTOR (7 downto 0);
        i_op : in STD_LOGIC_VECTOR (2 downto 0);
        o_result : out STD_LOGIC_VECTOR (7 downto 0);
        o_flags : out STD_LOGIC_VECTOR (2 downto 0)
    );
    
end ALU;

architecture behavioral of ALU is 
  
	-- declare components and signals
	component ripple_adder
        port ( A     : in  STD_LOGIC_VECTOR(3 downto 0);
               B     : in  STD_LOGIC_VECTOR(3 downto 0);
               Cin   : in  STD_LOGIC;
               S     : out STD_LOGIC_VECTOR(3 downto 0);
               Cout  : out STD_LOGIC);
    end component;
    
--    signal w_add_res : STD_LOGIC_VECTOR(8 downto 0); -- output of adder to input of mux
--    signal w_andor_res : STD_LOGIC_VECTOR(8 downto 0); -- output of and/or gate
--    signal w_Lshift_res : STD_LOGIC_VECTOR(8 downto 0); -- output of left shift
--    signal w_Rshift_res : STD_LOGIC_VECTOR(8 downto 0); -- output of right shift
--    signal w_final_res : STD_LOGIC_VECTOR(8 downto 0); -- the output of the mux, after selecting which operation to output
    
--    signal w_9bit_A : STD_LOGIC_VECTOR(8 downto 0);
--    signal w_9bit_B : STD_LOGIC_VECTOR(8 downto 0);
    signal x_sum, resultOUT : STD_LOGIC_VECTOR(7 downto 0);
 
    signal x_lower_carry, x_upper_carry : STD_LOGIC;
 
    signal q_result : STD_LOGIC_VECTOR(7 downto 0);
 
    signal B_mux : STD_LOGIC_VECTOR(7 downto 0);
 
    signal Cin_mux : STD_LOGIC;
    
begin
	-- PORT MAPS ----------------------------------------
    u0_ALU : ripple_adder
 
        port map ( A    => i_A(3 downto 0),
 
                   B    => B_mux(3 downto 0),
 
                   Cin  => Cin_mux,
 
                   S    => x_sum(3 downto 0),
 
                   Cout => x_lower_carry );
 
    Ripple_Upper : ripple_adder -- Upper Bits
 
        port map ( A    => i_A(7 downto 4),
                   B    => B_mux(7 downto 4),
                   Cin  => x_lower_carry,
                   S    => x_sum(7 downto 4),
                   Cout => x_upper_carry );
    -- MUX
    B_mux   <= not i_B when i_op = "001" else i_B;
    Cin_mux <= '1' when i_op = "001" else '0';
    resultOUT <= x_sum when i_op = "000" else
                 x_sum when i_op = "001" else
                 (i_A and i_B) when i_op = "010" else
                 (i_A or i_B) when i_op = "011" else
                 (others => '0');
    q_result <= resultOUT;
    -- overflow flag
    o_flags(0) <= not (i_A(7) xor i_B(7) xor i_op(0)) and (i_A(7) xor x_sum(7)) and (not i_op(1));
    -- carry
    o_flags(1) <= x_upper_carry and (not i_op(1));
    -- negative
    o_flags(3) <= resultOUT(7);
    -- zero
    o_flags(2) <= '1' when (resultOUT = "00000000") else '0';
    o_result <= q_result;
	
	-- CONCURRENT STATEMENTS ----------------------------
    -- cast the inputs to 9-bit vectors
--    w_9bit_A(8) <= '0';
--    w_9bit_A(7 downto 0) <= i_A;
--    w_9bit_B(8) <= '0';
--    w_9bit_B(7 downto 0) <= i_B;
	
	
	-- perform the operations
--	w_add_res <= std_logic_vector(unsigned(w_9bit_A) + unsigned(w_9bit_B)) when (i_op = "000") else
--	             std_logic_vector(unsigned(w_9bit_A) - unsigned(w_9bit_B)) when (i_op = "001") else
--	             "000000000";
--	w_andor_res <= w_9bit_A and w_9bit_B when (i_op = "011") else
--                                w_9bit_A or w_9bit_B when (i_op = "010") else
--                                "000000000";
                                
--     w_Lshift_res <= std_logic_vector(shift_left(unsigned(w_9bit_A), to_integer(unsigned(w_9bit_B(2 downto 0))))) when (i_op = "110") else
--                     std_logic_vector(shift_left(unsigned(w_9bit_A), to_integer(unsigned(w_9bit_B(2 downto 0))))) when (i_op = "111") else
--                     "000000000";
                     
--     w_Rshift_res <= std_logic_vector(shift_right(unsigned(w_9bit_A), to_integer(unsigned(w_9bit_B(2 downto 0))))) when (i_op = "100") else
--                     std_logic_vector(shift_right(unsigned(w_9bit_A), to_integer(unsigned(w_9bit_B(2 downto 0))))) when (i_op = "101") else
--                     "000000000";	             

      
         
	
--	w_final_res <= w_add_res when (i_op = "000") else
--	            w_add_res when (i_op = "001") else
--	            w_andor_res when (i_op = "011") else
--	            w_andor_res when (i_op = "010") else
--	            w_Rshift_res when (i_op = "100") else
--	            w_Rshift_res when (i_op = "101") else
--	            w_Lshift_res when (i_op = "110") else
--	            w_Lshift_res when (i_op = "111") else
--	            b"000000000";
	
--	-- sign bit is the MSB of the result 
--	o_flags(2) <= w_final_res(7);
	
--	-- zero flag
--	o_flags(1) <= '1' when (w_final_res(7 downto 0) = b"00000000") else
--	              '0';
    
--    -- carry flag                
--	o_flags(0) <= '1' when (w_final_res(8) = '1') else
--	              '0';
	
--	-- output the result signal
--	o_result <= w_final_res(7 downto 0);
	
end behavioral;