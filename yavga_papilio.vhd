--------------------------------------------------------------------------------
----                                                                        ----
---- This file is part of the yaVGA project                                 ----
---- http://www.opencores.org/?do=project&who=yavga                         ----
----                                                                        ----
---- Description                                                            ----
---- Implementation of yaVGA IP core                                        ----
----                                                                        ----
---- To Do:                                                                 ----
----                                                                        ----
----                                                                        ----
---- Author(s):                                                             ----
---- Sandro Amato, sdroamt@netscape.net                                     ----
----                                                                        ----
--------------------------------------------------------------------------------
----                                                                        ----
---- Copyright (c) 2009, Sandro Amato                                       ----
---- All rights reserved.                                                   ----
----                                                                        ----
---- Redistribution  and  use in  source  and binary forms, with or without ----
---- modification,  are  permitted  provided that  the following conditions ----
---- are met:                                                               ----
----                                                                        ----
----     * Redistributions  of  source  code  must  retain the above        ----
----       copyright   notice,  this  list  of  conditions  and  the        ----
----       following disclaimer.                                            ----
----     * Redistributions  in  binary form must reproduce the above        ----
----       copyright   notice,  this  list  of  conditions  and  the        ----
----       following  disclaimer in  the documentation and/or  other        ----
----       materials provided with the distribution.                        ----
----     * Neither  the  name  of  SANDRO AMATO nor the names of its        ----
----       contributors may be used to  endorse or  promote products        ----
----       derived from this software without specific prior written        ----
----       permission.                                                      ----
----                                                                        ----
---- THIS SOFTWARE IS PROVIDED  BY THE COPYRIGHT  HOLDERS AND  CONTRIBUTORS ----
---- "AS IS"  AND  ANY EXPRESS OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT ----
---- LIMITED  TO, THE  IMPLIED  WARRANTIES  OF MERCHANTABILITY  AND FITNESS ----
---- FOR  A PARTICULAR  PURPOSE  ARE  DISCLAIMED. IN  NO  EVENT  SHALL  THE ----
---- COPYRIGHT  OWNER  OR CONTRIBUTORS  BE LIABLE FOR ANY DIRECT, INDIRECT, ----
---- INCIDENTAL,  SPECIAL,  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, ----
---- BUT  NOT LIMITED  TO,  PROCUREMENT OF  SUBSTITUTE  GOODS  OR SERVICES; ----
---- LOSS  OF  USE,  DATA,  OR PROFITS;  OR  BUSINESS INTERRUPTION) HOWEVER ----
---- CAUSED  AND  ON  ANY THEORY  OF LIABILITY, WHETHER IN CONTRACT, STRICT ----
---- LIABILITY,  OR  TORT  (INCLUDING  NEGLIGENCE  OR OTHERWISE) ARISING IN ----
---- ANY  WAY OUT  OF THE  USE  OF  THIS  SOFTWARE,  EVEN IF ADVISED OF THE ----
---- POSSIBILITY OF SUCH DAMAGE.                                            ----
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
---- Modified to run on the Papilio One FPGA board                          ----
---- by Jack Gassett 1/31/2011                                              ----
---- www.GadgetFactory.net                                                  ----
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_ARITH.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

use work.yavga_pkg.all;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity yavga_papilio is
  Generic(											-- TEXT_MODE sets the type of text characters that are generated. Change the integer to control what text character mode to use.
			TEXT_MODE : integer := 1			-- 0 = no generated text characters, 1 = Gadget Factory Text, 2 = Random Characters, 3 = Default mode that came with original yavga source code.
--			WAVEFORM_MODE : integer := 0		-- Waveform mode controls the waveform that is displayed. 0 = no waveform, 1 = default waveform, 2 = random waveform
  );
  port (pp_clk   : in  std_logic;
		  msg_l1 : in std_logic_vector(256*8 downto 0);
		  msg_l2 : in std_logic_vector(256*8 downto 0);
		  msg_l3 : in std_logic_vector(256*8 downto 0);
		  msg_l4 : in std_logic_vector(256*8 downto 0);
        o_hsync : out std_logic;
        o_vsync : out std_logic;
        o_r     : out std_logic;
        o_g     : out std_logic;
        o_b     : out std_logic);
end yavga_papilio;

architecture Behavioral of yavga_papilio is

  component vga_ctrl
    port(
      i_clk       : in  std_logic;
      i_reset     : in  std_logic;
      i_h_sync_en : in  std_logic;
      i_v_sync_en : in  std_logic;
      i_chr_addr  : in  std_logic_vector(c_CHR_ADDR_BUS_W - 1 downto 0);
      i_chr_data  : in  std_logic_vector(c_CHR_DATA_BUS_W - 1 downto 0);
      i_chr_clk   : in  std_logic;
      i_chr_en    : in  std_logic;
      i_chr_we    : in  std_logic_vector(c_CHR_WE_BUS_W - 1 downto 0);
      i_chr_rst   : in  std_logic;
      i_wav_d     : in  std_logic_vector(c_WAVFRM_DATA_BUS_W - 1 downto 0);
      i_wav_clk   : in  std_logic;
      i_wav_we    : in  std_logic;
      i_wav_addr  : in  std_logic_vector(c_WAVFRM_ADDR_BUS_W - 1 downto 0);
      o_h_sync    : out std_logic;
      o_v_sync    : out std_logic;
      o_r         : out std_logic;
      o_g         : out std_logic;
      o_b         : out std_logic;
      o_chr_data  : out std_logic_vector(c_CHR_DATA_BUS_W - 1 downto 0)
      );
  end component;

--	COMPONENT DCM32to50
--	PORT(
--		CLKIN_IN : IN std_logic;          
--		CLKFX_OUT : OUT std_logic;
--		CLKIN_IBUFG_OUT : OUT std_logic;
--		CLK0_OUT : OUT std_logic
--		);
--	END COMPONENT;

  signal s_hsync : std_logic;
  signal s_vsync : std_logic;
  signal s_r     : std_logic;
  signal s_g     : std_logic;
  signal s_b     : std_logic;
  signal i_clk   : std_logic;
  signal s_rnd_bit  : std_logic;
  

  signal s_vsync_count : std_logic_vector(7 downto 0) := (others => '0');
  signal s_vsync1      : std_logic;
  
  signal s_hsync_count : std_logic_vector(4 downto 0) := (others => '0');
  signal s_hsync1      : std_logic;

  signal s_chr_addr : std_logic_vector(c_CHR_ADDR_BUS_W - 1 downto 0);  -- := (others => '0');
  signal s_chr_data : std_logic_vector(c_CHR_DATA_BUS_W - 1 downto 0);  -- := (others => '0');
  signal s_rnd      : std_logic_vector(c_CHR_DATA_BUS_W - 1 downto 0);  -- := (others => '0');
  signal s_chr_we   : std_logic_vector(c_CHR_WE_BUS_W - 1 downto 0);

  signal s_wav_addr : std_logic_vector(c_WAVFRM_ADDR_BUS_W - 1 downto 0);
  signal s_wav_d    : std_logic_vector(c_WAVFRM_DATA_BUS_W - 1 downto 0);
  signal s_mul      : std_logic_vector(7 downto 0);

  signal s_initialized : std_logic := '0';

  attribute U_SET                  : string;
  attribute U_SET of "u1_vga_ctrl" : label is "u1_vga_ctrl_uset";
  
 
  
begin
  o_hsync <= s_hsync;
  o_vsync <= s_vsync;
  o_r     <= s_r;
  o_g     <= s_g;
  o_b     <= s_b;
  
--	Inst_DCM32to50: DCM32to50 PORT MAP(
--		CLKIN_IN => pp_clk,
--		CLKFX_OUT => i_clk,
--		CLKIN_IBUFG_OUT => open,
--		CLK0_OUT => open
--	); 

		i_clk <= pp_clk;
  
  u1_vga_ctrl : vga_ctrl port map(
    i_clk       => i_clk,
    i_reset     => '0',
    o_h_sync    => s_hsync,
    o_v_sync    => s_vsync,
    i_h_sync_en => '1',
    i_v_sync_en => '1',
    o_r         => s_r,
    o_g         => s_g,
    o_b         => s_b,
    i_chr_addr  => s_chr_addr,          --B"000_0000_0000",
    i_chr_data  => s_chr_data,          --X"00000000",
    o_chr_data  => open,
    i_chr_clk   => i_clk,
    i_chr_en    => '1',
    i_chr_we    => s_chr_we,
    i_chr_rst   => '0',
    i_wav_d     => s_wav_d,  --X"0000",  --s_rnd(15 downto 0), --
    i_wav_clk   => i_clk,
    i_wav_we    => '0',  --'0',  -- '1',
    i_wav_addr  => s_wav_addr  --B"00_0000_0000"  --s_chr_addr(9 downto 0) --
    );
  s_wav_addr <= s_rnd(1 downto 0) & s_vsync_count;
  s_mul      <= s_vsync_count(3 downto 0) * s_vsync_count(3 downto 0);
--  s_wav_d    <= B"000" & s_rnd(2 downto 0) & B"00" & s_mul;
  s_wav_d <= B"000" & "100" & B"00" & s_mul;

GFText_Impl:if TEXT_MODE = 1 generate					--1 = Gadget Factory Text
--              cols              cols
--           00_01_02_03  ...  96_97_98_99
--   row_00 "00000000000" ... "00000011000"
--   row_01 "00000100000" ... "00000111000"
--    ...        ...               ...
--   row_37 "10010100000" ... "10010111000"
  p_write_chars : process(i_clk)
  
	 variable msg_pos : integer range 0 to 256*8 := 256*8;
	 variable msg_vsync : std_logic_vector(7 downto 0) := (others => '0');
	 variable msg_addr_l1 : std_logic_vector(10 downto 0) := "00000101100";
	 variable msg_addr_l2 : std_logic_vector(10 downto 0) := "00001101100";
	 variable msg_addr_l3 : std_logic_vector(10 downto 0) := "00010101100";
	 variable msg_addr_l4 : std_logic_vector(10 downto 0) := "00011101100";
	 variable i : integer range 0 to 64 := 0;
	 
  begin
    if rising_edge(i_clk) then
      if s_initialized = '0' then
			while i < 64 loop
				
				if s_hsync_count = 0 then
				
					if s_vsync_count = msg_vsync then			                 
							s_chr_we   <= "1111";
							s_chr_addr <= msg_addr_l1;
							s_chr_data <= msg_l1(msg_pos downto msg_pos-31);
					elsif s_vsync_count = "00000101" then  -- write config grid and cursor color (overwrite RAM defaults)
							s_chr_we      <= "1111";
							s_chr_addr    <= c_CFG_BG_CUR_COLOR_ADDR(c_CFG_BG_CUR_COLOR_ADDR'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
							--             ND   bgColor grid,cur   ND       curs_x          curs_y
							s_chr_data    <= "00" & "000" & "101" & "000" & "00000000000" & "0000000000";
							--            |--------108-------|-------109-------|----110-----|--111--|
							s_initialized <= '1';
					else
							s_chr_we   <= (others => '0');
							s_chr_addr <= (others => '1');
							s_chr_data <= x"00" & x"00" & x"00" & x"00";
					end if;
				end if;
				
				if s_hsync_count = 1 then
				
					if s_vsync_count = msg_vsync then                
							s_chr_we   <= "1111";
							s_chr_addr <= msg_addr_l2;
							s_chr_data <= msg_l2(msg_pos downto msg_pos-31);
					elsif s_vsync_count = "00000101" then  -- write config grid and cursor color (overwrite RAM defaults)
							s_chr_we      <= "1111";
							s_chr_addr    <= c_CFG_BG_CUR_COLOR_ADDR(c_CFG_BG_CUR_COLOR_ADDR'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
							--             ND   bgColor grid,cur   ND       curs_x          curs_y
							s_chr_data    <= "00" & "000" & "101" & "000" & "00000000000" & "0000000000";
							--            |--------108-------|-------109-------|----110-----|--111--|
							s_initialized <= '1';
					else
							s_chr_we   <= (others => '0');
							s_chr_addr <= (others => '1');
							s_chr_data <= x"00" & x"00" & x"00" & x"00";
					end if;
				end if;
				
				if s_hsync_count = 2 then
				
					if s_vsync_count = msg_vsync then                
							s_chr_we   <= "1111";
							s_chr_addr <= msg_addr_l3;
							s_chr_data <= msg_l3(msg_pos downto msg_pos-31);
					elsif s_vsync_count = "00000101" then  -- write config grid and cursor color (overwrite RAM defaults)
							s_chr_we      <= "1111";
							s_chr_addr    <= c_CFG_BG_CUR_COLOR_ADDR(c_CFG_BG_CUR_COLOR_ADDR'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
							--             ND   bgColor grid,cur   ND       curs_x          curs_y
							s_chr_data    <= "00" & "000" & "101" & "000" & "00000000000" & "0000000000";
							--            |--------108-------|-------109-------|----110-----|--111--|
							s_initialized <= '1';
					else
							s_chr_we   <= (others => '0');
							s_chr_addr <= (others => '1');
							s_chr_data <= x"00" & x"00" & x"00" & x"00";
					end if;
				end if;
				
				if s_hsync_count = 3 then
				
					if s_vsync_count = msg_vsync then                
							s_chr_we   <= "1111";
							s_chr_addr <= msg_addr_l4;
							s_chr_data <= msg_l4(msg_pos downto msg_pos-31);
					elsif s_vsync_count = "00000101" then  -- write config grid and cursor color (overwrite RAM defaults)
							s_chr_we      <= "1111";
							s_chr_addr    <= c_CFG_BG_CUR_COLOR_ADDR(c_CFG_BG_CUR_COLOR_ADDR'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
							--             ND   bgColor grid,cur   ND       curs_x          curs_y
							s_chr_data    <= "00" & "000" & "101" & "000" & "00000000000" & "0000000000";
							--            |--------108-------|-------109-------|----110-----|--111--|
							s_initialized <= '1';
					else
							s_chr_we   <= (others => '0');
							s_chr_addr <= (others => '1');
							s_chr_data <= x"00" & x"00" & x"00" & x"00";
					end if;
				end if;
				
				msg_addr_l1 := msg_addr_l1 + 1;
				msg_addr_l2 := msg_addr_l2 + 1;
				msg_addr_l3 := msg_addr_l3 + 1;
				msg_addr_l4 := msg_addr_l4 + 1;
				msg_pos := msg_pos - 32;
				msg_vsync := msg_vsync + 1;
				i := i + 1;
		  	
			end loop;									  
      else
        s_chr_we <= (others => '0');
      end if;
    end if;
  end process;
end generate;

RandomText_Impl:if TEXT_MODE = 2 generate			--2 = Random Characters
  s_chr_data <= s_rnd;
  p_write_chars : process(i_clk)
  begin
    if rising_edge(i_clk) then
      -- during the sync time in order to avoid flickering
      -- and each 128 vsync in order to stop for a while
      -- will write random chars...
      if s_vsync_count(7) = '1' and (s_hsync = '0' or s_vsync = '0') then
        -- generate a pseudo random 32 bit number
        s_rnd <= s_rnd(30 downto 0) & (s_rnd(31) xnor s_rnd(21) xnor s_rnd(1) xnor s_rnd(0));
        -- increment the address and write enable...
        s_chr_addr <= s_chr_addr + 1;
        s_chr_we   <= "1111";
      else
        s_chr_addr <= s_chr_addr;
        s_chr_we   <= "0000";
        s_rnd      <= s_rnd;
      end if;
    end if;
  end process;
end generate;

yavgaText_Impl:if TEXT_MODE = 3 generate			--3 = Default mode that came with original yavga source code
--              cols              cols
--           00_01_02_03  ...  96_97_98_99
--   row_00 "00000000000" ... "00000011000"
--   row_01 "00000100000" ... "00000111000"
--    ...        ...               ...
--   row_37 "10010100000" ... "10010111000"
  p_write_chars : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if s_initialized = '0' then
        case s_vsync_count(2 downto 0) is
          when "000" =>                 -- write ABCD
            s_chr_we   <= "1111";
            s_chr_addr <= "00000000000";
            s_chr_data <= "01000001" & "01000010" & "01000011" & "01000100";
          when "001" =>                 -- write EFGH
            s_chr_we   <= "1111";
            s_chr_addr <= "00000011000";
            s_chr_data <= "01000101" & "01000110" & "01000111" & "01001000";
          when "010" =>                 -- write IJKL
            s_chr_we   <= "1111";
            s_chr_addr <= "00000100000";
            s_chr_data <= "01001001" & "01001010" & "01001011" & "01001100";
          when "011" =>                 -- write MNOP
            s_chr_we   <= "1111";
            s_chr_addr <= "10010100000";
            s_chr_data <= "01001101" & "01001110" & "01001111" & "01010000";
          when "100" =>                 -- write QRST
            s_chr_we   <= "1111";
            s_chr_addr <= "10010111000";
            s_chr_data <= "01010001" & "01010010" & "01010011" & "01010100";
          when "101" =>  -- write config grid and cursor color (overwrite RAM defaults)
            s_chr_we      <= "1111";
            s_chr_addr    <= c_CFG_BG_CUR_COLOR_ADDR(c_CFG_BG_CUR_COLOR_ADDR'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
            --             ND   bgColor grid,cur   ND       curs_x          curs_y
            s_chr_data    <= "00" & "000" & "101" & "000" & "00111000010" & "0101011110";
            --            |--------108-------|-------109-------|----110-----|--111--|
            s_initialized <= '1';
          when others =>
            s_chr_we   <= (others => '0');
            s_chr_addr <= (others => '1');
            s_chr_data <= "10111110" & "10111101" & "10111100" & "10111011";
        end case;
      else
        s_chr_we <= (others => '0');
      end if;
    end if;
  end process;
end generate;

--  p_rnd_bit : process(i_clk)
--    variable v_rnd_fb : std_logic;
--    variable v_rnd : std_logic_vector(31 downto 0);
--  begin
--    if rising_edge(i_clk) then
--      s_rnd_bit <= v_rnd_fb;
--      v_rnd_fb := v_rnd(31) xnor v_rnd(21) xnor v_rnd(1) xnor v_rnd(0);
--      v_rnd    := v_rnd(30 downto 0) & v_rnd_fb;
--    end if;
--  end process;

  p_vsync_count : process(i_clk)
  begin
    if rising_edge(i_clk) then
      s_vsync1 <= s_vsync;
      if (not s_vsync and s_vsync1) = '1' then  -- pulse on vsync falling
        s_vsync_count <= s_vsync_count + 1;
      end if;
    end if;
  end process;
  
  p_hsync_count : process(i_clk)
  begin
    if rising_edge(i_clk) then
      s_hsync1 <= s_hsync;
      if (not s_hsync and s_hsync1) = '1' then  -- pulse on hsync falling
        s_hsync_count <= s_hsync_count + 1;
      end if;
    end if;
  end process;



end Behavioral;


--												  case s_vsync_count(2 downto 0) is
--													 when "000" =>                 -- write Gadg
--														s_chr_we   <= "1111";
--														s_chr_addr <= "00000101100";
--														s_chr_data <= x"47" & x"61" & x"64" & x"67";
--													 when "001" =>                 -- write et F  
--														s_chr_we   <= "1111";
--														s_chr_addr <= "00000101101";
--														s_chr_data <= x"65" & x"74" & x"20" & x"46";
--													 when "010" =>                 -- write acto
--														s_chr_we   <= "1111";
--														s_chr_addr <= "00000101110";
--														s_chr_data <= x"61" & x"63" & x"74" & x"6f";
--													 when "011" =>                 -- write ry
--														s_chr_we   <= "1111";
--														s_chr_addr <= "00000101111";
--														s_chr_data <= x"72" & x"79" & x"00" & x"00";
--										--          when "100" =>  -- configure cursor XY2 and XY3 (overwrite RAM defaults)
--										--            s_chr_we      <= "1111";
--										--            s_chr_addr    <= c_CFG_CURS_XY2(c_CFG_CURS_XY2'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
--										--            --             ND   bgColor grid,cur   ND       curs_x          curs_y
--										--            s_chr_data    <= x"11111111";
--										--            --            |--------108-------|-------109-------|----110-----|--111--|
--													 when "101" =>  -- write config grid and cursor color (overwrite RAM defaults)
--														s_chr_we      <= "1111";
--														s_chr_addr    <= c_CFG_BG_CUR_COLOR_ADDR(c_CFG_BG_CUR_COLOR_ADDR'left downto 2);  -- c_CFG_BG_CUR_COLOR_ADDR >> 2
--														--             ND   bgColor grid,cur   ND       curs_x          curs_y
--														s_chr_data    <= "00" & "000" & "101" & "000" & "00000000000" & "0000000000";
--														--            |--------108-------|-------109-------|----110-----|--111--|
--														s_initialized <= '1';
--													 when others =>
--														s_chr_we   <= (others => '0');
--														s_chr_addr <= (others => '1');
--														s_chr_data <= x"00" & x"00" & x"00" & x"00";
