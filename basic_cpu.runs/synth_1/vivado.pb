
�
Command: %s
1870*	planAhead2�
�read_checkpoint -auto_incremental -incremental C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/basic_cpu.srcs/utils_1/imports/synth_1/top_basys3.dcpZ12-2866h px� 
�
;Read reference checkpoint from %s for incremental synthesis3154*	planAhead2k
iC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/basic_cpu.srcs/utils_1/imports/synth_1/top_basys3.dcpZ12-5825h px� 
T
-Please ensure there are no constraint changes3725*	planAheadZ12-7989h px� 
e
Command: %s
53*	vivadotcl24
2synth_design -top top_basys3 -part xc7a35tcpg236-1Z4-113h px� 
:
Starting synth_design
149*	vivadotclZ4-321h px� 
z
@Attempting to get a license for feature '%s' and/or device '%s'
308*common2
	Synthesis2	
xc7a35tZ17-347h px� 
j
0Got license for feature '%s' and/or device '%s'
310*common2
	Synthesis2	
xc7a35tZ17-349h px� 
D
Loading part %s157*device2
xc7a35tcpg236-1Z21-403h px� 

VNo compile time benefit to using incremental synthesis; A full resynthesis will be run2353*designutilsZ20-5440h px� 
�
�Flow is switching to default flow due to incremental criteria not met. If you would like to alter this behaviour and have the flow terminate instead, please set the following parameter config_implementation {autoIncr.Synth.RejectBehavior Terminate}2229*designutilsZ20-4379h px� 
o
HMultithreading enabled for synth_design using a maximum of %s processes.4828*oasys2
2Z8-7079h px� 
a
?Launching helper process for spawning children vivado processes4827*oasysZ8-7078h px� 
N
#Helper process launched with PID %s4824*oasys2
28184Z8-7075h px� 
�
%s*synth2z
xStarting RTL Elaboration : Time (s): cpu = 00:00:06 ; elapsed = 00:00:16 . Memory (MB): peak = 952.141 ; gain = 467.910
h px� 
�
synthesizing module '%s'638*oasys2

top_basys32J
FC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/top_basys3.vhd2
438@Z8-638h px� 
O
%s
*synth27
5	Parameter k_DIV bound to: 50000000 - type: integer 
h p
x
� 
�
Hmodule '%s' declared at '%s:%s' bound to instance '%s' of component '%s'3392*oasys2
clock_divider2K
IC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/clock_divider.vhd2
532
stateMachineClock_inst2
clock_divider2J
FC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/top_basys3.vhd2
1208@Z8-3491h px� 
�
synthesizing module '%s'638*oasys2
clock_divider2M
IC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/clock_divider.vhd2
638@Z8-638h px� 
O
%s
*synth27
5	Parameter k_DIV bound to: 50000000 - type: integer 
h p
x
� 
�
%done synthesizing module '%s' (%s#%s)256*oasys2
clock_divider2
02
12M
IC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/clock_divider.vhd2
638@Z8-256h px� 
M
%s
*synth25
3	Parameter k_DIV bound to: 100000 - type: integer 
h p
x
� 
�
Hmodule '%s' declared at '%s:%s' bound to instance '%s' of component '%s'3392*oasys2
clock_divider2K
IC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/clock_divider.vhd2
532
TdmClock_inst2
clock_divider2J
FC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/top_basys3.vhd2
1288@Z8-3491h px� 
�
synthesizing module '%s'638*oasys2
clock_divider__parameterized12M
IC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/clock_divider.vhd2
638@Z8-638h px� 
M
%s
*synth25
3	Parameter k_DIV bound to: 100000 - type: integer 
h p
x
� 
�
%done synthesizing module '%s' (%s#%s)256*oasys2
clock_divider__parameterized12
02
12M
IC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/clock_divider.vhd2
638@Z8-256h px� 
�
Hmodule '%s' declared at '%s:%s' bound to instance '%s' of component '%s'3392*oasys2
TDM42B
@C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/TDM4.vhd2
562

TDM_inst2
TDM42J
FC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/top_basys3.vhd2
1368@Z8-3491h px� 
�
synthesizing module '%s'638*oasys2
TDM42D
@C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/TDM4.vhd2
698@Z8-638h px� 
J
%s
*synth22
0	Parameter k_WIDTH bound to: 4 - type: integer 
h p
x
� 
�
%done synthesizing module '%s' (%s#%s)256*oasys2
TDM42
02
12D
@C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/TDM4.vhd2
698@Z8-256h px� 
�
Hmodule '%s' declared at '%s:%s' bound to instance '%s' of component '%s'3392*oasys2
ALU2A
?C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/ALU.vhd2
402

ALU_inst2
ALU2J
FC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/top_basys3.vhd2
1528@Z8-3491h px� 
�
synthesizing module '%s'638*oasys2
ALU2C
?C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/ALU.vhd2
518@Z8-638h px� 
�
5synthesizing blackbox instance '%s' of component '%s'637*oasys2
u0_ALU2
ripple_adder2C
?C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/ALU.vhd2
828@Z8-637h px� 
�
5synthesizing blackbox instance '%s' of component '%s'637*oasys2
Ripple_Upper2
ripple_adder2C
?C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/ALU.vhd2
948@Z8-637h px� 
�
array index %s out of range7663*oasys2
32C
?C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/ALU.vhd2
1158@Z8-11324h px� 
�
failed synthesizing module '%s'285*oasys2
ALU2C
?C:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/ALU.vhd2
518@Z8-285h px� 
�
failed synthesizing module '%s'285*oasys2

top_basys32J
FC:/Users/C27Grace.Kurian/ece281/LAB/lab5.copy/Lab-5/src/top_basys3.vhd2
438@Z8-285h px� 
�
%s*synth2{
yFinished RTL Elaboration : Time (s): cpu = 00:00:07 ; elapsed = 00:00:21 . Memory (MB): peak = 1057.367 ; gain = 573.137
h px� 
C
Releasing license: %s
83*common2
	SynthesisZ17-83h px� 
~
G%s Infos, %s Warnings, %s Critical Warnings and %s Errors encountered.
28*	vivadotcl2
242
02
02
4Z4-41h px� 
<

%s failed
30*	vivadotcl2
synth_designZ4-43h px� 
|
Command failed: %s
69*common2G
ESynthesis failed - please see the console or run log file for detailsZ17-69h px� 
\
Exiting %s at %s...
206*common2
Vivado2
Mon May  5 20:45:23 2025Z17-206h px� 


End Record