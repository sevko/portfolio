// Inputs: R0, R1
// Outputs: R2
// Function: Compute `R0 / R1` and store it in `R2`. Both `R0` and `R1` must be
//      positive.

@R0
D=M

@dividend
M=D

@R2
M=-1

(LOOP)
	@R2
	M=M+1

	@R1
	D=M

	@dividend
	M=M-D
	D=M

	@END
	D;JLT

	@LOOP
	0;JMP

(END)
	@END
	0;JMP
