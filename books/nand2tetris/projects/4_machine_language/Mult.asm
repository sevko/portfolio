// Inputs: R0, R1
// Outputs: R2
// Function: Store the result of `R0` * `R1` in `R2`.

@R2
M=0

@R1
D=M

@ind
M=D

// Add R0 to R2, R1 times.
(LOOP)
	@ind
	D=M

	@END
	D;JEQ

	@ind
	M=M-1

	@R0
	D=M

	@R2
	M=D+M

	@LOOP
	0;JMP

(END)
	@END
	0;JMP
