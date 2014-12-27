// Inputs: R0
// Outputs: R1
// Function: Calculates the terminal function of R0, or the sum of all positive
// consecutive integers from 1 to R0, and stores the result in R1.

@sum
M=0

@R0
D=M

(LOOP)
	@END
	D;JEQ

	@sum
	M=M+D
	D=D-1

	@LOOP
	0;JMP

(END)
	@END
	0;JMP
