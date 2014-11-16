// The IO_LOOP subroutine will poll the keyboard: on key-press, it'll paint the
// entire screen black; once no keys are pressed any longer, it'll clear it.
(IO_LOOP)
	@KBD
	D=M

	// key pressed
	@BLACK_SCREEN
	D;JNE

	// no key pressed
	@CLEAR_SCREEN
	M=0

	@FILL_SCREEN
	0;JMP

// Set the color for a black fill, and fill the screen.
(BLACK_SCREEN)
	@color
	M=-1
	@FILL_SCREEN
	0;JMP


// Set the color for a clear fill, and fill the screen.
(CLEAR_SCREEN)
	@color
	M=-1
	@FILL_SCREEN
	0;JMP

// The FILL_SCREEN subroutine will fill the screen from its bottom-right corner
// to the top-left, using the color value specified at @color (0 for white --
// clear, -1 for black -- fill).
(FILL_SCREEN)
	@8192
	D=A

	// The number of words left to draw.
	@word
	M=D

	// The address of the word being currently filled.
	@address
	M=D

	@SCREEN
	D=A

	@address
	M=M+D

	// Fill all the words from @(SCREEN + 8192) to @SCREEN (in that order),
	// then jump to @END_IO_LOOP.
	(FILL_WORD)
		@word
		D=M

		@IO_LOOP
		D;JLT

		@word
		M=M-1

		// Fill in the current word with the selected color.
		@color
		D=M

		@address
		A=M
		M=D

		@address
		M=M-1

		@FILL_WORD
		0;JMP
