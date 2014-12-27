# chapter 8: virtual machine II: program control
High-level languages offer high expressive power by providing *subroutines*, which:

  * can be freely defined
  * can be freely called, like elementary operations
  * will execute, then return to the next sequential command

Each subroutine call requires:

  * passing parameters from the caller
  * saving the caller's state
  * allocating space for local variables
  * jumping to the subroutine
  * returning values to the caller
  * recycling the memory used by the subroutine
  * restoring the caller's state

Programs execute linearly with branching, which is facilitated by jumps (`goto`). Use the top value of the stack to
determine whether or not a jump should occur. *subroutines* (aka *procedures*, *methods*, *functions*):

  * require allocated arguments
  * require local variables
  * can be nested to arbitrary depths or outright recursive

Those traits map perfectly onto the *stack* data structure already employed by the VM; its *LIFO* property means that
newly called subroutines get pushed onto the stack, and the caller will only resume execution when they `return`.

A *stack frame* is the logical component of the *global stack*, and consists of:

  * local variables
  * arguments on which it operates
  * the working stack
  * other memory segments
