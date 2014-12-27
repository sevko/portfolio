from vmtranslator.operations import arithmetic_logic
from vmtranslator.operations import memory
from vmtranslator.operations import program_flow
from vmtranslator.operations import function_calling

OPERATION_CLASSES = arithmetic_logic.OPS + memory.OPS + program_flow.OPS + \
	function_calling.OPS
