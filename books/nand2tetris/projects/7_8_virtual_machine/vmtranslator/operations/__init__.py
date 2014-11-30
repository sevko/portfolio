from vmtranslator.operations import arithmetic_logic
from vmtranslator.operations import memory
from vmtranslator.operations import program_flow

operation_classes = arithmetic_logic.ops + memory.ops + program_flow.ops
