import __builtin__
from __builtin__ import True

class IllegalOperatorException(Exception):
	def __init__(self, msg):
		self.msg = msg

def calc(a, op, b):
	if (operator == "+"):
		result = a + b
	elif (operator == "-"):
		result = a - b
	elif (operator == "*"):
		result = a * b
	elif (operator == "/"):
		result = a / b
	else:
		raise IllegalOperatorException("Illegal operator")

# main:

a = 1
op = "+"
b = 2

# a = float(raw_input("First number: "))
# op = raw_input("Operator: ")
# b = float(raw_input("Second number: "))

try:
	result = calculator(a, op, b)
except ZeroDivisionError:
	err = "Division by zero"
except IllegalOperatorException as e:
	err = e.msg