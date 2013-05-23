class IllegalOperatorException(Exception):
	def __init__(self, msg):
		self.msg = msg

def calculator(a, op, b):
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

a = float(raw_input("First number: "))
op = raw_input("Operator: ")
b = float(raw_input("Second number: "))

try:
	result = calculator(a, op, b)
except ZeroDivisionError:
	err = "Division by zero"
except IllegalOperatorException as e:
	err = e.msg
