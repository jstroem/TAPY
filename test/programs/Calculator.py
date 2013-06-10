def calculator(a, op, b):
	if (op == "+"):
		result = a + b
	elif (op == "-"):
		result = a - b
	elif (op == "*"):
		result = a * b
	elif (op == "/"):
		result = a / b
	else:
		raise Exception()
	return result

try:
	aplusb = calculator(10, "+", 20)
	aminusb  = calculator(10, "-", 20)
except:
	err = "An error occured"