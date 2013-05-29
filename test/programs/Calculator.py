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
	__Analysis_Dump_try1__
	aminusb  = calculator(10, "-", 20)
	__Analysis_Dump_try2__
except:
	err = "An error occured"