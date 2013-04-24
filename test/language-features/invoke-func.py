def f():
	return 10

f.f = lambda: 20

f()
f.f()
