class C(object):
	x = 0

class D(object):
	y = C()

# PROPERTIES ON THE LEFT HAND SIDE

a = C()
# print C.x // 0
# print a.x // 0

C.x = 5
# print C.x // 5
# print a.x // 5

a.x = 10
# print C.x // 5
# print a.x // 10

b = D()
b.y = a
b.y.x = 15
# print a.x // 15

# PROPERTIES ON THE RIGHT HAND SIDE

b = a.x
c = Klass.x