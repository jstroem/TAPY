class F(object):
	pass

class G(object):
	pass

if (True):
	GF = F
else:
	GF = G

class E(object):
	pass

class D(object):
	pass

class C(D,GF):
	x = y

class B(D,E):
	pass

class A(B,C):
	x = y # should return A B C D E F O