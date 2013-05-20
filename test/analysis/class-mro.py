class G(object):
	pass

class F(object):
	pass

if (True):
	GF = F
else:
	GF = G

class E(GF):
	object

#class E(object):
#	pass

#class D(object):
#	pass

#class C(D,GF):
#	pass

#class B(D,E):
#	pass

#class A(B,C):
#	x = y # should return A B C D E F O