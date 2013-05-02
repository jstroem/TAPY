class Foo(object):
    def __setitem__(self, key, value):
        print key, type(key)

a = Foo()
c = list([slice(1,1),3])
b = tuple(c)
a[1:1] = 1

a[1:1:1] = 1

a[1:1, 1:1] = 1
a[1:1,3] = 1
a[b] = 1
a[1,Ellipsis,3] = 1
a[1,...,3] = 1