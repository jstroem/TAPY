class Foo(object):
    def __setitem__(self, key, value):
        print key

a = Foo()
a[1:1] = 1

a[1:1:1] = 1

a[1:1, 1:1] = 1
a[1,Ellipsis,3] = 1
a[1,...,3] = 1