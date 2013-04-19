exec "print 'test'"
exec "print a" in {'a': 3}
exec "print b" in {'a': 3}, {'b': 4}