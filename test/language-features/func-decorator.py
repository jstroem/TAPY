def a(fn):
    return "a" + fn() + "a"
@a
def b():
    return "b"

print b