# Portable way to get the You-Know-Which object without naming it
class __:
 def __getitem__(__, _):
  return _

___ = __()[...]

# An Ellipsobinary-to-ASCII convertor
class __:
 def __getitem__(__, _):
  return chr(sum(1<<i if _[-i-1] is ___ else 0 for i in range(len(_))))

_ = __()

# Finally, use the That-Which-Must-Not-Be-Named object
print (
 _[...,_,_,...,_,_,_] +
  _[...,...,_,_,...,_,...] +
   _[...,...,_,...,...,_,_] +
    _[...,...,_,...,...,_,_] +
     _[...,...,_,...,...,...,...] +
      _[...,_,...,...,_,_] +
       _[...,_,_,_,_,_] +
        _[...,...,...,_,...,...,...] +
         _[...,...,_,...,...,...,...] +
          _[...,...,...,_,_,...,_] +
           _[...,...,_,...,...,_,_] +
            _[...,...,_,_,...,_,_] +
             _[...,_,_,_,_,...])