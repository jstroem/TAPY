#!/usr/bin/python

import errno
import fcntl
import os
import select
import socket
import subprocess
import sys


# Works:
#_REMOTE_CMD = ["cat"]

# Works (NOTE: this cmd is python, not jython!)
#_REMOTE_CMD = ["python", "-c", "import sys; sys.stdout.write(sys.stdin.readline()); sys.stdout.flush();"]

# Doesn't work with jython 2.5.3 (but works fine with jython 2.5.2)
_REMOTE_CMD = ["./dist/bin/jython", "-c", "import sys; sys.stdout.write(sys.stdin.readline()); sys.stdout.flush();"]


def _testAll():
  for name in sorted(g_cmds.keys()):
    
    func = g_cmds[name]
    
    if func is not _testAll:
      func()


def _testBlockingViaFile():
  print >> sys.stderr, ("\n"
    "Testing blocking write/read via file API")
  
  p = _createSubprocess(_REMOTE_CMD)
  
  dataIn = "f"*55 + "\n"
  p.stdin.write(dataIn)
  p.stdin.flush()
  
  dataOut = p.stdout.read(len(dataIn))
  
  print repr(dataOut)
  
  assert dataOut == dataIn, "%r != %r" % (dataOut, dataIn)
  
  p.stdin.close()
  p.wait()
  
  print "ok"
  return


def _testBlockingViaFD():
  print >> sys.stderr, ("\n"
    "Testing blocking write/read via fd")
  
  p = _createSubprocess(_REMOTE_CMD)
  
  w = p.stdin.fileno()
  r = p.stdout.fileno()
  
  dataIn = "d"*55 + "\n"
  
  os.write(w, dataIn)
  
  dataOut = os.read(r, len(dataIn))
  
  print repr(dataOut)
  
  assert dataOut == dataIn, "%r != %r" % (dataOut, dataIn)
  
  p.stdin.close()
  p.wait()
  
  print "ok"
  return


def _testNonBlockingViaFDPolling():
  print >> sys.stderr, ("\n"
    "Testing NON-BLOCKING write/read via fd/POLLING")
  
  p = _createSubprocess(_REMOTE_CMD)
  
  w = p.stdin.fileno()
  r = p.stdout.fileno()
  
  wOldFlags = _getFCNTLFlags(w)
  print "w-fd initial FCNTL flags:", wOldFlags
  rOldFlags = _getFCNTLFlags(r)
  print "r-fd initial FCNTL flags:", rOldFlags
  
  _applyFCNTLFlags(w, os.O_NONBLOCK)
  wNewFlags = _getFCNTLFlags(w)
  print "w-fd FCNTL flags after w+=O_NONBLOCK:", _getFCNTLFlags(w)
  print "r-fd FCNTL flags after w+=O_NONBLOCK:", _getFCNTLFlags(r)
  assert _getFCNTLFlags(r) == rOldFlags
  
  _applyFCNTLFlags(r, os.O_NONBLOCK)
  print "w-fd FCNTL flags after r+=O_NONBLOCK:", _getFCNTLFlags(w)
  print "r-fd FCNTL flags after r+=O_NONBLOCK:", _getFCNTLFlags(r)
  assert _getFCNTLFlags(w) == wNewFlags
  
  
  dataIn = "P"*55 + "\n"
  
  savedFlags = _applyFCNTLFlags(w, os.O_NONBLOCK)
  try:
    numBytesWritten = os.write(w, dataIn)
  finally:
    _replaceFCNTLFlags(w, savedFlags)
  print "numBytesWritten:", numBytesWritten
  
  dataOut = ""
  while True:
    try:
      dataOut += os.read(r, numBytesWritten)
    except OSError, e:
      if e.errno != errno.EAGAIN:
        raise
      
      if dataOut:
        break
      else:
        continue
  
  numBytesRead = len(dataOut)
  print "numBytesRead: ", numBytesRead
  
  print repr(dataOut)
  
  assert dataOut == dataIn, "%r != %r" % (dataOut, dataIn)
  
  p.stdin.close()
  p.wait()
  
  print "ok"
  return


def _testNonBlockingViaFDSelect():
  print >> sys.stderr, ("\n"
    "Testing NON-BLOCKING write/read via fd/SELECT")
  
  p = _createSubprocess(_REMOTE_CMD)
  
  w = p.stdin.fileno()
  r = p.stdout.fileno()
  
  wOldFlags = _getFCNTLFlags(w)
  print "w-fd initial FCNTL flags:", wOldFlags
  rOldFlags = _getFCNTLFlags(r)
  print "r-fd initial FCNTL flags:", rOldFlags
  
  _applyFCNTLFlags(w, os.O_NONBLOCK)
  wNewFlags = _getFCNTLFlags(w)
  print "w-fd FCNTL flags after w+=O_NONBLOCK:", _getFCNTLFlags(w)
  print "r-fd FCNTL flags after w+=O_NONBLOCK:", _getFCNTLFlags(r)
  assert _getFCNTLFlags(r) == rOldFlags
  
  reader = FDLineReader(r)
  print "w-fd FCNTL flags after FDLineReader(r):", _getFCNTLFlags(w)
  print "r-fd FCNTL flags after FDLineReader(r):", _getFCNTLFlags(r)
  assert _getFCNTLFlags(w) == wNewFlags
  
  
  dataIn = "S"*55 + "\n"
  
  fdWriteString(w, dataIn)
  
  dataOut = reader.readline()
  
  numBytesRead = len(dataOut)
  print "numBytesRead: ", numBytesRead
  
  print repr(dataOut)
  
  assert dataOut == dataIn, "%r != %r" % (dataOut, dataIn)
  
  p.stdin.close()
  p.wait()
  
  print "ok"
  return


def _createSubprocess(args):
  kwargs = dict(
    bufsize=-1,
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None,
    close_fds=True,
    shell=False,
    universal_newlines=False    
  )
  return subprocess.Popen(args, **kwargs)


###############################################################################
class FDLineReader(object):
  """ Read a line of text from a file descriptor up to and including the very
  first '\n', using select.select to wait for input.
  """
  
  def __init__(self, fd):
    """ Construct the reader; configure the given file descriptor for
    non-blocking operation
    
    fd:             The input file-descriptor; WARNING: don't mix use of
                     buffered (e.g., sys.stdin.read()) I/O with
                     file-descriptor-based I/O (e.g.,
                     os.read(sys.stdin.fileno()))
    makeNonBlocking:
                    If True, the given file-descriptor will be configured for
                     non-blocking IO;
                     WARNING: if the fd is from sys.stdin.fileno(),
                     then changing this fd to non-blocking also has the side-
                     effect of making the file desriptor in sys.stdout
                     non-blocking, so print and sys.stdout.write/flush will
                     sometimes fail with "IOError: [Errno 35] Resource
                     temporarily unavailable" (35=errno.EWOULDBLOCK).
    """
    
    fcntl.fcntl(fd, fcntl.F_SETFL,
                fcntl.fcntl(fd, fcntl.F_GETFL) | os.O_NONBLOCK)
    
    self._fd = fd
    
    return
  
  
  def readline(self):
    """ Read a line of text from a file descriptor up to and including the very
    first '\n'
    
    NOTE: Lines are assumed to be terminated with '\n'
    """
    
    fd = self._fd
    
    data = ""
    
    rlist = [fd]
    wlist = xlist = []
    
    while not data.endswith('\n'):
      rready = select.select(rlist, wlist, xlist)[0]
      assert fd in rready
      
      # Read 
      newData = ""
      while newData != '\n':
        try:
          newData = os.read(fd, 1024)
        except (OSError, socket.error), e:
          if e.args[0] != errno.EWOULDBLOCK:
            raise
          
          # EWOULDBLOCK: we'll wait in outer loop's select for more data
          break
        else:
          if not newData:
            # EOF reached
            return data
          
          data += newData
          
    
    return data



###############################################################################
def fdWriteString(fd, string):
  """ Write the given string to a file descriptor, using select.select to wait
  for input.
  """
  if isinstance(string, unicode):
    string = string.encode()
  
  wlist = [fd]
  rlist = xlist = []
  
  bytesToSend = len(string)
  numBytesSent = 0
  
  while numBytesSent < bytesToSend:
    wready = select.select(rlist, wlist, xlist)[1]
    assert fd in wready
    
    originalFdFlags = _applyFCNTLFlags(fd, os.O_NONBLOCK)
    try:
      numBytesSent += os.write(fd, buffer(string, numBytesSent))
    except (OSError, socket.error), e:
      if e.args[0] != errno.EWOULDBLOCK:
        raise
    finally:
      _replaceFCNTLFlags(fd, originalFdFlags)
  
  return



def _getFCNTLFlags(fd):
  return fcntl.fcntl(fd, fcntl.F_GETFL)

def _replaceFCNTLFlags(fd, flags):
  oldFlags = _getFCNTLFlags(fd)
  fcntl.fcntl(fd, fcntl.F_SETFL, flags)
  return oldFlags

def _applyFCNTLFlags(fd, mixInFlags):
  oldFlags = _getFCNTLFlags(fd)
  fcntl.fcntl(fd, fcntl.F_SETFL, oldFlags | mixInFlags)
  
  return oldFlags



g_cmds = {
  "all" : _testAll,
  "cat-block" : _testBlockingViaFile,
  "cat-fdblock" : _testBlockingViaFD,
  "cat-fdpollnoblock" : _testNonBlockingViaFDPolling,
  "cat-fdselectnoblock" : _testNonBlockingViaFDSelect,
}


def main():
  
  
  def argErrorAndHelpAndExit(cmd=None):
    if cmd is not None:
      print "Unknown command: ", cmd
      
    print "Supported commands:", ", ".join(sorted(g_cmds.keys()))
    sys.exit(1)
    
  
  try:
    func = g_cmds[sys.argv[1]]
  except IndexError:
    argErrorAndHelpAndExit()
  except KeyError:
    argErrorAndHelpAndExit(sys.argv[1])

  print "_REMOTE_CMD:", " ".join(_REMOTE_CMD)
  
  func()



if __name__ == '__main__':
  main()
