OSNAME := ${shell uname}

all: clean compile run

clean:
	rm -rf bin
	rm -f test/*.gif
	rm -f test/*.ast
	rm -f test/*.dot
	rm -f test/*/*.dot
	rm -f test/*/*.ast
	rm -f test/*/*.gif	
	rm -f test/*/*/*.dot
	rm -f test/*/*/*.ast
	rm -f test/*/*/*.gif

compile:
	mkdir -p bin
	scalac $(COMPILEFLAGS) -classpath lib/*.jar -d bin src/*.scala src/*/*.scala

run:
	@if (test "${OSNAME}" = "CYGWIN_NT-6.2-WOW64"); \
		then scala -cp "bin;./lib/jython-2.7-b1.jar" tapy.Main $(FILES); fi
	@if (test "${OSNAME}" = "Darwin"); \
		then scala -cp "bin:./lib/jython-2.7-b1.jar" tapy.Main $(FILES); fi
	rm -f test/*.dot
	rm -f test/*/*.dot	
	rm -f test/*/*/*.dot
