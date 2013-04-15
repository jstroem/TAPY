OSNAME := ${shell uname}

all: clean compile run

clean:
	rm -rf bin

compile:
	mkdir -p bin
	scalac -classpath lib/*.jar -d bin src/*.scala src/*/*.scala

run:
	@if (test "${OSNAME}" = "CYGWIN_NT-6.2-WOW64"); \
		then scala -cp "bin;./lib/jython-2.7-b1.jar" tapy.Main $(FILES); fi
	@if (test "${OSNAME}" = "Darwin"); \
		then scala -cp "bin:./lib/jython-2.7-b1.jar" tapy.Main $(FILES); fi