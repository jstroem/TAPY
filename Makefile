clean:
	rm -r bin

compile:
	mkdir bin
	scalac -classpath lib/*.jar -d bin src/*.scala

run:
	scala -cp "bin:./lib/jython-2.7-b1.jar" Main
