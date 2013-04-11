# Makefile for deleting

make: 
	compile

run:
	scala

clean:
	rm -Rf *.class

compile:
	scalac src/*.scala -deprecation

all: 
	clean compile run
