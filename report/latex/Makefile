OSNAME := ${shell uname}

all: remove compile show clean

remove:
	rm -rf *.pdf

clean:
	rm -rf *.aux *.log *.out

compile:
	pdflatex -shell-escape Main.tex
	pdflatex -shell-escape Main.tex

show:
	@if (test "${OSNAME}" = "CYGWIN_NT-6.2-WOW64"); \
		then open Main.pdf; fi
	@if (test "${OSNAME}" = "Darwin"); \
		then open Main.pdf; fi
