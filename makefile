all:
	cmucl-run COMPILE_FILES.lisp


run:
	cmucl-run MAIN.lisp


clean:
	rm -f *~ *.x86f
