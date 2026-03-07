all: vectr

bridge.c:
	gsc -:s -o bridge.c -module-ref vectr/bridge -c $(GSC-OPTIONS) . bridge-gambit.sld

assert.c:
	gsc -:s -o assert.c -module-ref wqy24/assert -c $(GSC-OPTIONS) . wqy24/assert.sld

vectr: assert.c bridge.c
	gsc -:s -o vectr -exe -ld-options -lsoundio $(GSC-OPTIONS) . assert.c bridge.c main.scm

clean:
	rm -f *.o
	rm -f bridge.c
        #rm -f assert.c
	rm -f vectr

dev: GSC-OPTIONS+=-debug -warnings
dev: all
