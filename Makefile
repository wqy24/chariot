all: mtr


wqy24/assert.c:
	gsc -:s -module-ref wqy24/assert -c . wqy24/assert.sld

mtr: wqy24/assert.c
	gsc -:s -exe  -ld-options -lsoundio  . wqy24/assert.c mtr.scm


