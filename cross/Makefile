
# Build the cross-compilation environment
# We start by creating links in otherlibs/unix because unixsupport.h is
# needed by heapdump_stubs.c in byterun/
all:
	cd otherlibs/unix; $(MAKE) -f Makefile.cross links
	cd tools; $(MAKE) -f Makefile.cross all
	cd byterun; $(MAKE) -f Makefile.cross all
	cd yacc; $(MAKE) -f Makefile.cross all
	cd stdlib; $(MAKE) -f Makefile.cross all
	cd flexdll; $(MAKE) -f Makefile.cross all
	cd otherlibs/unix; $(MAKE) -f Makefile.cross all


clean:
	cd byterun; $(MAKE) -f Makefile.cross clean
	cd yacc; $(MAKE) -f Makefile.cross clean
	cd stdlib; $(MAKE) -f Makefile.cross clean
	cd flexdll; $(MAKE) -f Makefile.cross clean
	cd otherlibs/unix; $(MAKE) -f Makefile.cross clean
	cd ocpwin-nsis; $(MAKE) -f Makefile.cross clean
	cd ocpwin-tool; $(MAKE) -f Makefile.cross clean
	rm -rf local/
	rm -f ocamlpack.cm?
	rm -rf nsis/files
#	rm -rf tests/_obuild
#	find tests -name '*.cm?' -exec rm -f {} \;
#	find tests -name '*.o' -exec rm -f {} \;
#	find tests -name '*.cm??' -exec rm -f {} \;
