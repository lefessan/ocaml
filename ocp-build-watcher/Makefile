include ../config/Makefile

FILES= \
    watcherConfig.ml watcherProtocol.ml		\
    watcherServerUtils.ml watcherTypes.ml watcherAPIPrivate.ml		\
    watcherAPI.ml watcherClientAPI.ml watcherDBFormat.ml watcherDB.ml	\
    watcherServer.ml watcherClient.ml \
    watcherConfig.mli watcherProtocol.mli		\
    watcherServerUtils.mli watcherAPIPrivate.mli		\
    watcherAPI.mli watcherDBFormat.mli watcherDB.mli	\
     \
    .depend

ALLFILES= $(FILES) Makefile.watcher

ocp-watcher-client$(EXE): $(ALLFILES)
	$(MAKE) -f Makefile.watcher ocp-watcher-client$(EXE)

ocp-watcher-client.opt$(EXE): $(ALLFILES)
	$(MAKE) -f Makefile.watcher ocp-watcher-client.opt$(EXE)

depend: $(ALLFILES)
	$(MAKE) -f Makefile.watcher depend

clean: $(ALLFILES)
	$(MAKE) -f Makefile.watcher clean
	rm -f *.o
	rm -f $(ALLFILES)

partialclean: $(ALLFILES)
	$(MAKE) -f Makefile.watcher partialclean

Makefile.watcher: ../../ocp-build-watcher/Makefile
	rm -f Makefile.watcher
	ln -s ../../ocp-build-watcher/Makefile Makefile.watcher

$(FILES): %: ../../ocp-build-watcher/%
	rm -f $*
	ln -s ../../ocp-build-watcher/$* $*
