SUBDIRS="src"
GENSUBDIRS="bin"

default: all

.phony: all clean configuresrc

configuresrc:
	(cd src; ./configure)

all: configuresrc
	for d in $(SUBDIRS); do \
		(cd $$d ; $(MAKE)) \
	done


clean:
	for d in $(SUBDIRS); do \
		( cd $$d ; $(MAKE) clean ) \
	done
