BIN_MIN = terser
SRC_DIR = src/
DIST_DIR = dist/

ORIGINAL_MODULES = $(filter-out $(wildcard ${DIST_DIR}*.min.js), $(wildcard ${DIST_DIR}*.js))

.PHONY: build min clean

all: build min

build: $(SRC_DIR)*.lisp
	mkdir -p $(DIST_DIR)
	for f in $^; do \
		./psbuild $${f} > $(DIST_DIR)`basename $${f%%.*}`.js; \
	done

min:
	for f in $(ORIGINAL_MODULES); do \
		${BIN_MIN} -cm toplevel < $${f} > $(DIST_DIR)`basename $${f%%.*}`.min.js; \
	done

clean:
	rm -rf $(DIST_DIR)
