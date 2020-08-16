BIN = deno
SRC_DIR = src/
DIST_DIR = dist/

.PHONY: build clean

all: build

build: $(SRC_DIR)*.lisp
	mkdir -p $(DIST_DIR)
	for f in $^; do \
		./psbuild $${f} > $(DIST_DIR)`basename $${f%%.*}`.js; \
	done

clean:
	rm -rf $(DIST_DIR)
