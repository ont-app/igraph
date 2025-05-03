
ifneq ("$(wildcard ~/.clojure/Makefile)","")
## Put stuff that references `~/.clojure/deps.edn aiases in ~/.clojure/Makefile...
include ~/.clojure/Makefile
endif

## Testing ....
.PHONY: test-jvm
test-jvm:
	clojure -T:build test

.PHONY: test-js
test-js:
	clojure -M:test-cljs

.PHONY: test-node
test-node:
	npx shadow-cljs compile node-test

.PHONY: clean-all
clean-all:
	clojure -T:build clean :include-caches? true

test-all: clean-all test-jvm test-js test-node

## Publishing...
uberjar:
	clojure -T:build ci

install:
	clojure -T:build install

deploy: uberjar
	clojure -T:build deploy
