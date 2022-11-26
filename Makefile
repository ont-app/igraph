
## Update dependencies as needed....
outdated:
	clojure -M:outdated

## Testing ....
.PHONY: test-jvm
test-jvm:
	clojure -T:build test

.PHONY: test-js
test-js:
	clojure -M:test-cljs

.PHONY: test-node
test-node:
	shadow-cljs compile node-test

.PHONY: clean-all
clean-all:
	clojure -T:build clean :include-caches? true

test-all: clean-all test-jvm test-js test-node

## Style ....
kondo:
	clojure -M:kondo --lint src

## Generate documentation ...
codox:
	clojure -X:codox

## Publishing...
uberjar:
	clojure -T:build ci

install:
	clojure -T:build install

deploy:
	clojure -T:build deploy
