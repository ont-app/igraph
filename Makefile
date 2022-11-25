
## Update dependencies as needed....
outdated:
	clojure -M:outdated

upgrade:
	clojure -M:neil deps upgrade

## Testing ....
.PHONY: jvm-test
jvm-test:
	clojure -T:build test

.PHONY: cljs-test
cljs-test:
	clojure -M:test-cljs

.PHONY: node-test
node-test:
	shadow-cljs compile node-test

.PHONY: clean-all
clean-all:
	clojure -T:build clean :include-caches? true

test-all: clean-all jvm-test cljs-test node-test

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
