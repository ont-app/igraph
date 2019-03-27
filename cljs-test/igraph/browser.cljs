(ns igraph.browser
  (:require [doo.runner :refer-macros [doo-tests]]
            [igraph.graph-test]))

(doo-tests 'igraph.graph-test)
