(ns ont-app.igraph.browser
  (:require [doo.runner :refer-macros [doo-tests]]
            [ont-app.igraph.graph-test]))

(doo-tests 'ont-app.igraph.graph-test)
