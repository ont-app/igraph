(ns ont-app.igraph.doo
  (:require [doo.runner :refer-macros [doo-tests]]
            [ont-app.igraph.core-test]
            [ont-app.igraph.graph-test]
            ))

(doo-tests
 'ont-app.igraph.core-test
 'ont-app.igraph.graph-test
 )

