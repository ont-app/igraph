(ns igraph.core-test
  (:require [clojure.test :refer :all]
            [igraph.core :refer :all]))

(deftest traverse-test
  (testing "Test traverse and transitive-closure"
    (let [g (add (make-graph) [[:a :isa :b] [:b :isa :c][:c :isa :d]])
          isa* (fn [g acc to-visit]
                 [(conj acc (first to-visit))
                  (concat to-visit (g (first to-visit) :isa))])
          ]
      (is (= (traverse g isa* [] [:a])
             [:a :b :c :d]))
      ;; transitive-closure writes equivalent of isa*...
      (is (= (traverse g (transitive-closure :isa) [] [:a])
             [:a :b :c :d])))))
