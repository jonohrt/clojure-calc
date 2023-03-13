(ns example.core
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [ match ]]))

(def data
  {:timestamps [
                "2021-01-01"
                "2022-01-02"
                "2022-01-03"
                "2022-01-04"
                "2022-01-05"
                "2022-01-06"
                ]
   :values { :banana [1 2 3 4 5 6]
            :apple [10 20 30 40 50 60]}
   }
  )
(def vars (atom {}))

(def WS
  (insta/parser
    "WS = #'\\s+'"))


(def transform-options
  {:number read-string})

(def parser
    (insta/parser
        "expr = add | mult | number | variable | equal
        add = expr (('+' | '-' ) expr)
        mult = (number|variable) ( ('*' | '/') (variable|mult|number) )
        variable = #'[a-zA-Z]+'
        number = #'-?[0-9]+'
        equal = expr (('=') expr)"
        :auto-whitespace WS
        ))

(defn parse[input]
  (->> (parser input)
       (insta/transform transform-options)))

(defn set-var [e1, e2]
    (swap! vars assoc (keyword e1) e2)
  )

(defn get-var [e1]
  ( if (( keyword e1) ( :values data ))
                            (( keyword e1) ( :values data ))
                            (if ((keyword e1) @vars)
                              ((keyword e1) @vars)
                              (eval-expr e1)
                              )
                            )
  )

(defn eval-expr [expr]
  ( match expr
          [:equal e1 "=" e2] (set-var (eval-expr e1) (eval-expr e2))
          [:mult e1 "*" e2] (arithmatic '* (eval-expr e1) (eval-expr e2))
          [:mult e1 "/" e2] (arithmatic '/ (eval-expr e1) (eval-expr e2))
          [:add e1 "+" e2] (arithmatic '+ (eval-expr e1) (eval-expr e2))
          [:add e1 "-" e2] (arithmatic '- (eval-expr e1) (eval-expr e2))
          [:number e1] (eval-expr e1)
          [:variable e1] (get-var e1)
          [:expr e1] (eval-expr e1)
          :else expr
          ))


(defn arithmatic [sym e1 e2]
  (match [ e1 e2]
         [[_ & r1] [_ & r2]] (mapv (eval sym) e1 e2)
         [[_ & r] _] (mapv #((eval sym) % e2) e1 )
         [_ [_ & r]] (mapv #((eval sym) % e1) e2)
         [_ _] ((eval sym) e1 e2)
         :else (println "END")
         ))


(->> "grape = banana + 2
     cherry = grape + grape * grape
     banana * apple * grape + cherry"
  clojure.string/split-lines
  (mapv parse)
  (mapv eval-expr)
  last
  )

