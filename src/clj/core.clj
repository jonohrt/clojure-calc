(ns backend.core
  (:require [ring.adapter.jetty :as ring-jetty]
            [reitit.ring :as ring]
            [ring.util.response :as r]
            [muuntaja.core :as m]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [clojure.java.io :as io]
            [instaparse.core :as insta]
            [clojure.core.match :refer [ match ]]))

(defn index [req]
  {:body (slurp (io/resource "public/index.html")) :status 200}
  )

(defn run-code [req]
  (let [code (get-in req [:body-params :code])]
    {:body {:code  (run code)} :status 200}
    ))

(def app
  (ring/ring-handler
   (ring/router [["/" {:get index} ]
                 ["/assets/*" (ring/create-resource-handler {:root "public/assets"})]
                 ["/code" {:post run-code}]
                 ]
                {:data {:muuntaja m/instance
                        :middleware [muuntaja/format-middleware]}})))
(defn start []
  (ring-jetty/run-jetty #'app {:port  3001 :join? false}))

( def server (start))

(.stop server)

(def data
  {:timestamps [
                "2021-01-01"
                "2022-01-02"
                "2022-01-03"
                "2022-01-04"
                "2022-01-05"
                "2022-01-06"
                ]
   :values { :banana [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ]
            :apple [10 5 80 20 0 25 10 5 80 20 0 25 10 5 80 20 0 25 10 5 80 20 0 25 43]}
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
        "
        expr = par | add | mult | number | variable | equal | ident | func | comma
        func = #'[a-zA-Z]+' <'('> expr <')'>
        par = <'('> expr <')'>
        mult = expr ( ('*' | '/') expr)
        add = expr (('+' | '-' ) expr)
        ident = #'[a-zA-Z]+(?= \\=)'
        variable = #'[a-zA-Z]+'
        number = #'-?[0-9]+'
        equal = expr (('=') expr)
        comma = expr((<','>) expr)"
        :auto-whitespace WS
        ))


(defn parse[input]
  (->> (parser input)
       (insta/transform transform-options)))

(defn set-var [e1, e2]
    (swap! vars assoc (keyword e1) e2)
  nil
  )

(defn get-var [v]
  (println ( (keyword v) @vars))
  ( if (( keyword v) ( :values data ))
                            (( keyword v) ( :values data ))
                            (if ((keyword v) @vars)
                              ((keyword v) @vars)
                              (eval-expr v)
                              )
                            )
  )

(def method {
             :map  (fn [data] ( mapv (fn [_] ( apply max data)) data) )
             })
(defn try-me [f data]

((:map method) data))

(defn eval-expr [expr]
  ( match expr
          [:equal e1 "=" e2] (set-var  (eval-expr e1 ) (eval-expr e2))
          [:func e1 e2] (try-me e1 (eval-expr e2))
          [:par e1] (eval-expr e1)
          [:mult e1 "*" e2] (arithmatic '* (eval-expr e1) (eval-expr e2))
          [:mult e1 "/" e2] (arithmatic '/ (eval-expr e1) (eval-expr e2))
          [:add e1 "+" e2] (arithmatic '+ (eval-expr e1) (eval-expr e2))
          [:add e1 "-" e2] (arithmatic '- (eval-expr e1) (eval-expr e2))
          [:number e1] (eval-expr e1)
          [:ident e1] (eval-expr e1)
          [:variable e1] (get-var e1)
          [:comma e1 e2] [(eval-expr e1) (eval-expr e2)]
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

(defn nest [result]
  (if (vector? (first  result))
    result

    [result]
    )
  )


(defn run [code]
  ( ->> code
    clojure.string/split-lines
    (mapv parse)
    (mapv eval-expr)
    (last)
    (nest)
    ))

(comment
  (run "grape = banana "
       )

( ->> "max(banana)"
      parse
      eval-expr
      )

(->> "grape = max(banana) + 2
     cherry = (grape * 2)
     banana * apple * grape + cherry"
  clojure.string/split-lines
  (mapv parse)
  (mapv eval-expr)
  last
  )
(->> "grape = max(banana)
     grape", banana
  clojure.string/split-lines
  (mapv parse)
   (mapv eval-expr)
     (last)
  (nest)
    )

  )
