(ns app.core
  (:require [helix.core :refer [defnc $]]
            [helix.hooks :as hooks]
            [helix.dom :as d]
            ["react" :as r]
            ["react-dom/client" :as rdom]
            ["react-codemirror" :as CodeMirror]
            [promesa.core :as p]
            ["highcharts" :as highcharts]
            ["highcharts-react-official$default" :as highchartsReact]
            [lambdaisland.fetch :as fetch]
            ))

(def options
 { :title {:text "Results" } })


(defn app []
(let [[code set-code] (hooks/use-state "")
      [results set-results] (hooks/use-state [])
      send-code (fn [] (p/let [response (fetch/post "http://localhost:3001/code"
                                                    {:accept :json
                                                     :content-type :json
                                                     :body {:code code}
                                                     })
                            ]
                         (if (not (:index ( :code (js->clj (:body response) :keywordize-keys true))))
                      (set-results  (:code (js->clj (:body response) :keywordize-keys true))))
                      ))

                    ]

  (d/div {:class-name "m-auto h-screen bg-slate-500 flex flex-col items-center" }
         (d/div  {:class-name "tile w-1/3"  :height "200px"}
                (d/h1 "Code goes here")
                (d/div { :class-name "py-2"}
                ($ CodeMirror {
                               :options {:line-numbers true}
                               :value code
                               :onChange #(set-code %)
                               } ))
                (d/button {:on-click send-code :class-name "bg-blue-200 rounded-sm px-5 py-2 float-right"} "Send")
                )
         (d/div { :class-name "py-4 w-2/3"}

           ($ highchartsReact {:highcharts highcharts
                               :options (clj->js (merge options {:series (map (fn [d] {:data d}) results)}))})
           )
         )))

(defn ^:export init []
  (defonce root (rdom/createRoot (js/document.getElementById "app")))
  (.render root ($ app)))

(defn ^:dev/after-load re-render
  []
  (init))

