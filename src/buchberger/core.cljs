(ns buchberger.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(println "This text is printed from src/buchberger/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:title "Buchberger's Algorithm Calculator"
                          :poly ""
                          :result ""}))

(defn title []
  [:div {:class-name "title"}
   [:h1 (:title @app-state)]
  ]
)

(defn more-polys [] (println "Not implemented (more-polys)!"))

(defn do-calc [] (println "Not implemented (do-calc)!"))

(defn readpoly []
  [:div {:class-name "poly-inputs"}
   [:h3 "Polynomial generators"]
   [:input {:type "text"
            :placeholder "Polynomial, e.g. x1^2 + 3x1x2 + 4"
            :value (:poly @app-state)
            :on-change #(swap!
                          app-state
                          assoc
                          :poly (-> % .-target .-value)
                         )
           }
   ]
   [:button {:on-click more-polys} "More..."]
   [:button {:on-click do-calc} "Calculate!"]
  ]
)

(defn result []
  [:div {:class-name "result"}
   (if (not= (:result @app-state) "")
     [:h3 "Result"]
   )
   (:result @app-state)
  ]
)
  

(defn app []
  [:div {:class "buchberger"}
   [title]
   [readpoly]
   [result]
  ]
)

(reagent/render-component [app]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
