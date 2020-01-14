(ns buchberger.core
    (:require 
      [reagent.core :as reagent :refer [atom]]
      [instaparse.core :as insta :refer-macros [defparser]]
      [clojure.pprint :as pp]
    )
)

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

(insta/defparser parse-poly
    "polynomial = ws monomial (plus monomial)*
     <ws> = <' '*>
     <plus> = <'+'> ws
     <times> = <(' ' | '*')> ws
     <pow> = <'^'> ws
     monomial = (number | sympower) (times sympower)*
     sympower = symbol (pow natnum ws)?
     symbol = <'x'> natnum ws
     number = '-'? natnum ('.' natnum)? ws
     <natnum> = #'\\d+'
    "
)

(defn get-num-val [& nparts] (read-string (str nparts)))

(defn v-at-n [n v]
  (if (<= n 1) ;; don't allow x_n for n<1
    '(v)
    (conj (v-at-n (- n 1) v) 0)
  )
)

(defn sympowervec-to-list [spvec]
  (let [spext (conj spvec "1")]
    (v-at-n (get-num-val ((spext 1) 1)) (get-num-val (spext 2)))
  )
)

(defn sum-lists [l1 l2]
  (if (< (count l1) (count l2))
    (sum-lists l2 l1)
    (for [x1 l1 i (iterate inc 0)]
      (+ x1 (nth l2 i 0))
    )
  )
)

(defn faclist-to-powlist [faclist]
  (reduce sum-lists (map sympowervec-to-list faclist))
)

(defn condense-monomial [mv]
  (let [factors (rest mv)] 
    (match [(first factors)]
     [[:number & numval]] `((get-num-val numval) (faclist-to-powlist (rest factors)))
     [[:sympower & _]] `(1 (faclist-to-powlist factors))
     [_] `()
    )
  )
)

(defn cmp-grevlex [l1 l2]
  (let [s1 (reduce + l1)
        s2 (reduce + l2)
        n1 (count l1)
        n2 (count l2)
        cmp-lists (fn cmp-lists [l1 l2]
                    (if (empty? l1)
                      0
                      (if (> (first l1) (first l2))
                        -1
                        (if (< (first l1) (first l2))
                          1
                          (cmp-lists (rest l1) (rest l2))
                        )
                      )
                    )
                  )
       ]
    (if (> s1 s2)
      1
      (if (< s1 s2)
        -1
        (if (> n1 n2)
          -1
          (if (< n1 n2)
            1
            (cmp-lists (reverse l1) (reverse l2))
          )
        )
      )
    )
  )
)

(defn polyvec-to-list [pv]
  (sort-by (fn [x] (nth x 1)) cmp-grevlex (map condense-monomial (rest pv)))
)

;; TODO sort polynomials grevlex
(defn do-calc [] (pp/pprint (parse-poly (:poly @app-state))))

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
