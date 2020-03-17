(ns buchberger.core
    (:require 
      [reagent.core :as reagent :refer [atom]]
      [instaparse.core :as insta :refer-macros [defparser]]
      [clojure.pprint :as pp]
      [clojure.reader :as cljr]
      [clojure.core.match :refer-macros [match]]
      [clojure.string :as string]
    )
)

(enable-console-print!)

(println "This text is printed from src/buchberger/core.cljs. Go ahead and edit it and see reloading in action.")

(defn echo [msg x]
  (do
    (println msg x)
    x
  )
)

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

(defn get-num-val [nparts] (cljr/read-string (echo "str to read" (string/join nparts))))

(defn v-at-n [n v]
  (if (<= n 1) ;; don't allow x_n for n<1
    (list v)
    (conj (v-at-n (- n 1) v) 0)
  )
)

(defn sympowervec-to-list [spvec]
  (let [spext (conj spvec "1")]
    (v-at-n (get-num-val ((spext 1) 1)) (get-num-val (spext 2)))
  )
)

(defn sum-lists [l1 l2]
  (if (< (count (echo "sum-lists l1" l1)) (count (echo "sum-lists l2" l2)))
    (sum-lists l2 l1)
    (map (fn [x1 i] (+ x1 (nth l2 i 0))) l1 (iterate inc 0))
  )
)

(defn faclist-to-powlist [faclist]
  (reduce sum-lists (echo "reduce sl coll" (map sympowervec-to-list faclist)))
)

(defn condense-monomial [mv]
  (let [factors (rest mv)] 
    (match [(first factors)]
     [[:number & numval]] (list (echo "gnv" (get-num-val numval)) 
                                (faclist-to-powlist (rest factors)))
     [[:sympower & _]] (list 1 (faclist-to-powlist factors))
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
                        1
                        (if (< (first l1) (first l2))
                          -1
                          (cmp-lists (rest l1) (rest l2))
                        )
                      )
                    )
                  )
       ]
    (if (> s1 s2)
      -1
      (if (< s1 s2)
        1
        (if (> n1 n2)
          1
          (if (< n1 n2)
            -1
            (cmp-lists (reverse l1) (reverse l2))
          )
        )
      )
    )
  )
)

(defn cmp-lex [l1 l2]
  (match [(empty? l1) (empty? l2)]
   [true true] 0
   [true false] -1
   [false true] 1
   [false false] (cond
                   (> (first l1) (first l2)) 1
                   (< (first l1) (first l2)) -1
                   :else (cmp-lex (rest l1) (rest l2))
                 )
  )
)


(defn combine-identical-monomials [pl]
  (let [add (fn [pl, m]
              (let [m0 (first pl)]
                (if (and m0 (= (second m0) (second m))) ;; if monomials exist and have same multidegree
                  (match [(+ (first m0) (first m))]
                   [0] (rest pl)
                   [csum] (conj (rest pl) (list csum (second m)))
                  )
                  (conj pl m)
                )
              )
            )
       ]
    (reverse (reduce add '() pl)) ;; piping a list through reduce and conj reverses it, so undo that
  )
)

(defn polyvec-to-list [pv]
  (combine-identical-monomials
    (sort-by (fn [x] (nth x 1)) cmp-grevlex (map condense-monomial (rest pv)))
  )
)

(defn varpow-to-string [vnum pow]
  (match [(echo "vpts pow" pow)]
   [0] ""
   [1] (str "x<sub>" vnum "</sub>")
   [_] (str "x<sub>" vnum "</sub><sup>" pow "</sup>")
  )
)

(defn monomial-to-string [ml]
  (apply str (conj (map varpow-to-string (iterate inc 1) (second ml))
                   (first ml)))
)

(defn polylist-to-string [pl]
  (string/join " + " (map monomial-to-string (echo "poly to convert" pl)))
)

(defn lcmd [poly] ;; leading-coefficient-multidegree
  (second (first poly))
)

(defn cmp-poly-grevlex [p1 p2]
  (let [cv (cmp-grevlex (lcmd p1) (lcmd p2))]
    (if (zero? cv)
      (compare p1 p2)
      cv
    )
  )
)

(defn monomial-mul [m1 m2]
  (list (* (first m1) (first m2)) (sum-lists (second m1) (second m2)))
)

(defn negate [m]
  (conj (rest m) (- (first m)))
)

(defn poly-times-monomial [poly m]
  (map #(monomial-mul (echo "ptm m" m) %) (echo "ptm p" poly))
)

(defn poly-sum [p1 p2]
  (combine-identical-monomials (sort-by second cmp-grevlex (concat p1 p2)))
)

(defn multideg-pdm [d1 d2] ;; positive difference from maximum of multidegrees
  (let [mdconj (fn [md exp] (if (and (empty? md) (= 0 exp)) md (conj md exp)))
        conj-two (fn [mds exps] (list (mdconj (first mds) (first exps))
                                      (mdconj (second mds) (second exps))))
        pdm (fn [c1 c2] (let [cm (max c1 c2)] (list (- cm c1) (- cm c2))))
        fdeg #(nth % 0 0)]
    (if (> (+ (count d1) (count d2)) 0)
      (conj-two (multideg-pdm (rest d1) (rest d2)) (pdm (fdeg d1) (fdeg d2)))
      '(() ())
    )
  )
)

(defn multideg-quot [md dd]
  (if (> (count dd) (count md))
    nil
    (if (= 0 (count dd))
      md
      (let [qi (- (first md) (first dd))]
        (if (< qi 0)
          nil
          (when-let [qv (multideg-quot (rest md) (rest dd))]
            (if (or (> qi 0) (> (count qv) 0))
              (conj qv qi)
              qv
            )
          )
        )
      )
    )
  )
)

(defn monomial-cancel-factors [m1 m2]
  (let [[m1c m2c] (multideg-pdm (second m1) (second m2))]
    (list (list (/ (first m1)) m1c) (list (- (/ (first m2))) m2c))
  )
)

(defn monomial-quot [m d]
  (when-let [q (echo "multideg-quot" (multideg-quot (second (echo "m" m)) (second (echo "d" d))))]
    (list (/ (first m) (first d)) q)
  )
)

(defn next-poly-quo [poly dsors n]
  (if-let [di (first dsors)]
    (if-let [qnew (monomial-quot (first poly) (first di))]
      (list n qnew)
      (next-poly-quo poly (rest dsors) (+ 1 n))
    )
    (list -1 nil)
  )
)

(defn gen-poly-div-quos-rdr [poly dsors quos rdr]
  (if (= 0 (count poly))
    (list quos (combine-identical-monomials (sort-by second cmp-grevlex rdr)))
    (let [[ix qnew] (next-poly-quo poly dsors 0)]
      (if (= -1 ix)
        (gen-poly-div-quos-rdr (rest poly) dsors quos (conj rdr (first poly)))
        (gen-poly-div-quos-rdr 
          (poly-sum poly (poly-times-monomial (nth dsors ix) (negate qnew)))
          dsors
          (concat (take ix  quos) (list (conj (nth quos ix) qnew)) (drop (+ 1 ix) quos))
          rdr
        )
      )
    )
  )
)

(defn gen-poly-div [poly dsors]
  (gen-poly-div-quos-rdr (echo "gpd dividend" poly) dsors (repeat (count dsors) '()) '())
)

(defn cancel-lt [p1 p2]
  (apply poly-sum (map poly-times-monomial (list (echo "cancel-lt p1" p1) (echo "cancel-lt p2" p2)) 
                       (monomial-cancel-factors (first p1) (first p2))))
)

(defn buchberger-expand-basis [sys i j]
  (if (>= i (count sys))
    sys
    (if (>= j (min (count sys) 9))
      (buchberger-expand-basis sys (+ i 1) (+ i 2))
      (let [ps (seq sys)
            p (second (gen-poly-div (cancel-lt (nth ps i) (nth ps j)) ps))]
        (if (empty? p)
          (buchberger-expand-basis sys i (+ j 1))
          (buchberger-expand-basis (conj sys p) i (+ j 1))
        )
      )
    )
  )
)

(defn buchberger [sys]
  (buchberger-expand-basis (echo "sys" sys) 0 1)
)

(defn prune-grobner-basis-at [sys n]
  (if (< n 0)
    sys
    (let [p (nth (seq sys) n)
          quots (map #(multideg-quot (lcmd p) (lcmd %)) (drop (+ n 1) sys))]
      (if (some identity quots) ;; if any p' in sys has an LT that divides LT(p)
        (prune-grobner-basis-at (disj sys p) (- n 1))
        (prune-grobner-basis-at sys (- n 1))
      )
    )
  )
)
        
(defn minimize-grobner-basis [sys]
  (prune-grobner-basis-at sys (- (count sys) 1))
)

(defn reduce-grobner-basis [sys]
  (apply sorted-set-by cmp-poly-grevlex
         (for [p sys] (second (gen-poly-div p (disj sys p)))))
)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:title "Buchberger's Algorithm Calculator"
                          :poly ""
                          :system (sorted-set-by cmp-poly-grevlex)
                          :result ""}))

(defn title []
  [:div {:class-name "title"}
   [:h1 (:title @app-state)]
  ]
)

(defn more-polys [] 
  (do
    (echo "test" (:result @app-state))
    (swap! app-state update-in [:system] 
           #(conj % (polyvec-to-list (parse-poly (:poly @app-state)))))
    (swap! app-state assoc :poly "")
    (swap! app-state assoc :result 
           (string/join "<br/>" (map polylist-to-string (:system @app-state))))
    (echo "test2" (:result @app-state))
  )
)

(defn do-calc [] 
  (if (>= (count (:system @app-state)) 2) 
    (println (gen-poly-div (first (:system @app-state)) (rest (:system @app-state))))
    (println (polylist-to-string (polyvec-to-list (parse-poly (:poly @app-state)))))
  )
)

(defn clear-system []
  (do
    (swap! app-state assoc :system (sorted-set-by cmp-poly-grevlex))
    (swap! app-state assoc :result "")
  )
)

(defn readpoly []
  [:div {:class-name "poly-inputs"}
   [:h3 "Polynomial generators x<sub>1</sub>"]
   [:p {:dangerouslySetInnerHTML {:__html "test x<sub>1</sub>"}}]
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
   [:button {:on-click clear-system} "Clear"]
  ]
)
  
(defn result []
  [:div {:class-name "result"}
   (if (not= (:result @app-state) "")
     [:h3 "Result"]
   )
   [:p {:dangerouslySetInnerHTML {:__html (:result @app-state)}}]
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
