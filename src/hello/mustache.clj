(ns hello.mustache
  (require [clojure.string :as str]))

;; test-factories <- server lookup
(defrecord F1Car [team engine tire nationality])

(defn make-f1team [{:keys [team engine tire nationality] :as opts}]
  (map->F1Car opts))

(defonce template "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
                   ")

(defonce RedBull (make-f1team {:nationality "USA" :team "RedBull" :tire "Pirelli" :engine "Renault"}))
(defonce Mclaren (make-f1team {:nationality "UK" :team "Mclaren" :tire "Pirelli" :engine "Renault"}))
;;end test-factories

(map-indexed (merge (seq RedBull) (seq Mclaren)))

;; (let [v (parse-model-properties :team "Mclaren")] (str/replace "{{team}}" #(true) v))
(defonce open-expression "\\{\\{")
(defonce close-expression "\\}\\}")

;; (defn parse-expression [e] (str/replace e #{open-expression}))


(str/replace "{{name}}" (re-pattern (str open-expression "name" close-expression)) "tyler")
;; (->> str/replace "{{name}}" #{open-expression  close-expression})

(let [f #(-> %1)
      g #(-> %2)]
  (-> template (g (g str/replace "name"))))

(defn parse-exp [tpl model]
  (str/replace tpl
               (re-pattern
                (str open-expression
                     (name (first (keys model)))
                     close-expression))
               (get model (first (keys model)))))

(def testkvm! {:engine "renault"})


;; (defn calculate* []
;;   (->> (range 10)
;;        (filter odd?)
;;        (map #(* % %))
;;        (reduce +)))

;; (calculate)

;; Bool::True (a,b) -> a
;; Bool::False (a,b) -> b 
(defn pipe [a b] (str/replace a (re-pattern " | pipe") a) b)

(str/replace "a | b" #"\\| b" "!")

(defn parse-exp-filter [a] a)

(-> "engine"
    (parse-exp {:engine "renault"})
    (parse-exp-filter)
    (pipe "cba"))

; (-> template
;     (parse-exp {:engine "renault" :team "redbull"})
;     (result-exp))

;; (-> template
;;     (str/replace
;;      (parse-exp {:engine "renault"})
;;      (result-exp "Renault"))
;;     (str/replace
;;      (parse-exp {:engine "renault"})
;;      (result-exp "Redbull")))

(-> "template"
    (-> println-str
        (str 42))
    (-> println-str
        (str "?")
        prn
        (str "!")))

(#(-> (prn `(str/replace "template"  %2))) #"template" "avalue")

(defn mapr [& args]
  (->> args `(str/replace "template" (eval))))

;; (as-> #"template" "value")
; (mapr #"template" "value")



(eval `(str/replace "template" #"template" "value"))


;; (let []
;;   (-> #"name" (str/replace "tyler")))

(->  "X" (str "-") (str 100) (str 99) (str 98) (str 97))
(->> "Y" (str "-") (str 100) (str 99) (str 98) (str 97))

(reduce + 1 [1 0
             0 1]) ;; 8

;; (->> (str/replace) (-> template #(#"name" "tyler") #(#"age" "21")))

(-> template (str/replace #"name" "tyler") (str/replace #"team" "rb") (str/replace #"engine" "fast"))

(defn prnt [h & xs] (str h "!!" xs))
;; working double thread
(-> "template"
    (-> .toUpperCase (println) (str "asd") (println) (str "dsa"))
    (-> .toLowerCase (println) (str "asd") (println) (str "dsa")))

(defn compose-str-replacer [t k v]
  (str t k v))

;;(-> (read) (eval) (println) (while true))

(defn parser-dictions [template k v]
  (compose-str-replacer template #{"\\{\\{k\\}\\}"} v))

;;   (str/replace template #{"\\{\\{" k "| pipe\\}\\}"} (str "| " v))

(defn parser [template model]
  (loop [template template
         model model]
    (cond (empty? model) template
          :else
          (let [[key value] (first model)]
            (recur (try
                     (parser-dictions template key value)
                     (catch Exception e template))
                   (rest model))))))

(println (parser template RedBull))


;; (defn first-and-then-rest [a b & c]
;;   (let [rest c]
;;     rest))

;; (first-and-then-rest 1 2 3 :4 :5 :6 :tail "hello world" :tail "overwritten")