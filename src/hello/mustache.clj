(ns hello.mustache
  (require [clojure.string :as str]))

;; test-factories <- server lookup
(defrecord F1Car [team engine tires nationality])

(defn make-f1team [{:keys [team engine tires nationality] :as opts}]
  (map->F1Car opts))

(def template "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality | pipe}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
                   ")

(def RedBull (make-f1team {:nationality "usa" :team "RedBull" :tires "Pirelli" :engine "Renault"}))
(def Mclaren (make-f1team {:nationality "uk" :team "Mclaren" :tires "Pirelli" :engine "Renault"}))
(def testkvm! {:engine "renault"})
(def testtplstr! "{{engine | pipe}}")
;;end test-factories

;; (let [v (parse-model-properties :team "Mclaren")] (str/replace "{{team}}" #(true) v))

(def open-expression (re-pattern "\\{\\{"))
(def close-expression (re-pattern "\\}\\}"))
(str/replace "{{name}}" (re-pattern (str open-expression "name" close-expression)) "tyler")

(def open-pipe-fn (re-pattern "\\("))
(def close-pipe-fn (re-pattern "\\)"))
(def pipe-pattern-symbol (re-pattern "\\|"))
(def pipe-pattern (re-pattern "pipe"))

(defn parse-pipe-fn [template]
  (last
   (-> template ;;"renault | pipe(.toUpperCase)"
       (str/replace pipe-pattern-symbol "")
       (str/replace close-pipe-fn "")
       (str/replace  pipe-pattern "")
       (str/split #" ")))) ;; "(.toUpperCase"

(defn remove-pipe-fn [template]
  (str/join (drop-last (-> template;;"engine | pipe (.toUpperCase)"
                          ;  (str/replace pipe-pattern-symbol "")
                           (str/split #" ")))))

(defn remove-pipe-pattern [template]
  (str/join (-> template
                (str/replace pipe-pattern "")
                (str/split #" "))))

(parse-pipe-fn "|pipe(.toUpperCase)")
(remove-pipe-fn "engine | pipe (.toUpperCase)")
(remove-pipe-pattern "engine | pipe (.toUpperCase)")

(defn parse-expression [e] (-> e (str/replace open-expression "") (str/replace close-expression "")))

(defn parse-exp [tpl model]
  (let [key (first (keys model))]
    (str/replace tpl (re-pattern (name key)) (get model key))))

(defn parse-exp-filter [a] (-> a (parse-expression)))
(get testkvm! (first (keys testkvm!))) ;; "renault"


(parse-exp-filter testtplstr!) ;; "engine | pipe"
(parse-exp
 "something not related at all but pipe and engine is here"
 testkvm!)
(parse-exp-filter "{{engine | pipe (.toUpperCase)}}")
(str '(parse-exp))



(parse-exp-filter "renault | pipe") ;; "renault"
(defn pipe [s f]
  (let [rgx (re-pattern " \\|\\ pipe")
        st (str/replace s rgx "")]
    (cond (reduce < (map #(count %) [st s])) (f st) :else s)))

(pipe "renault" #(.toUpperCase %)) ;; "renault"
(pipe "renault | pipe" #(.toUpperCase %)) ;; "RENAULT"
(pipe "USA | pipe(.toLowerCase)" #(parse-expression %)) ;; "usa"
(let [s "USA | pipe(.toLowerCase)"] (str (parse-pipe-fn s)))

(defn testfn! [a] (str "(.toLowerCase "))
; (println (eval (read-string (testfn! "A"))))
(testfn! "asd")

(let [a (symbol testfn!)
      f #(a %)]
  (f "A"))


(def abool true)
(cond (true? abool) "?" :else "!") ;;"!"

(reduce < (map #(count %) [2 1]))

(as-> 0 n
  (inc n)
  (if false
    (inc n)
    n))

(as-> "name" n
  (-> n
      (#(map
         #{(get {:tyler (str %)} :tyler)}
         (str %)))))
      ; (#(if
      ;    (str/includes? % "me")
      ;     (str "tyler")))
      ; (str)))

(-> "n" (#(str % "!")))

(cond-> "()"
  #(str/includes? % "(") (str "!")
  #(str/includes? % ")") (str "!!")) ;; ()!!!

(take
 (str/index-of "renault | pipe(.toUpperCase)" " |")
 "renault | pipe(.toUpperCase)")

(defn pipe-factory [a]
  (try
    (-> "{{engine | pipe (.toUpperCase)}}"
        (remove-pipe-pattern)
        (parse-exp-filter)
        (parse-exp testkvm!) ;; "renault|(.toUpperCase)"
        (parse-pipe-fn)
        (str/split open-pipe-fn))

    (map #(%)  ["renault" ".toUpperCase"])
        ; ((fn [b]
        ;    (let [x (first b)
        ;          f (str (second b))]
        ;      (eval (read-string (str `(f x)))))))
        ; ; (str)
        ; (read-string)
        ; (eval))

        ; ((fn [& a] (if (str/includes? a (str "| pipe")) (str a) nil)))
        ; ((fn [a] (if (str/includes? a open-pipe-fn) (str a) nil)))
        ; ((fn [a]
        ;    (cond-> a ;;"{{engine | pipe (.toUpper)}}"
        ;      #(str/includes? % open-pipe-fn) (str/replace open-pipe-fn  "")
        ;      #(str/includes? % close-pipe-fn) (str/replace close-pipe-fn ""))))
        ; (str "?"))

    (catch Exception e a)))


(map #(str %)
     (list
      (pipe-factory "{{engine | pipe (.toUpperCase)}}") ;; "RENAULT"  
      (pipe-factory "{{engine | pipe(.toUpperCase)}}") ;; "RENAULT" 
      (pipe-factory "{{engine | pipe}}") ;; "RENAULT"
      (pipe-factory "{{engine | pipe") ;; nil
      (pipe-factory "{{engine |") ;; nil
      (pipe-factory "{{engine}}") ;; nil
      (pipe-factory "{{engine"))) ;; nil



; ((fn [a] (if (or (str/includes? a "(") (str/includes? a ")"))
        ;            (-> a
        ;                (parse-pipe-fn)
        ;                (#(str "(" % " "))
        ;                (str '(first (vals testkvm!))))
        ;            ) :else (str "???")))
        ; (str))

(str a)
(parse-exp testkvm!) ;; "renault | pipe(.toLowerCase)"

(str/includes? "{{engine | pipe (.toUpperCase)}}" "(")

(pipe-factory)
(pipe-factory 1)



(str `(first (vals testkvm!)) ")")
(read-string)
(eval)


(pipe-factory testtplstr!)
(pipe-factory)



(str/re-quote-replacement `(str))


;; (->> (str/replace) (-> template #(#"name" "tyler") #(#"age" "21")))

(-> template (str/replace #"name" "tyler") (str/replace #"team" "rb") (str/replace #"engine" "fast"))

(defn test-prnt [h & xs] (str h "!!" xs))

(defn compose-str-parser [t k v]
  (-> t
      (parse-exp-filter)
      (parse-exp {k v})
      (pipe #(str %))))

; (defn parser-dictions [t k v]
;   (str/replace t (re-pattern (str "\\{\\{" (name k) "\\}\\}")) v))

;;(-> (read) (eval) (println) (while true))


;;   (str/replace template #{"\\{\\{" k "| pipe\\}\\}"} (str "| " v))

; (defn parser2 [template model {:keys [(keys model)] :as opts}]
;   (map->model opts))

(defn parser [template model]
  (loop [template template
         model model]
    (cond (empty? model) template
          :else
          (let [[key value] (first model)]
            (recur (try
                     (compose-str-parser template key value)
                     (catch Exception e template))
                   (rest model))))))

(println (parser template RedBull))



;; Testing Stuff

;; (defn first-and-then-rest [a b & c]
;;   (let [rest c]
;;     rest))

;; (first-and-then-rest 1 2 3 :4 :5 :6 :tail "hello world" :tail "overwritten")
(map-indexed (merge (seq RedBull) (seq Mclaren)))
;; ;; working double thread
(-> "template"
    (-> .toUpperCase (println) (str "asd") (println) (str "dsa"))
    (-> .toLowerCase (println) (str "asd") (println) (str "dsa")))


(defn mapr [& args]
  (->> args `(str/replace "template" (eval))))

;; (as-> #"template" "value")
; (mapr #"template" "value")

(eval `(str/replace "template" #"template" "value"))


;; (defn calculate* []
;;   (->> (range 10)
;;        (filter odd?)
;;        (map #(* % %))
;;        (reduce +)))
;; (calculate)
;; Bool::True (a,b) -> a
;; Bool::False (a,b) -> b
; (defn pipe [a b] (str/replace a (re-pattern " | pipe") a) b)
; (defn pipe [f s] (let [a ()] (f s)))
; (pipe #(.toUpperCase %) "asd | pipe")
; (def pipe (fn [a] (str/replace "a | pipe" #"\\|\\pipe" a)))


; (-> "engine | pipe"
;     ;; parse {{ }}
;     (parse-exp-filter)
;     ;; grab text inside "engine | pipe"
;     (->> parse-exp {:engine "renault"}
;     ;; parse-exp and return val "renault | pipe"
;     ;; run function of pipe on val and remove "| pipe"
;          (pipe .toUpperCase)))
;     ;; "RENAULT" 

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
;; "template\n42\n?"
;;"!"


; (eval
;  (#(-> `(str/replace "template"  %2)) `#"template" `"avalue");; (clojure.string/replace "template" p2__11918__11919__auto__)
; );; Unable to resolve symbol: p2__11983__11984__auto__ in this context

;; (let []
;;   (-> #"name" (str/replace "tyler")))


;; (->> str/replace "{{name}}" #{open-expression  close-expression})

(let [f #(-> %1)
      g #(-> %2)]
  (-> template (g (g str/replace "name"))))


(->  "X" (str "-") (str 100) (str 99) (str 98) (str 97)) ;; "X-100999897"
(->> "Y" (str "-") (str 100) (str 99) (str 98) (str 97)) ;; "979899100-Y"

(reduce + 1 [1 0
             0 1]) ;; 3

(def sum #(reduce + %))

(def average #(/ (sum %) (count %)))

;;apply a function to a collection
(defn results [coll]
  (map #(% coll) [sum average count]))

(results [10 20 30 40 50 0 0])