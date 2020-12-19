(ns calva-repl
  (require [test.template-parser.data :refer :all]
           [template-parser.mustache :refer :all]
           [clojure.string :as str]))

(get testkvm! (first (keys testkvm!))) ;; "renault"

(parse-exp-filter testtplstr!) ;; "engine | pipe"
(parse-exp
 "something not related at all but pipe and engine is here"
 testkvm!)
(parse-exp-filter "{{engine | pipe (.toUpperCase)}}")
(str '(parse-exp))

(parse-exp-filter "renault | pipe") ;; "renault"

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
; 


(-> "n" (#(str % "!")))

(cond-> "()"
  #(str/includes? % "(") (str "!")
  #(str/includes? % ")") (str "!!")) ;; ()!!!


(build-pipe-exp "engine | pipe (.toUpperCase)")


(parse-pipe-fn "engine | pipe(.toUpperCase)")

(remove-pipe-fn "engine | pipe (.toUpperCase)")

(remove-pipe-pattern)

(remove-pipe-fn "engine (.toUpperCase)")
(parse-pipe-fn "engine | pipe (.toUpperCase)")
(remove-pipe-fn "engine | pipe (.toUpperCase)")

(remove-pipe-fn "(.toUpperCase")
(remove-pipe-fn "engine|pipe")
(remove-pipe-fn "engine | pipe")
(remove-pipe-fn "engine|")
(remove-pipe-pattern "engine | pipe")
(remove-pipe-fn "engine")
(parse-exp "engine" testkvm!)
(parse-pipe-fn "engine | pipe ")



(let [a "engine | pipe"]
  (cond-> a
    #(str/includes? % "| pipe") (str/replace #"\| pipe" "")
    #(str/includes? % "| pipe") (str "nice")))

(let [a "engine pipe"]
  (cond-> a
    #(or
      (str/includes? % "|")
      (str/includes? % "pipe")) (str "1")
    (str/includes? a "| pipe") (str "2")))

(let [a 4]
  (cond-> a
    (string? a) (str "A")
    (integer? a) (str))) ;; "4"

(str/includes? "engine | pipe" "| pipe")
(str/includes? "engine | pipe" "pipe (")
(str/includes? "engine | pipe" "pipe(")

(defn pipe-factory [a]
  (try
    (cond-> a
      (str/includes? a "(")       (build-pipe-exp)
      (not (str/includes? a "(")) (remove-pipe-pattern))
    (catch Exception e a)))

(run-pipe-factory-tests)

(defn run-pipe-factory-tests []
  (map #(str %)
       (list
        (pipe-factory "engine | pipe (.toUpperCase)") ;; "RENAULT"  
        (pipe-factory "engine | pipe(.toUpperCase)") ;; "RENAULT" 
        (pipe-factory "engine | pipe") ;; "RENAULT"
        (pipe-factory "engine | pipe") ;; nil
        (pipe-factory "engine |") ;; nil
        (pipe-factory "engine") ;; nil
        (pipe-factory "engine")))) ;; nil


    ; (str "engine | pipe (.toUpperCase)")
        ; (map #(%)  ["renault" ".toUpperCase"])
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


; ((fn [a] (if (or (str/includes? a "(") (str/includes? a ")"))
        ;            (-> a
        ;                (parse-pipe-fn)
        ;                (#(str "(" % " "))
        ;                (str '(first (vals testkvm!))))
        ;            ) :else (str "???")))
        ; (str))


;; (->> (str/replace) (-> template #(#"name" "tyler") #(#"age" "21")))

(-> template (str/replace #"name" "tyler") (str/replace #"team" "rb") (str/replace #"engine" "fast"))

(defn test-prnt [h & xs] (str h "!!" xs))


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


(loop [i 0]
  (when (< i 5)
    (println i)
    (recur (inc i))))


;; ;define F1Car record
;; (defrecord F1Car [team engine tyre oil])

;; ;;build the constructor distructing a single map with options
;; (defn make-f1team [f1-team f1-engine {:keys [f1-tyre f1-oil] :as opts}]
;;   (let [{:keys [tyre oil]} opts]
;;     (map->F1Car {:team f1-team
;;                  :engine f1-engine
;;                  :tyre f1-tyre
;;                  :oil f1-oil})))

;;create a record
;; (def mclaren (make-f1team "Mclaren" "Renault" {:f1-tyre "Pirelli" :f1-oil "Castrol"}))
;; mclaren
;; (println (templateParser template mclaren))


;; (defn templateParser [tpl env]
;;   (loop [tpl tpl
;;          env env]
;;     (cond (empty? env)
;;           tpl
;;           :else
;;           (let [[key value] (first env)]
;;             (recur (try (str/replace tpl
;;                                      (re-pattern (str "\\{\\{" (name key) "\\}\\}"))
;;                                      value)
;;                         (catch Exception e tpl))
;;                    (rest env))))))



;; ;define F1Car record
;; (defrecord F1Car [team engine tyre oil])

;; ;;build the constructor distructing a single map with options
;; (defn make-f1team [f1-team f1-engine {:keys [f1-tyre f1-oil] :as opts}]
;;   (let [{:keys [tyre oil]} opts]
;;     (map->F1Car {:team f1-team
;;                  :engine f1-engine
;;                  :tyre f1-tyre
;;                  :oil f1-oil})))

;;create a record
;; (def mclaren (make-f1team "Mclaren" "Renault" {:f1-tyre "Pirelli" :f1-oil "Castrol"}))
;; mclaren
;; (println (templateParser template mclaren))

(take-while #(>= % 0) [0 1 2 3 4 5]) ;; (0 1 2 3 4 5)
(take-while #(>= % 1) [0 1 2 3 4 5]) ;; ()

(take-while #(str/includes? % "{{") '("dontgrab{{test (fn)}}"))

(def dontgrabmestr "dontgrab{{test (fn)}}dontgrab{{test (fn)}}dontgrab")

(defn between-two-ferns
  "get value of s between ferns"
  [s open close]
  (take-while (str/includes? s close) s))

(-> dontgrabmestr
    (between-two-ferns "{{" "}}")
    (between-two-ferns "((" "))"))

(let [a  '(#(+ 1 %) 1 2 3)
      f   (first a)
      col (rest a)]
  (map f col))
;; (2 3 4)

(map #(+ 1 %) '(1 2))

(defmacro !!- [& args]
  (loop [result ""]
    (cond (empty? args) result
          :else
          (let [f (first args)
                a (rest args)]
            (f (first a))))))

(let [a '(#(+ 1 %) 1 2 3)
      f (first a)
      r (rest a)])


  
'(.toUpperCase "smthn")


(!!- '(#(+ 1 %) 1 2 3))

(defn looper [& args]
  (loop [result args]
    (cond (empty? (count (rest args))) result
          :else
          (let [f (first args)
                a (rest args)]
            (recur (try
                     (f a)
                     (catch Exception e template))
                   (rest args))))))

;; for (const {key, value} in object) { console.log(key, value) }