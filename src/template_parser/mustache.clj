(ns template-parser.mustache
  (require [clojure.string :as str]))

(def open-expression (re-pattern "\\{\\{"))
(def close-expression (re-pattern "\\}\\}"))

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
  (str/join (drop-last (str/split template #" "))))
                        ;;"engine | pipe (.toUpperCase)"

(defn remove-pipe-pattern [template]
  (str/join " " (-> template
                    (str/replace pipe-pattern-symbol "")
                    (str/replace pipe-pattern "")
                    (str/split #" "))))


(defn remove-expression [e] (-> e (str/replace open-expression "") (str/replace close-expression "")))

(defn parse-exp [tpl model]
  (let [key (first (keys model))]
    (str/replace tpl (re-pattern (name key)) (get model key))))

(defn parse-exp-filter [a] a)

;; (defn pipe [s f]
;;   (let [rgx (re-pattern " \\|\\ pipe")
;;         st (str/replace s rgx "")]
;;     (cond (reduce < (map #(count %) [st s])) (f st) :else s)))
(defn pipe [s f]  (f s))

(defn parse-model [a]
  (str/join
   (take (str/index-of a " |") a)))

(let [s "asdasd{{engine | pipe (.toupper)"]
  (take-while #(str/includes? % "{{") s))

(defn build-pipefn-exp [s]
  ;; (let [s "renault | pipe (.toUpperCase)"]
  (-> s
      (remove-pipe-pattern)
      (parse-pipe-fn)
      (str \" (parse-model s) \" "))")
      (read-string))) ;; (.toUpperCase "renault")

(let [s "\n<h1>{{team}}</h1>\n<ul>\n<li>{{usa(.toUpperCase)}}</li>\n<li>{{engine}}</li>\n<li>{{tires}}</li>\n</ul>\n"]
  (str/index-of s "|"))

(defn pipe-factory [a]
  (try
    (cond-> a
      (str/includes? a "(") (build-pipefn-exp)
      (str/includes? a "(") (eval))
    (catch Exception e a)))

(defn compose-str-parser [t k v]
  (-> t
      (parse-exp-filter)
      (remove-pipe-pattern)
      (parse-exp {k v})))

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

