(ns hello.core
  (require [clojure.string :as str])
  (:gen-class))

(defn -main [& args]
  (println "Hello"))

(loop [i 0]
  (when (< i 5)
    (println i)
    (recur (inc i))))


(defn templateParser [tpl env]
  (loop [tpl tpl
         env env]
    (cond (empty? env)
          tpl
          :else
          (let [[key value] (first env)]
            (recur (try (str/replace tpl
                                     (re-pattern (str "\\{\\{" (name key) "\\}\\}"))
                                     value)
                        (catch Exception e tpl))
                   (rest env))))))


(def template "
<h1>{{team}}</h2> 
<ul>
    <li>{{engine}}</li>
    <li>{{tyre}}</li>
    <li>{{oil}}</li>
</ul>
")


;;define F1Car record
(defrecord F1Car [team engine tyre oil])

;;build the constructor distructing a single map with options
(defn make-f1team [f1-team f1-engine {:keys [f1-tyre f1-oil] :as opts}]
  (let [{:keys [tyre oil]} opts]
    (map->F1Car {:team f1-team
                 :engine f1-engine
                 :tyre f1-tyre
                 :oil f1-oil})))



;;create a record
(def mclaren (make-f1team "Mclaren" "Renault" {:f1-tyre "Pirelli" :f1-oil "Castrol"}))
mclaren
(println (templateParser template mclaren))
