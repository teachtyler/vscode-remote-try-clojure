(ns core
  (require [clojure.string :as str]
           [template-parser.mustache :refer [parser]])
  (:gen-class))

(defn mustache-parser [template model]
  (try
    (parser template model)
    (catch Exception e '(parser template model))))

(defn -main [& args]
  (let [template (first args)
        model (second args)]
    (if (and (string? template) (map? model))
      (mustache-parser template model)
      (str "I may be hello.core but this is actually a {{handlebar}} parser [string map]"))))

;; (-main "engine: {{engine | pipe (.toUpperCase)}}" {:engine "renault"})
;; should be "engine: RENAULT"