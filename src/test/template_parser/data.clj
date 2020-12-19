(ns test.template-parser.data)

;; test-factories <- server lookup

(def testkvm! {:engine "renault"})
(def testtplstr! "{{engine | pipe}}")

(defrecord F1Car [team engine tires nationality])
(defn make-f1team [{:keys [team engine tires nationality] :as opts}]
  (map->F1Car opts))

(def F1TeamModelRedBull (make-f1team {:nationality "at" :team "RedBull" :tires "Pirelli" :engine "Honda"}))
(def F1TeamModelMclaren (make-f1team {:nationality "uk" :team "Mclaren" :tires "Pirelli" :engine "Renault"}))


(def template "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
")

(def templateWithEmptyPipe "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality | pipe}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
")


(def templateWithUpperCasePipe "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality | pipe(.toUpperCase)}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
")

(def templateWithLowerCasePipe "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality | pipe(.toLowerCase)}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
")

(def templateWithSpacedLowerCaseFnPipe "
<h1>{{team}}</h1>
<ul>
    <li>{{nationality | pipe (.toLowerCase)}}</li>
    <li>{{engine}}</li>
    <li>{{tires}}</li>
</ul>
")
;;end test-factories
