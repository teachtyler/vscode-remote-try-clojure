(ns test.template-parser.mustache
  (require [clojure.test :refer :all]
           [template-parser.mustache :refer [parser pipe-factory]]
           [test.template-parser.data :as tdata]))

(deftest template-with-model
  (testing "should parse template and replace keys with values from model"
    (is (= "\n<h1>Mclaren</h1>\n<ul>\n    <li>uk</li>\n    <li>Renault</li>\n    <li>Pirelli</li>\n</ul>\n"
           (parser template tdata/F1TeamModelMclaren)))))

(deftest template-with-pipe-and-uppercase-fn
  (testing "should uppercase nationaility"
    (is (= "\n<h1>Mclaren</h1>\n<ul>\n    <li>UK</li>\n    <li>Renault</li>\n    <li>Pirelli</li>\n</ul>\n"
           (parser tdata/templateWithUpperCasePipe tdata/F1TeamModelMclaren)))))

(deftest verify-all-templates-with-model
  (template-with-model)
  (template-with-pipe-and-uppercase-fn))

(verify-all-templates-with-model)

(defn run-pipe-factory-tests []
  (list
   (pipe-factory "renault | pipe (.toUpperCase)") ;; "RENAULT"  
   (pipe-factory "renault | pipe(.toUpperCase)") ;; "RENAULT" 
   (pipe-factory "renault | pipe") ;; "RENAULT"
   (pipe-factory "renault | pipe") ;; nil
   (pipe-factory "renault |") ;; nil
   (pipe-factory "renault") ;; nil
   (pipe-factory "renault"))) ;; nil

(run-pipe-factory-tests)



(parser
 tdata/templateWithUpperCasePipe
 tdata/F1TeamModelMclaren)

(parser
 tdata/template
 tdata/F1TeamModelMclaren)
