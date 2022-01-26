(ns scicloj.ml.crf-test
  (:require [clojure.test :refer :all]
            [tech.v3.dataset.modelling :as ds-mod]
            [tech.v3.dataset :as ds]
            [scicloj.metamorph.ml :as ml]
            [scicloj.ml.crf :refer :all]))

(def train-ds
  (-> (ds/->dataset "./test/data/jane-austen-emma-ch1.tsv" {:header-row? false :key-fn keyword})
      (ds-mod/set-inference-target :column-1)))

(deftest train-predict-default
  (let [model      (ml/train train-ds {:model-type :corenlp/crf})
        text-ds    (ds/->dataset {:column-0 ["I like Mr. X and I like  Mr. Y"
                                             "Smith"
                                             "Mrs."
                                             "Weston"]})
        prediction (ml/predict text-ds model)]
    (is (= [["PERS" 7 10]
            ["PERS" 25 30]]
           (first (:ner prediction))))))

(deftest train-predict-defaul-no-ngrams
  (let [model      (ml/train train-ds {:model-type :corenlp/crf
                                       :useNGrams  "false"})
        text-ds    (ds/->dataset {:column-0 ["I like Mr. X and I like  Mr. Y"
                                             "Smith"
                                             "Mrs."
                                             "Weston"]})
        prediction (ml/predict text-ds model)]
    (is (= [["PERS" 7 12]
            ["PERS" 25 30]]
           (first (:ner prediction))))))
