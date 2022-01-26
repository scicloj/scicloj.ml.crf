(ns scicloj.ml.crf.util
  "Various utility functions used from the other namespaces, along with
  collections of more or less static data."
  (:require [clojure.string :as str])
  (:import [java.util Properties]
           [edu.stanford.nlp.ling CoreLabel
                                  CoreLabel$OutputFormat
                                  CoreAnnotations$TrueCaseTextAnnotation
                                  CoreAnnotations$PartOfSpeechAnnotation]))


(defn- keys-in
  "Get the nested keys in map `m`."
  [m]
  (let [f (fn [[k v]]
            (let [nested-ks (filter (comp not empty?) (keys-in v))
                  append-ks (fn [path] (into [k] path))
                  kscoll    (map append-ks nested-ks)]
              (if (seq kscoll)
                kscoll
                [[k]])))]
    (if (map? m)
      (vec (mapcat f m))
      [])))

(defn- ks->str
  "Convert `ks` (e.g. from keys-in) to a flattened CoreNLP key."
  [ks]
  (str/join "." (map name ks)))

(defn- flatten-map
  "Flatten a map `m` of nested keys."
  [m]
  (let [kscoll   (keys-in m)
        flat-k+v (fn [ks] [(ks->str ks) (get-in m ks)])]
    (into {} (map flat-k+v kscoll))))

(defn properties
  "Make a Properties object based on a map `m`."
  [m]
  (doto (Properties.)
    (.putAll (flatten-map m))))
