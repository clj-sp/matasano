(ns matasano.utils
  (:require [clojure.string :as str]))

(defn read-lines [file]
  (str/split-lines (slurp file)))


(defn distinct-blocks? [blocks]
  (->> blocks
       (map vec)
       (apply distinct?)))


(defn unique? [b-array block-size]
  (->> b-array
      (partition block-size)
      distinct-blocks?))



(defn padding-pkcs7 [block-size k]
  (let [k-seq (seq k)
        padding-size (- block-size (rem (count k-seq) block-size))
        padding-bytes (repeat padding-size padding-size)]

  (byte-array (concat k-seq padding-bytes))))
