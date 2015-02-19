(ns matasano.utils
  (:require [clojure.string :as str]
            [matasano.adapters :as a]))

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

(def block-size 16)

(defn encryption-oracle [encryption-fn]
  (let [fixed-input (byte-array (repeat 80 65))
        output (encryption-fn fixed-input)]
    (if (unique? output block-size)
      "CBC"
      "ECB")))

(defn encryption-oracle2 [block-size encryption-fn]
  (let [fixed-input (byte-array (repeat (* 4 block-size) (int \A)))
        output (encryption-fn fixed-input)]
    (if (unique? output block-size)
      "CBC"
      "ECB")))

(defn average [coll]
  (/ (reduce + coll)
     (count coll)))

(defn message-blocks [cipher keysize]
  (->> cipher
       (partition keysize)
       (partition 2 1)))

(defn transpose [m]
  (apply map vector m))

(defn gen-random-bytes [len]
  (byte-array (repeatedly len #(a/char->byte (rand-int 256)))))
