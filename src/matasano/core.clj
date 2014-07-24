(ns matasano.core
  (:require [taoensso.nippy :as nippy]
            [clojure.data.codec.base64 :as b64]
            [clojure.java.io :refer :all])
  (:import org.apache.commons.codec.binary.Base64))

(defn hex->byte-array [hex-string]
  (->> hex-string
       (partition 2)
       (map #(Integer/parseInt (apply str %) 16))
       (into-array Character/TYPE)
       (map int)))

(defn encode-base64 [hex-string]
  (->> hex-string
       hex->byte-array
       b64/encode
       String.))

(defn number-seq->hex-string [number-seq]
  (->> number-seq
       (map (fn [x] (format "%x" x)))
       (apply str)))

(defn decode-base64 [input]
  (->> input
       .getBytes
       b64/decode
       number-seq->hex-string))

(defn xor [hex-str-1 hex-str-2]
  (number-seq->hex-string
   (map bit-xor (hex->byte-array hex-str-1) (hex->byte-array hex-str-2))))

; tried "doall" instead of "vec" but didn't work, why?
(def special-ascii-codes
  (vec
    (concat (range 0 32)
            (range 33 65)
            (range 91 97)
            (range 123 256))))

; need to see how frequent you see the characters in special-ascii-codes in the input string
(defn int-seq [string]
  (doall (map int (seq string))))

(defn has-value? [coll value]
  (some #(= value %) coll))

; contains? is not what I thought it was.. "some" is more appropriate
(defn score [string]
  (count (filter #(not (has-value? special-ascii-codes %)) (int-seq string))))

(defn decode-message [key message]
  (->>
   (xor message (apply str (repeat (/ (count message) 2) (format "%x" key))))
   hex->byte-array
   (map char)
   (apply str)))

(defn score-map
  ([str]
    (hash-map str (score str)))
  ([key hex-encoded-str]
    (hash-map (decode-message key hex-encoded-str) (score (decode-message key hex-encoded-str)))))

(defn best-xor [hex-encoded-str]
  (key (apply max-key val
    (into {} (map #(score-map %1 hex-encoded-str) (range 256))))))

(defn best-xor-in-file [file]
  (key (apply max-key val
    (into {} (with-open [rdr (reader file)]
               (doall (map #(score-map %1) (map #(best-xor %1) (line-seq rdr)))))))))
