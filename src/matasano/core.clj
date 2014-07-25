(ns matasano.core
  (:require
            [clojure.data.codec.base64 :as b64]
            [clojure.java.io :refer :all])
  (:import org.apache.commons.codec.binary.Base64))

(defn hex->byte-array [hex-string]
  (->> hex-string
       (partition 2)
       (map #(Integer/parseInt (apply str %) 16))
       (into-array Character/TYPE)))

(defn hex->base64 [hex-string]
  (->> hex-string
       hex->byte-array
       (map byte)
       (into-array Byte/TYPE)
       b64/encode
       String.))

(defn number-seq->hex-string [number-seq]
  (->> number-seq
       (map (fn [x] (format "%x" x)))
       (apply str)))

(defn base64->hex [input]
  (->> input
       .getBytes
       b64/decode
       number-seq->hex-string))

(defn conversion [x]
  (if (<= (int x) 127)
    (byte x)
    (-> x int (- 255) byte)))

(defn xor [hex-str-1 hex-str-2]
  (number-seq->hex-string
   (map #(bit-xor (conversion %1) (conversion %2))
        (hex->byte-array hex-str-1)
        (hex->byte-array hex-str-2))))

(def special-ascii-codes
  (vec
    (concat (range 0 32)
            (range 33 65)
            (range 91 97)
            (range 123 256))))

(defn int-seq [string]
  (doall (map int (seq string))))

(defn has-value? [coll value]
  (some #(= value %) coll))

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
