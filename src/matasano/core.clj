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
  (-> #{32}
      (into (range 65 91))
      (into (range 97 123))))

(defn int-seq [string]
  (doall (map int (seq string))))

(defn score [string]
  (count (filter special-ascii-codes (int-seq string))))

(defn decode-message [key message]
  (->>
   (xor message (apply str (repeat (/ (count message) 2) (format "%x" key))))
   hex->byte-array
   (map char)
   (apply str)))

(defn score-pair
  ([str]
   [str (score str)])
  ([key hex-encoded-str]
   (score-pair (decode-message key hex-encoded-str))))

(defn best-xor [hex-encoded-str]
  (first (apply max-key second
                (map #(score-pair % hex-encoded-str) (range 256)))))

(defn read-lines [file]
  (clojure.string/split-lines
    (slurp file)))

(defn calculate-scores [strings]
  (pmap (comp score-pair best-xor) strings))

(defn best-xor-in-file [file]
  (->> file
       read-lines
       #_(take 10)
       calculate-scores
       (apply max-key second)
       first))
