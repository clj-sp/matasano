(ns matasano.core
  (:require
            [clojure.data.codec.base64 :as b64]
            [clojure.java.io :refer :all])
  (:import org.apache.commons.codec.binary.Base64))

(defn to-str [seq]
  (apply str seq))

(defn char->byte [x]
  (if (<= (int x) 127)
    (byte x)
    (-> x int (- 255) byte)))

(defn hex-string->byte-seq [hex-string]
  (->> hex-string
       (partition 2)
       (map #(Integer/parseInt (apply str %) 16))
       (map char->byte)))

(defn hex-string->string [hex-string]
  (->> hex-string
       hex-string->byte-seq
       (map char->byte)
       (map char)
       to-str))

(defn hex-string->base64 [hex-string]
  (->> hex-string
       hex-string->byte-seq
       byte-array
       b64/encode
       String.))

(defn byte->hex [b]
  (format "%02x" b))

(defn base64->hex [input]
  (->> input
       .getBytes
       b64/decode
       (map byte->hex)))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def regular-ascii-codes
  (set (concat [\space \']
               (char-range \A \Z)
               (char-range \a \z))))

(defn score [string]
  (count (filter regular-ascii-codes string)))

(defn string->hex [s]
  (to-str (map (comp byte->hex int) s)))

(defn string->byte-seq [s]
  (map char->byte s))

(defn encode-message [key byte-seq]
  (let [long-key (take (count byte-seq) (cycle key))
        key-bytes (string->byte-seq long-key)]
    (map bit-xor key-bytes byte-seq)))

(defn byte-seq->string [byte-seq]
  (String. (byte-array byte-seq)))

(defn best-xor [byte-seq]
  (->> (range 256)
       (map #(byte-seq->string (encode-message (str (char %)) byte-seq)))
       (apply max-key score)))

(defn read-lines [file]
  (clojure.string/split-lines
    (slurp file)))

(defn best-xor-in-file [file]
  (->> file
       read-lines
       (map (comp best-xor hex-string->byte-seq))
       (apply max-key score)))
