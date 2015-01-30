(ns matasano.adapters
  (:require
   [clojure.data.codec.base64 :as b64]))

(defn array-xor [a1 a2]
  (byte-array (map bit-xor a1 a2)))

(defn to-str [seq]
  (apply str seq))

(defn char->byte [x]
  (byte
    (if (<= (int x) 127)
      x
      (- (int x) 255))))

(defn hex-string->byte-seq [hex-string]
  (->> hex-string
       (partition 2)
       (map #(char->byte (Integer/parseInt (to-str %) 16)))))

(defn hex-string->string [hex-string]
  (->> hex-string
       hex-string->byte-seq
       (map (comp char char->byte))
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

(defn string->hex [s]
  (to-str (map (comp byte->hex int) s)))

(defn string->byte-seq [s]
  (map char->byte s))

(defn byte-seq->string [byte-seq]
  (String. (byte-array byte-seq)))

(defn encode-message [key byte-seq]
  (let [key-bytes (cycle (string->byte-seq key))]
    (map bit-xor key-bytes byte-seq)))
