(ns matasano.core
  (:require [clojure.string :as str]
            [clojure.data.codec.base64 :as b64]
            [clojure.java.io :refer :all]))

(defn hex->bytes [hex]
  (->> hex
       (re-seq #".{2}")
       (map #(Integer/parseInt % 16))
       byte-array))

(defn bytes->hex [b]
  (->> b
       byte-array
       (map #(format "%02x" %))
       (apply str)))

(defn b64->bytes [b64]
  (-> b64
      .getBytes
      b64/decode))

(defn bytes->b64 [b]
  (-> b
      byte-array
      b64/encode
      String.))

(defn bytes->str [b]
  (-> b
      byte-array
      String.))

(defn str->bytes [s]
  (-> s
      .getBytes
      byte-array))

(defn encode-base64 [hex]
  (-> hex
      hex->bytes
      bytes->b64))

(defn decode-base64 [b64]
  (->> b64
       b64->bytes
       bytes->hex))

(defn xor [ba1 ba2]
  (map bit-xor ba1 ba2))

(defn hex-xor [hex-1 hex-2]
  (bytes->hex
    (xor (hex->bytes hex-1) (hex->bytes hex-2))))

(def letter-frequencies
  "From http://www.cl.cam.ac.uk/~mgk25/lee-essays.pdf"
  {\a 0.0609
   \b 0.0105
   \c 0.0284
   \d 0.0292
   \e 0.1139
   \f 0.0179
   \g 0.0138
   \h 0.0341
   \i 0.0544
   \j 0.0024
   \k 0.0041
   \l 0.0292
   \m 0.0276
   \n 0.0544
   \o 0.0600
   \p 0.0195
   \q 0.0024
   \r 0.0495
   \s 0.0568
   \t 0.0803
   \u 0.0243
   \v 0.0097
   \w 0.0138
   \x 0.0024
   \y 0.0130
   \z 0.0003
   \space 0.1217
   \. 0.0657})

(defn abs [n]
  (if (neg? n)
    (- n)
    n))

(defn score [s]
  (if (seq s)
    (let [freq (-> s
                   str/lower-case
                   (str/replace #"[^a-z ]" ".")
                   frequencies)
          letters (->> freq
                       (map second)
                       (reduce +))
          scoring (fn [[char quant]]
                    (abs
                      (- (or (letter-frequencies char)
                           0)
                        (/ quant letters))))]
      (->> freq
           (map scoring)
           (reduce +)))
    1))

(defn decode [k msg]
  (xor msg (cycle k)))

(def encode decode)

(defn decode-message [k message]
  (->> (decode [k] (hex->bytes message))
       bytes->str))

(defn all-decode-tries [s]
  (for [x (range 256)
        :let [d (decode-message x s)]]
    [(score d) d]))

(defn best-score* [s]
  (apply min-key first
    (all-decode-tries s)))

(defn best-score [s]
  (second (best-score* s)))

(defn best-score-file [f]
  (with-open [rdr (reader f)]
    (->> rdr
         line-seq
         (pmap #(best-score* %))
         (apply min-key first)
         second)))

(defn repeating-key-xor [k msg]
  (bytes->hex
    (decode (str->bytes k) (str->bytes msg))))
