(ns matasano.core
  (:require
            [clojure.data.codec.base64 :as b64]
            [clojure.string :as string]
            [clojure.java.io :refer :all]))

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

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def regular-ascii-codes
  (set (map int (concat [\space \']
                        (char-range \A \Z)
                        (char-range \a \z)))))

(defn string->hex [s]
  (to-str (map (comp byte->hex int) s)))

(defn string->byte-seq [s]
  (map char->byte s))

(defn encode-message [key byte-seq]
  (let [key-bytes (cycle (string->byte-seq key))]
    (map bit-xor key-bytes byte-seq)))


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

(defn byte-seq->string [byte-seq]
  (String. (byte-array byte-seq)))

(defn score [s]
  (if (seq s)
    (let [freq (-> (byte-seq->string s)
                   string/lower-case
                   (string/replace #"[^a-z ]" ".")
                   frequencies)
          letters (->> freq
                       (map second)
                       (reduce +))
          scoring (fn [[char quant]]
                    (Math/abs
                      (- (or (letter-frequencies char)
                           0)
                        (/ quant letters))))]
      (->> freq
           (map scoring)
           (reduce +)))
    1))

(defn best-xor [byte-seq]
  (->> (range 256)
       (map #(encode-message [%] byte-seq))
       (apply min-key score)))

(defn read-lines [file]
  (clojure.string/split-lines
    (slurp file)))

(defn best-xor-in-file [file]
  (->> file
       read-lines
       (pmap (comp best-xor hex-string->byte-seq))
       (apply min-key score)))

(defn count-bits [x]
  (loop [c 0
         b x]
    (if (zero? b) c
      (recur (+ c (bit-and 1 b))
             (bit-shift-right b 1)))))

(defn hamming-distance [byte-seq1 byte-seq2]
  (apply + (map (comp count-bits bit-xor) byte-seq1 byte-seq2)))

(defn average [coll]
  (/ (reduce + coll)
     (count coll)))

(defn message-blocks [cipher keysize]
  (->> cipher
       (partition keysize)
       (partition 2 1)))

(defn try-key [cipher keysize]
  (let [cipher-seq (string->byte-seq cipher)
        N 20]
    (->> (message-blocks cipher-seq keysize)
         (map #(-> (apply hamming-distance %) (/ keysize) float))
         (take N)
         average)))

(def cipher
  (->> (string/replace (slurp "resources/challenge06") "\n" "")
       base64->hex
       (apply str)
       (hex-string->byte-seq)))

(defn guess-keysize [cipher min max]
  (->> (range min (inc max))
       (apply min-key (partial try-key cipher))))

(defn transpose [m]
  (apply map vector m))

(defn break-repeating-key-xor [cipher]
  (->> cipher
       (partition (guess-keysize cipher 2 40))
       transpose
       (map best-xor)
       transpose
       (apply concat)
       byte-seq->string))
