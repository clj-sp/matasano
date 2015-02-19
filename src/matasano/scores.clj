(ns matasano.scores
  (:require [clojure.string :as string]
            [matasano.utils :refer :all]
            [matasano.adapters :as a]))


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

(defn score [s]
  (if (seq s)
    (let [freq (-> (a/byte-seq->string s)
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
       (map #(a/encode-message [%] byte-seq))
       (apply min-key score)))


(defn best-xor-in-file [file]
  (->> file
       read-lines
       (pmap (comp best-xor a/hex-string->byte-seq))
       (apply min-key score)))

(defn hamming-distance [byte-seq1 byte-seq2]
  (apply + (map #(Integer/bitCount (bit-xor %1 %2)) byte-seq1 byte-seq2)))


(defn try-key [cipher keysize]
  (let [cipher-seq (a/string->byte-seq cipher)
        N 20]
    (->> (message-blocks cipher-seq keysize)
         (map #(-> (apply hamming-distance %) (/ keysize) float))
         (take N)
         average)))

(defn guess-keysize [cipher min max]
  (->> (range min (inc max))
       (apply min-key (partial try-key cipher))))

(defn break-repeating-key-xor [cipher]
  (->> cipher
       (partition (guess-keysize cipher 2 40))
       transpose
       (map best-xor)
       transpose
       (apply concat)
       a/byte-seq->string))
