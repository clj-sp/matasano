(ns matasano.scores
  (:require [clojure.string :as string]
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
