(ns matasano.core
  (:import [javax.crypto Cipher]
           [javax.crypto SecretKey]
           [javax.crypto.spec SecretKeySpec IvParameterSpec])
  (:require [clojure.data.codec.base64 :as b64]
            [matasano.adapters :as a]
            [matasano.utils :refer :all]
            [matasano.scores :as s]
            [clojure.string :as string]
            [clojure.java.io :refer :all]))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def regular-ascii-codes
  (set (map int (concat [\space \']
                        (char-range \A \Z)
                        (char-range \a \z)))))


(defn best-xor-in-file [file]
  (->> file
       (read-lines)
       (pmap (comp s/best-xor a/hex-string->byte-seq))
       (apply min-key s/score)))

(defn hamming-distance [byte-seq1 byte-seq2]
  (apply + (map #(Integer/bitCount (bit-xor %1 %2)) byte-seq1 byte-seq2)))

(defn average [coll]
  (/ (reduce + coll)
     (count coll)))

(defn message-blocks [cipher keysize]
  (->> cipher
       (partition keysize)
       (partition 2 1)))

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

(defn transpose [m]
  (apply map vector m))

(defn break-repeating-key-xor [cipher]
  (->> cipher
       (partition (guess-keysize cipher 2 40))
       transpose
       (map s/best-xor)
       transpose
       (apply concat)
       a/byte-seq->string))

(defn aes [mode message key]
  (let [cipher (Cipher/getInstance "AES/ECB/NoPadding")
        key (SecretKeySpec. key "AES")]
    (.init cipher mode key)
    (-> cipher
        (.doFinal message))))

(def encrypt-aes (partial aes Cipher/ENCRYPT_MODE))
(def decrypt-aes (partial aes Cipher/DECRYPT_MODE))


(defn distinct-blocks? [blocks]
  (->> blocks
       (map vec)
       (apply distinct?)))


(defn unique? [b-array block-size]
  (->> b-array
      (partition block-size)
      distinct-blocks?))

(defn find-first-duplicated [b-array block-size]
  (let [num-blocks (/ (count b-array) block-size)]
  (->>
       b-array
       (partition block-size)
       (partition 2 1)
       (take-while #(apply not= %))
       count
       (#(if (= % (dec num-blocks)) nil %)))))

(find-first-duplicated (.getBytes "12349999aaaaaaaa5678") 4)
(find-first-duplicated (.getBytes "12349999aabaaaaa5678") 4)


(defn padding-pkcs7 [block-size k]
  (let [k-seq (seq k)
        padding-size (- block-size (rem (count k-seq) block-size))
        padding-bytes (repeat padding-size padding-size)]

  (byte-array (concat k-seq padding-bytes))))

(defn encrypt-block-cbc [key previous-block block]
  (encrypt-aes (a/array-xor block previous-block) key))

(defn decrypt-block-cbc [key previous-block block]
  (a/array-xor (decrypt-aes block key) previous-block))

(def block-size 16)

(defn encrypt-cbc [data key iv]
  (->> data
       (partition block-size)
       (map byte-array)
       (reductions (partial encrypt-block-cbc key) iv)
       rest
       (apply concat)
       byte-array))

(defn decrypt-cbc [data key iv]
  (let [blocks (map byte-array (partition block-size data))]
    (byte-array (mapcat (fn [previous-block block]
                          (decrypt-block-cbc key previous-block block))
                        (cons iv blocks) blocks))))

(defn gen-random-bytes [len]
  (byte-array (repeatedly len #(a/char->byte (rand-int 256)))))

(defn rand-encrypt [data]
  (let [key (gen-random-bytes 16)
        iv (gen-random-bytes 16)
        encrypt-fn (rand-nth [#(encrypt-cbc %1 %2 iv) encrypt-aes])
        pref (gen-random-bytes (+ 5 (rand-int 6)))
        suff (gen-random-bytes (+ 5 (rand-int 6)))
        new-data (padding-pkcs7 16 (byte-array (concat pref data suff)))]
    (encrypt-fn new-data key)))

(defn ebc-block-cipher-mode? []
  (unique? (rand-encrypt (byte-array (repeat 80 65))) 16))

(defn encryption-oracle [encryption-fn]
  (let [fixed-input (byte-array (repeat 80 65))
        output (encryption-fn fixed-input)]
    (if (unique? output block-size)
      "CBC"
      "EBC")))


;; Challenge 12

(defn encryption-oracle2 [block-size encryption-fn]
  (let [fixed-input (byte-array (repeat (* 4 block-size) (int \A)))
        output (encryption-fn fixed-input)]
    (if (unique? output block-size)
      "CBC"
      "ECB")))

(defn f [block-size message encrypt-fn]
  )

(def random-ecb-key (.getBytes "YELLOW SUBMARINE"))

(def suffix-bytes
  (a/b64->bytes "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))

#_(def suffix-bytes (a/b64->bytes "QUE="))

(count suffix-bytes)

(defn encrypt-with-key [message]
  (encrypt-aes (padding-pkcs7 16 (byte-array (concat message suffix-bytes))) random-ecb-key))

(defn find-block-size [fn]
  (let [max-block-size 1000]
    (->> (range 16 max-block-size)
         (filter #(= "ECB" (encryption-oracle2 % fn)))
         first)))

(find-block-size encrypt-with-key)

(count (encrypt-cbc (gen-random-bytes 17) random-ecb-key (gen-random-bytes 16)))

(find-block-size #(encrypt-cbc % random-ecb-key (gen-random-bytes 16)))

;(find-block-size #(gen-cookie (byte-seq->string %)))

(defn crack [target-fn]
  (let [salt-size  (count (target-fn (.getBytes "")))
        block-size (find-block-size target-fn)
        guessed-size (+ salt-size (rem salt-size block-size))]
  (loop [message (byte-array (repeat guessed-size (int \A)))
         guessed-salt []]
    (let [probe-message (rest message)

          cipher (->> (target-fn probe-message)
                      (take guessed-size))
          cipher-list (->> (range 0 256)
                           (map #(vector % (-> (vec probe-message)
                                               (concat guessed-salt)
                                               vec
                                               (conj %)
                                               byte-array
                                               target-fn
                                               (->> (take guessed-size))))))
          [found _] (->> cipher-list
                         (filter (fn [[c cipher-candidate]]
                                   (= cipher-candidate cipher)))
                         first)]


      (if-not found (butlast (byte-array guessed-salt))
                    (recur (byte-array probe-message)
                           (conj guessed-salt found))))))
  )

(a/byte-seq->string (crack encrypt-with-key))

(count (a/byte-seq->string suffix-bytes))

(defmacro let-dbg
  "Versão do Mauro Lopes <maurolopes@gmail.com>"
  [bindings & body]
  `(do ~@(map #(cons 'def %)
              (partition 2 bindings))
       ~@body))

(defn remove-padding [s]
  (seq s) #_  (drop-last (last s) s))

(defn y [guessed-size encrypt-with-key]
  (loop [message (byte-array (repeat guessed-size (int \A)))
         guessed-salt []]
    (let [probe-message (rest message)

          cipher (->> (encrypt-with-key probe-message)
                      (take guessed-size))
          cipher-list (->> (range 0 256)
                           (map #(vector % (-> (vec probe-message)
                                               (concat guessed-salt)
                                               vec
                                               (conj %)
                                               byte-array
                                               encrypt-with-key
                                               (->> (take guessed-size))))))
          [found _] (->> cipher-list
                         (filter (fn [[c cipher-candidate]]
                                   (= cipher-candidate cipher)))
                         first)]


      (if-not found (butlast (byte-array guessed-salt))
                    (recur (byte-array probe-message)
                           (conj guessed-salt found))))))

;;guessed-size
;;(byte-seq->string (y guessed-size encrypt-with-key))
;;(byte-seq->string x)
;; challenge 13


(defn parse-query-string [s]
  (->> (string/split s #"&")
       (map #(->> (string/split % #"=")
                  (apply  hash-map)))
       (apply merge)))

(defn parse-query-string2 [s]
  (->> (string/split s #"&")
       (mapcat #(string/split % #"="))
       (apply hash-map)))



(defn profile-for [email]
  {:email (string/replace email #"[&=]" "")
   :uid 10
   :role "user"})

(defn to-query-string [x]
  (->> (map (fn [[k v]] (str (name k) "=" v)) x)
       (interpose "&")
       (apply str))
  )

(->> (profile-for "asdasd@kskks&&&&&&&&.com=====")
     str(defn encryption-oracle2 [block-size encryption-fn]
  (let [fixed-input (byte-array (repeat (* 4 block-size) (int \A)))
        output (encryption-fn fixed-input)]
    (if (unique? output block-size)
      "CBC"
      "ECB")))
     )



(def random-key (.getBytes "sjfidhfkshwhmtsjd")) ;; juro que não vi

(defn gen-cookie [email]
  (encrypt-aes (->>
                     (profile-for email)
                     str
                     .getBytes
                     (padding-pkcs7 16))
                    random-key))

#_(->> (gen-cookie "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      (partition 16)
     frequencies
     encrypt-with-key
     )

#_(encrypt-with-key (a/string->byte-seq "fera@nubank.com.br"))


#_(find-prefix-size #(gen-cookie (a/byte-seq->string %)))
#_(find-suffix-size #(gen-cookie (a/byte-seq->string %)))

(defn find-prefix-size [fun]
  (let [block-size (find-block-size fun)]
    (loop [i block-size]
      (let [message (byte-array (repeat i (int \A)))]
        (if-let [♥ (find-first-duplicated (fun message) block-size)]
          (- (* ♥ block-size) (rem i block-size))
          (recur (inc i)))))))


(defn find-suffix-size [fun]
  (- (count (fun (.getBytes ""))) (find-prefix-size fun)))

(comment



(= (butlast (seq x)) (seq suffix-bytes))
(seq suffix-bytes)
(seq x)

; A{15}X A{15} [content][salt]
;              [   prefix    ]

(count (concat probe-message guessed-salt))
(nth cipher-list 82)
;(encryption-oracle #(encrypt-aes % (gen-random-bytes 16)))
;(encryption-oracle #(encrypt-cbc % (gen-random-bytes 16) iv))
)
