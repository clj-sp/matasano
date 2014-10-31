(ns matasano.core
  (:import [javax.crypto Cipher]
           [javax.crypto SecretKey]
           [javax.crypto.spec SecretKeySpec IvParameterSpec])
  (:require [clojure.data.codec.base64 :as b64]
            [clojure.string :as string]
            [clojure.java.io :refer :all]))


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
  (let [cipher-seq (string->byte-seq cipher)
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
       (map best-xor)
       transpose
       (apply concat)
       byte-seq->string))

(defn aes [mode message key]
  (let [cipher (Cipher/getInstance "AES/ECB/NoPadding")
        key (SecretKeySpec. key "AES")]
    (.init cipher mode key)
    (-> cipher
        (.doFinal message))))

(def encrypt-aes (partial aes Cipher/ENCRYPT_MODE))
(def decrypt-aes (partial aes Cipher/DECRYPT_MODE))

;; From Marcelo's Branch

(defn bytes->str [b]
  (-> b
      byte-array
      String.))

(defn str->bytes [s]
  (-> s
      .getBytes
      byte-array))

(defn b64->bytes [b64]
  (-> b64
      .getBytes
      b64/decode))

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

(defn distinct-blocks? [blocks]
  (->> blocks
       (map vec)
       (apply distinct?)))

(defn unique? [b-array block-size]
  (->> b-array
      (partition block-size)
      distinct-blocks?))

(defn padding-pkcs7 [block-size k]
  (let [k-seq (seq k)
        padding-size (- block-size (rem (count k-seq) block-size))
        padding-bytes (repeat padding-size padding-size)]

  (byte-array (concat k-seq padding-bytes))))

(defn encrypt-block-cbc [key previous-block block]
  (encrypt-aes (array-xor block previous-block) key))

(defn decrypt-block-cbc [key previous-block block]
  (array-xor (decrypt-aes block key) previous-block))

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
  (byte-array (repeatedly len #(char->byte (rand-int 256)))))

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


(defn encryption-oracle2 [block-size encryption-fn]
  (let [fixed-input (byte-array (repeat (* 2 block-size) (int \A)))
        output (encryption-fn fixed-input)]
    (if (unique? output block-size)
      "CBC"
      "ECB")))

(defn f [block-size message encrypt-fn]
  )

;; AAAA AAAV
;; AAAA AAVA
;; AAAA AVAC
;; AAAA VACA
;; AAAV ACAA
;; AAVA CAAM
;; AVAC AAMA
;; VACA AMAR ELLA

;; AAAO
;; AAOI

;;  0. Seja B o tamanho do Bloco
;;  0. Seja S o tamanho do Salt (padded)
;;  0. Seja SA os caracteres do salt que conhecemos
;;  0. Seja ME a mensagem a cada passo
;;  1. Pegue o tamanho do bloco (max B S) (sendo multiplo de B)
;;  2. Faça ME (repeat tamanho-acima A)
;;  3. Troque os ultimos caracteres de ME pelo Salt conhecido
;;  4. REST de ME, para remover o primeiro caracter
;;  5. Encrypt de ME
;;  6. Para n de 0 a 255, (encrypt (conj (repeat (- tamanho-maximo (count SA)) (int \A)) n))
;;     a. Caso (= ME-encryptado ME-acima-encryptado) -> SA .= n

(def random-ecb-key (.getBytes "YELLOW SUBMARINE"))

(def suffix-bytes
  (b64->bytes "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))

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

(def salt-size (count (encrypt-with-key (.getBytes ""))))

(def guessed-size (+ salt-size (rem salt-size block-size)))

(count (byte-seq->string suffix-bytes))

(defmacro let-dbg
  "Versão do Mauro Lopes <maurolopes@gmail.com>"
  [bindings & body]
  `(do ~@(map #(cons 'def %)
              (partition 2 bindings))
       ~@body))

(def remove-padding
  ([seq] (remove-padding 1 seq))

  )

;(comment
(def x
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
    (if-not found (remove-padding (byte-array guessed-salt))
      (recur (byte-array probe-message)
             (conj guessed-salt found))))))

(= (butlast (seq x)) (seq suffix-bytes))
(seq suffix-bytes)
(seq x)

; A{15}X A{15} [content][salt]
;              [   prefix    ]

(count (concat probe-message guessed-salt))
(nth cipher-list 82)
;(encryption-oracle #(encrypt-aes % (gen-random-bytes 16)))
;(encryption-oracle #(encrypt-cbc % (gen-random-bytes 16) iv))
