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


(defn aes-ecb [mode message key]
  (let [cipher (Cipher/getInstance "AES/ECB/NoPadding")
        key (SecretKeySpec. key "AES")]
    (.init cipher mode key)
    (-> cipher
        (.doFinal message))))

(def encrypt-ecb (partial aes-ecb Cipher/ENCRYPT_MODE))
(def decrypt-ecb (partial aes-ecb Cipher/DECRYPT_MODE))

(defn encrypt-block-cbc [key previous-block block]
  (encrypt-ecb (a/array-xor block previous-block) key))

(defn decrypt-block-cbc [key previous-block block]
  (a/array-xor (decrypt-ecb block key) previous-block))

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

(defn rand-encrypt [data]
  (let [key (gen-random-bytes 16)
        iv (gen-random-bytes 16)
        encrypt-fn (rand-nth [#(encrypt-cbc %1 %2 iv) encrypt-ecb])
        pref (gen-random-bytes (+ 5 (rand-int 6)))
        suff (gen-random-bytes (+ 5 (rand-int 6)))
        new-data (padding-pkcs7 16 (byte-array (concat pref data suff)))]
    (encrypt-fn new-data key)))

(defn ebc-block-cipher-mode? []
  (unique? (rand-encrypt (byte-array (repeat 80 65))) 16))

(defn find-block-size [fn]
  (let [max-block-size 1000]
    (->> (range 16 max-block-size)
         (filter #(= "ECB" (encryption-oracle2 % fn)))
         first)))

#_(find-block-size encrypt-with-key)

#_(count (encrypt-cbc (gen-random-bytes 17) random-ecb-key (gen-random-bytes 16)))

#_(find-block-size #(encrypt-cbc % random-ecb-key (gen-random-bytes 16)))

#_(defn concat-byte-array [b1 b2]
  (byte-array (concat b1 b2)))

#_(defn ignore-prefix [target-fn]
  (let [prefix-size (find-prefix-size target-fn)
        block-size (find-block-size target-fn)
        padding-size (- block-size (rem prefix-size block-size))]
    #(->> (target-fn (concat-byte-array (byte-array padding-size) %))
          (drop padding-size)
          byte-array)))

(defn crack [target-fn]
  (let [salt-size  (count (target-fn (.getBytes "")))
        block-size (find-block-size target-fn)
        guessed-size (+ salt-size (rem salt-size block-size))
        guess-fn (fn [probe-message-with-salt]
                   (->> (for [i (range 256)]
                          {(-> probe-message-with-salt
                               (conj i)
                               byte-array
                               target-fn
                               (->> (take guessed-size))) i})
                        (apply merge)))]
    (loop [message (vec (repeat guessed-size (int \A)))
           guessed-salt []]
      (if (= guessed-size (count guessed-salt))
        guessed-salt
        (let [probe-message (subvec message 1)
              cipher (->> (target-fn probe-message)
                          (take guessed-size))
              probe-message-with-salt (into probe-message guessed-salt)
              cipher-map (guess-fn probe-message-with-salt)
              salt-char (cipher-map cipher)]
          (if-not salt-char
            (butlast guessed-salt) ; it will only work for pkcs7.
            (recur probe-message
                   (conj guessed-salt salt-char))))))))

(defn find-padding [target-fn]
  (count (target-fn (.getBytes "")))

  )


(comment
  (let [suffix-bytes (a/b64->bytes "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")
        random-ecb-key (.getBytes "YELLOW SUBMARINE")
        encrypt-with-key #(encrypt-ecb (padding-pkcs7 16 (byte-array (concat % suffix-bytes))) random-ecb-key)]
    ;(find-padding encrypt-with-key)
    (a/bytes->str (crack encrypt-with-key))
    )

  )

(defn remove-padding-pkcs7 [s]
  (byte-array (drop-last (int (last s)) s)))




(defn parse-query-string [s]
  (->> (string/split s #"&")
       (mapcat #(string/split % #"="))
       (apply hash-map)))

(defn profile-for [email]
  {:email (string/replace email #"[&=]" "")
   :uid 10
   :role "user"})

(defn to-query-string [{:keys [role email uid]}]
  (format "email=%s&uid=%s&role=%s" email uid role))

(def random-key (.getBytes "sjfidhfkshwhmtsj")) ;; juro que não vi

(defn gen-cookie [email]
  (encrypt-ecb (->> email
                    profile-for
                    to-query-string
                    str
                    .getBytes
                    (padding-pkcs7 16))
               random-key))

(defn open-cookie [cookie]
  (-> cookie
      (decrypt-ecb random-key)
      remove-padding-pkcs7
      String.
      parse-query-string))

(defn find-first-duplicated [b-array block-size]
  (let [num-blocks (/ (count b-array) block-size)]
  (->> b-array
       (partition block-size)
       (partition 2 1)
       (take-while #(apply not= %))
       count
       (#(if (= % (dec num-blocks)) nil %)))))

(defn find-prefix-size [fun]
  (let [block-size (find-block-size fun)]
    (loop [i block-size]
      (let [message (byte-array (repeat i (int \A)))]
        (if-let [first-duplicated (find-first-duplicated (fun message) block-size)]
          (- (* first-duplicated block-size) (rem i block-size))
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
;(encryption-oracle #(encrypt-ecb % (gen-random-bytes 16)))
;(encryption-oracle #(encrypt-cbc % (gen-random-bytes 16) iv))
)
