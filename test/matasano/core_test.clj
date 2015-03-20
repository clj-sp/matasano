(ns matasano.core-test
  (:require [midje.sweet :refer :all]
            [clojure.string :as string]
            [matasano.adapters :as a]
            [matasano.utils :refer :all]
            [matasano.scores :as s]
            [matasano.core :refer :all]))

(facts "on helper functions"
       (let [message "Hello, world!"]

         (fact "Convert string to hex-string and back"
               (-> message a/string->hex a/hex-string->string) => message)
         (fact "Convert string to byte-seq and back"
               (-> message a/string->byte-seq a/byte-seq->string) => message)))

(facts "on challenge #1"
       (let [in "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
             out "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]

         (fact "encode from hex to base64"
               (a/to-str (a/hex-string->base64 in)) => out)

         (fact "back to hex"
               (a/to-str (a/base64->hex out)) => in)))

(facts "on challenge #2"
       (let [hex-str-1 "1c0111001f010100061a024b53535009181c"
             hex-str-2 "686974207468652062756c6c277320657965"
             out "746865206b696420646f6e277420706c6179"]
         (fact "xor on hex-str"
               (map bit-xor (a/hex-string->byte-seq hex-str-1) (a/hex-string->byte-seq hex-str-2)) => (a/hex-string->byte-seq out))))

(facts "on challenge #3"
       (let [message "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"]
         (fact "decoding the message"
               (a/encode-message "any key" (a/hex-string->byte-seq message)) => irrelevant)
         (fact "finding best-xor"
               (a/byte-seq->string (s/best-xor (a/hex-string->byte-seq "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")))
               => "Cooking MC's like a pound of bacon")))

(facts "on challenge #4"
       (fact "finding best xor in file"
             (a/byte-seq->string (s/best-xor-in-file "resources/challenge04"))
             => "Now that the party is jumping\n"))

(facts "on challenge #5 - Repeating-key XOR Cipher"
       (let [message "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
             key "ICE"]
         (fact "encoding a message"
               (->> message a/string->byte-seq (a/encode-message key) a/byte-seq->string a/string->hex)
               => "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")))

(facts "on challenge #6"
       (let [cipher (->> (string/replace (slurp "resources/challenge06") "\n" "")
                         a/base64->hex
                         (apply str)
                         (a/hex-string->byte-seq))]
         (fact "on hamming distance"
               (s/hamming-distance (a/string->byte-seq "this is a test") (a/string->byte-seq "wokka wokka!!!")) => 37)
         (fact "on guess-keysize"
               (s/guess-keysize cipher 2 40) => 29)
         (fact "on challenge6"
               (let [challenge6 (s/break-repeating-key-xor cipher)]
                 (count challenge6) => 2871
                 (.startsWith challenge6 "I'm back and I'm ringin' the bell") => true
                 (.contains challenge6 "Supercalafragilisticexpialidocious") => true))))

(facts "on challenge #7"
       (-> (apply str (read-lines "resources/challenge07"))
           a/b64->bytes
           (decrypt-ecb (a/str->bytes "YELLOW SUBMARINE"))
           a/bytes->str)
       => #"^I'm back")

(facts "on challenge #8"
       (fact "on distinct_blocks?"
         (distinct-blocks? [[1] [1]]) => false
         (distinct-blocks? [[1] [2]]) => true)

       (fact "on unique?"
         (unique? [1 1] 1) => false
         (unique? [1 2] 1) => true)

       (->> (read-lines "resources/challenge08")
            (map a/hex->bytes)
            (remove #(unique? % 16))
            count) => 1)


(facts "on challenge #9"
       (fact "on padding-pkcs7"
             (->> [42]
                  (padding-pkcs7 2)
                  (map int)) => [42 1]
             (->> [42 42]
                  (padding-pkcs7 2)
                  (map int)) => [42 42 2 2]


;;           (->> "YELLOW SUBMARINE"
;;              .getBytes
;;              (padding-pkcs7 20)
;;              (map char)
;;              (apply str)) => (str "YELLOW SUBMARINE\x04\x04\x04\x04")
            ))



(facts "on challenge #10"
       (let [iv (byte-array (repeat block-size 0))]
       (-> (apply str (read-lines "resources/challenge10"))
           a/b64->bytes
           (decrypt-cbc (a/str->bytes "YELLOW SUBMARINE") iv)
           a/bytes->str) => #"^I'm back"))

(facts "on challenge #11"
       (let [iv (byte-array (repeat block-size 0))]
         (fact "deterministic"
               (encryption-oracle #(encrypt-ecb % (gen-random-bytes 16))) => "ECB"
               (encryption-oracle #(encrypt-cbc % (gen-random-bytes 16) iv)) => "CBC")

         (fact "mocking randomness"
               (encryption-oracle rand-encrypt) => "ECB"
               (provided (rand-nth anything) => encrypt-ecb)

               (encryption-oracle rand-encrypt) => "CBC"
               (provided (rand-nth anything) => #(encrypt-cbc %1 %2 (gen-random-bytes 16))))))

(facts "on challenge #12"
       (let [suffix-bytes (a/b64->bytes "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK")
             random-ecb-key (.getBytes "YELLOW SUBMARINE")
             encrypt-with-key #(encrypt-ecb (padding-pkcs7 16 (byte-array (concat % suffix-bytes))) random-ecb-key)]
         (a/bytes->str (crack encrypt-with-key)) => #"(?s)^Rollin' in my 5.0.*I just drove by\n$"))

(facts "on challenge #13"
  (fact "on finding the first duplicated block"
        (find-first-duplicated (byte-array [1 1 2 2 5 6 3 3 3 3]) 2)  =>  3)

  (fact ""
        (find-prefix-size #(-> % a/bytes->str gen-cookie ) ) => 6)

       )

