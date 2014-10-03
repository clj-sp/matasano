(ns matasano.core-test
  (:require [midje.sweet :refer :all]
            [clojure.string :as string]
            [matasano.core :refer :all]))

(facts "on helper functions"
       (let [message "Hello, world!"]

         (fact "Convert string to hex-string and back"
               (-> message string->hex hex-string->string) => message)
         (fact "Convert string to byte-seq and back"
               (-> message string->byte-seq byte-seq->string) => message)))

(facts "on challenge #1"
       (let [in "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
             out "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]

         (fact "encode from hex to base64"
               (to-str (hex-string->base64 in)) => out)

         (fact "back to hex"
               (to-str (base64->hex out)) => in)))

(facts "on challenge #2"
       (let [hex-str-1 "1c0111001f010100061a024b53535009181c"
             hex-str-2 "686974207468652062756c6c277320657965"
             out "746865206b696420646f6e277420706c6179"]
         (fact "xor on hex-str"
               (map bit-xor (hex-string->byte-seq hex-str-1) (hex-string->byte-seq hex-str-2)) => (hex-string->byte-seq out))))

(facts "on challenge #3"
       (let [message "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"]
         (fact "decoding the message"
               (encode-message "any key" (hex-string->byte-seq message)) => irrelevant)
         (fact "finding best-xor"
               (byte-seq->string (best-xor (hex-string->byte-seq "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")))
               => "Cooking MC's like a pound of bacon")))

(facts "on challenge #4"
       (fact "finding best xor in file"
             (byte-seq->string (best-xor-in-file "resources/challenge04"))
             => "Now that the party is jumping\n"))

(facts "on challenge #5 - Repeating-key XOR Cipher"
       (let [message "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
             key "ICE"]
         (fact "encoding a message"
               (->> message string->byte-seq (encode-message key) byte-seq->string string->hex)
               => "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")))

(facts "on challenge #6"
       (let [cipher (->> (string/replace (slurp "resources/challenge06") "\n" "")
                         base64->hex
                         (apply str)
                         (hex-string->byte-seq))]
         (fact "on hamming distance"
               (hamming-distance (string->byte-seq "this is a test") (string->byte-seq "wokka wokka!!!")) => 37)
         (fact "on guess-keysize"
               (guess-keysize cipher 2 40) => 29)
         (fact "on challenge6"
               (let [challenge6 (break-repeating-key-xor cipher)]
                 (count challenge6) => 2871
                 (.startsWith challenge6 "I'm back and I'm ringin' the bell") => true
                 (.contains challenge6 "Supercalafragilisticexpialidocious") => true))))

(facts "on challenge #7"
       (-> (apply str (read-lines "resources/challenge07"))
           b64->bytes
           (decrypt-aes (str->bytes "YELLOW SUBMARINE"))
           bytes->str)
       => #"^I'm back")

(facts "on challenge #8"
       (fact "on distinct_blocks?"
         (distinct-blocks? [[1] [1]]) => false
         (distinct-blocks? [[1] [2]]) => true)

       (fact "on unique?"
         (unique? [1 1] 1) => false
         (unique? [1 2] 1) => true)

       (->> (read-lines "resources/challenge08")
            (map hex->bytes)
            (remove #(unique? % 16))
            count) => 1)

(facts "on challenge #9"
       (fact "on padding-pkcs7"
             (->> [42]
                  (padding-pkcs7 2)
                  (map int)) => [42 1])
             (->> [42 42]
                  (padding-pkcs7 2)
                  (map int)) => [42 42 2 2]

       (->> "YELLOW SUBMARINE"
            .getBytes
            (padding-pkcs7 20)
            (map char)
            (apply str)) => "YELLOW SUBMARINE\x04\x04\x04\x04")


(facts "on challenge #10"
       (let [iv (byte-array (repeat block-size 0))]
       (-> (apply str (read-lines "resources/challenge10"))
           b64->bytes
           (decrypt-cbc (str->bytes "YELLOW SUBMARINE") iv)
           bytes->str) => #"^I'm back"))








