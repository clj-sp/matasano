(ns matasano.core-test
  (:require [midje.sweet :refer :all]
            [matasano.core :refer :all]))


(facts "on challenge #1"
       (let [in "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
             out "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]

         (fact "encode from hex to base64"
               (hex->base64 in) => out)

         (fact "back to hex"
               (base64->hex out) => in)))

(facts "on challenge #2"
       (let [hex-str-1 "1c0111001f010100061a024b53535009181c"
             hex-str-2 "686974207468652062756c6c277320657965"
             out "746865206b696420646f6e277420706c6179"]

         (xor hex-str-1 hex-str-2)
         (fact "xor on hex-str"
               (xor hex-str-1 hex-str-2) => out)))



(facts "on challenge #3"
       (let [key (prepare-key (str (char 162)))
             message "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"]
         (fact "decoding the message"
               (decode-message 100 message) => irrelevant)
         (fact "finding best-xor"
               (best-xor message)
               => "Cooking MC's like a pound of bacon")))

(facts "on challenge #4"
       (fact "finding best xor in file"
             (best-xor-in-file "resources/challenge04")
             => "Now that the party is jumping\n"))

(facts "on challenge #5 - Repeating-key XOR Cipher"
       (let [message "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
             key (prepare-key "ICE")]
         (fact "encoding a message"
                (encode-message key message)
                => "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")))

(future-facts "on challenge #6")
(future-facts "on challenge #7")
(future-facts "on challenge #8")
