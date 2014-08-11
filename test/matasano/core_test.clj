(ns matasano.core-test
  (:require
    [expectations :refer :all]
    [matasano-cc.core :refer :all]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]))

(defrecord SimpleCheck []
  CustomPred
  (expect-fn [e a] (:result a))
  (expected-message [e a str-e str-a]
    (format "%s of %s failures" (:failing-size a) (:num-tests a)))
  (actual-message [e a str-e str-a] (format "fail: %s" (:fail a)))
  (message [e a str-e str-a] (format "shrunk: %s" (get-in a [:shrunk :smallest]))))

;; Generators
(def hex-char
  (gen/fmap char (gen/one-of [(gen/choose 97 102)
                              (gen/choose 97 102)])))

(def even-hex-string
  (gen/bind (gen/fmap #(* 2 %) gen/pos-int)
    #(gen/fmap clojure.string/join (gen/vector hex-char %))))

(def two-even-hex-string
  (gen/bind (gen/fmap #(* 2 %) gen/pos-int)
    #(gen/tuple (gen/fmap clojure.string/join (gen/vector hex-char %))
       (gen/fmap clojure.string/join (gen/vector hex-char %)))))

;; Generative tests
(def hex->b64->hex
  (prop/for-all [s even-hex-string]
    (= s (encode-base64 (decode-base64 s)))))

(expect (->SimpleCheck) (tc/quick-check 100 hex->b64->hex))

(def bytes->b64->bytes
  (prop/for-all [v (gen/vector gen/byte)]
    (= v (b64->bytes (bytes->b64 v)))))

(expect (->SimpleCheck) (tc/quick-check 100 bytes->b64->bytes))

(def bytes->hex->bytes
  (prop/for-all [v (gen/vector gen/byte)]
    (= v (vec (hex->bytes (bytes->hex v))))))

(def hex->bytes->hex
  (prop/for-all [s even-hex-string]
    (= s (bytes->hex (hex->bytes s)))))

(expect (->SimpleCheck) (tc/quick-check 100 bytes->hex->bytes))
(expect (->SimpleCheck) (tc/quick-check 100 hex->bytes->hex))

(def xor-hex
  (prop/for-all [[x y] two-even-hex-string]
    (and (= (hex-xor (hex-xor x y) y) x)
         (= (hex-xor (hex-xor x y) x) y))))

(expect (->SimpleCheck) (tc/quick-check 100 xor-hex))

(def xor-bytes
  (prop/for-all [k   (gen/not-empty (gen/vector gen/byte))
                 msg (gen/vector gen/byte)]
    (= msg (decode k (encode k msg)))))

(expect (->SimpleCheck) (tc/quick-check 100 xor-bytes))

(def str->bytes->str
  (prop/for-all [s gen/string-ascii]
    (= s (bytes->str (str->bytes s)))))

(expect (->SimpleCheck) (tc/quick-check 100 str->bytes->str))

(def bytes->str->bytes
  (prop/for-all [b (gen/vector (gen/choose 0 127))]
    (= b (vec (str->bytes (bytes->str b))))))

(expect (->SimpleCheck) (tc/quick-check 100 bytes->str->bytes))

;; Challenges
(let [input  "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
      output "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]
  (expect output (encode-base64 input))
  (expect input (decode-base64 output)))

(expect "746865206b696420646f6e277420706c6179"
  (hex-xor "1c0111001f010100061a024b53535009181c"
           "686974207468652062756c6c277320657965"))

(expect "Cooking MC's like a pound of bacon"
  (best-score "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))

(expect "Now that the party is jumping\n"
  (best-score-file "resources/gistfile1.txt"))

(expect "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  (repeating-key-xor "ICE" "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"))
