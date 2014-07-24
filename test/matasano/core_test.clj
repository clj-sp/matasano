(ns matasano.core-test
  (:require [midje.sweet :refer :all]
            [matasano.core :refer :all]))


(facts "on challenge #1"
       (let [in "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
             out "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"]

         (fact "encode from hex to base64"
               (hex->base64 in) => out)


         (fact "back to hex"
               (base64->hex out) => in))

       )
(future-facts "on challenge #2")
(future-facts "on challenge #3")
(future-facts "on challenge #4")
(future-facts "on challenge #5")
(future-facts "on challenge #6")
(future-facts "on challenge #7")
(future-facts "on challenge #8")
