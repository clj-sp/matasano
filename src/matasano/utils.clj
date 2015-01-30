(ns matasano.utils
  (:require [clojure.string :as str]))

(defn read-lines [file]
  (str/split-lines (slurp file)))
