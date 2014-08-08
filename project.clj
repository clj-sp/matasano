(defproject matasano "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins      [[lein-midje "3.1.3"]]
  :dependencies [[org.clojure/clojure "1.7.0-alpha1"]
                 [com.taoensso/nippy "2.6.3"]
                 [midje "1.6.3" :exclusions [org.clojure/core]]
                 [org.clojure/data.codec "0.1.0"]
                 [commons-codec/commons-codec "1.9"]])
