(defproject matasano "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.codec "0.1.0"]]
  :profiles {:dev {:dependencies [[expectations "2.0.9"]
                                  [org.clojure/test.check "0.5.9"]]}})
