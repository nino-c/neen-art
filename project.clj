(defproject neenart "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;:plugins [[lein-cljfmt "0.5.6"]]
  :repositories [["sonatype-oss-public"
                  "https://oss.sonatype.org/content/groups/public/"]]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.3"]
                 ;[org.clojure/algo.graph "x.y.z"]
                 [org.clojure/tools.namespace "0.3.0-SNAPSHOT"]
                 [org.clojure/algo.generic "0.1.2"]
                 [net.mikera/core.matrix "0.57.0"]
                 [quil "2.5.0"]
                 [expresso "0.2.0"]]
  :main neenart.landscape2)
