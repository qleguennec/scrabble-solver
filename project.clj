(defproject scrabble-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [http-kit "2.3.0"]
                 [clj-time "0.14.0"]
                 [compojure "1.6.1"]
                 [com.novemberain/monger "3.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [ring "1.7.0"]
                 [ring/ring-defaults "0.3.2"]]
  :repl-options {:init-ns scrabble-solver.core}
  :jvm-opts ["-Xmx16G"])
