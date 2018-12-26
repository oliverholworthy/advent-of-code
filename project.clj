(defproject oliverholworthy.advent-of-code "0.1.0"
  :description "Advent of Code Solutions"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.priority-map "0.0.10"]
                 [com.microsoft/z3 "4.8.4"]
                 [org.clojure/core.async "0.3.465"]]
  :jvm-opts [~(str "-Djava.library.path="
                   (. System getProperty "java.library.path")
                   ":" "lib")])
