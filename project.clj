(defproject minesweeper "0.1.0-SNAPSHOT"
  :description "A mine sweeper"
  :url "https://github.com/changgengli/minesweeper.git"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [reagent  "0.6.0-alpha"]]
  :plugins [[lein-cljsbuild "1.1.1"]
            [cider/cider-nrepl "0.8.2"]]
  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.6"]]}}
  :cljsbuild {:builds 
              {:dev {:source-paths ["src-cljs"]
                        :compiler {:output-to "app.js"
                                   :optimizations :whitespace
                                   :preamble ["reagent/react.js"]
                                   :pretty-print true}}
               :prod {:source-paths ["src-cljs"]
                        :compiler {:output-to "app-prod.js"
                                   :optimizations :advanced
                                   :preamble  ["reagent/react.min.js"]
                                   :externs ["externs.js"]
                                   :pretty-print false}}}})
