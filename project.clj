(defproject minesweeper "0.1.0-SNAPSHOT"
  :description "A mine sweeper"
  :url "https://github.com/changgengli/minesweeper.git"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2760"]
                 [reagent "0.4.3"]]
  :plugins [[lein-cljsbuild "1.0.4"]
            [cider/cider-nrepl "0.8.2"]]
  :profiles {:dev {:plugins [[com.cemerick/austin "0.1.6"]]}}
  :cljsbuild {:builds [{:source-paths ["src-cljs"]
                        :compiler {:output-to "app.js"
                                   :optimizations :whitespace
                                   :preamble ["reagent/react.js"]
                                   :pretty-print true}}]})
