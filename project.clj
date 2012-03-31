(defproject hs "0.0.1-SNAPSHOT"
            :description "tell.me"
            :plugins [[lein-cljsbuild "0.1.2"] 
                      [org.clojars.ibdknox/lein-nailgun "1.1.1"]]
            :dependencies [[aleph "0.2.1-alpha1"]
                           [org.clojure/core.match "0.2.0-alpha9"]
                           [net.cgrand/moustache "1.1.0"]
                           [org.clojure/clojure "1.3.0"]
                           [org.clojure/tools.trace "0.7.1"]
						   [vimclojure/server "2.3.1" :exclusions [org.clojure/clojure]]]
            :dev-dependencies [[lein-marginalia "0.7.0"]]
            :source-path "src/clj"
            :cljsbuild {
                        :crossovers [tellme.base]
                        :crossover-path "build/crossover-cljs"
                        :crossover-jar false

                        :builds [{
                                  :notify-command ["growlnotify" "-m" "%" :bell true]
                                  ;:notify-command [:bell true]
                                  :source-path "src/cljs"
                                  :compiler {
                                             :output-to "resources/public/cljs/bootstrap.js"
                                             :optimizations :simple
                                             :pretty-print true}}]}
            :main hs.backend.views.view)

