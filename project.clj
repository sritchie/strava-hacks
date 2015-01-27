(defproject racehub/strava-hacks "0.1.0-SNAPSHOT"
  :description "Strava Chrome plugin."
  :url "https://github.com/racehub/strava-hacks"
  :source-paths ["src/cljs" "src/clj"]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :exclusions [org.clojure/clojure org.clojure/clojurescript]
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [org.clojure/clojurescript "0.0-2727"]
                 [prismatic/schema "0.3.1"]

                 ;; Mostly here for clj-only testing.
                 [cheshire "5.3.1"]]
  :cljsbuild
  {:builds
   {:dev
    {:source-paths ["src/cljs" "src/clj"]
     :notify-command ["node" "run.js"]
     :compiler {:output-to "out/strava.js"
                :output-dir "out"
                :optimizations :none
                :target :nodejs
                :source-map true}
     :test
     {:source-paths ["src/cljs" "src/clj" "test"]
      :compiler {:output-to "target/strava.js"
                 :optimizations :whitespace
                 :pretty-print true}}}}}
  :lein-release {:deploy-via :shell
                 :shell ["lein" "deploy" "clojars"]})
