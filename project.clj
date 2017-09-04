(defproject sc-demo-page "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.908"]
                 [reagent "0.7.0"]
                 [re-frame "0.10.1"]
                 [secretary "1.2.3"]
                 [cljs-ajax "0.7.0"]]

  :plugins [[lein-cljsbuild "1.1.5"]]

  :min-lein-version "2.5.3"

  :source-paths ["src/clj"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :figwheel {:css-dirs ["resources/public/css"]}

  :profiles
  {:dev
   {:dependencies [[binaryage/devtools "0.9.4"]
                   [com.cemerick/piggieback "0.2.2"]
                   [figwheel-sidecar "0.5.13"]
                   [org.clojure/tools.nrepl "0.2.12"]]

    :plugins      [[lein-figwheel "0.5.13"]]
    :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
    }}



  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src/cljs" "src/clj"]
     :figwheel     {:on-jsload "nike-sk.core/mount-root"}
     :compiler     {:main                 nike-sk.core
                    :output-to            "resources/public/js/compiled/app.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    :preloads             [devtools.preload]
                    :external-config      {:devtools/config {:features-to-install :all}}
                    }}

    {:id           "min"
     :source-paths ["src/cljs"]
     :compiler     {:main            nike-sk.core
                    :output-to       "resources/public/js/compiled/app.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}


    ]}

  )
