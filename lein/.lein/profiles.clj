{:user
 {:env {:squiggly {:checkers [:eastwood]
                   :eastwood-exclude-linters [:kibit]}}
  :plugin-repositories [["private-plugins"
                         {:url "private repo url"}]]
  :singing {:gpg-key "matthew.russell@wormbase.org"}
  :dependencies [[clojure-ini "0.0.2"]
                 [me.raynes/fs "1.4.6"]]
  :plugins [[jonase/eastwood "0.2.3"]
            [lein-ancient "0.6.8"]
            [lein-bikeshed "0.3.0"]
            [lein-ns-dep-graph "0.1.0-SNAPSHOT"]
            [refactor-nrepl "0.2.2"]]
  ;; VisualVM profiling opts
  ;; :jvm-opts ["-Dcom.sun.management.jmxremote"
  ;;            "-Dcom.sun.management.jmxremote.ssl=false"
  ;;            "-Dcom.sun.management.jmxremote.authenticate=false"
  ;;            "-Dcom.sun.management.jmxremote.port=43210"]}
  :repl {:dependencies [[acyclic/squiggly-clojure "0.1.6"]
                        [cider/cider-nrepl "0.15.0-SNAPSHOT"]
                        [datomic-schema-grapher "0.0.1"]
                        [org.clojure/tools.nrepl "0.2.12"]]
         :plugins [[cider/cider-nrepl "0.15.0-SNAPSHOT"]]}
  :repl-options {:init (set! *print-length* 10)}}}


