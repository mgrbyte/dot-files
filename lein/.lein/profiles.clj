{:user {
        :plugins [[cider/cider-nrepl "0.12.0-SNAPSHOT"]
                  [jonase/eastwood "0.2.3"]]

        :dependencies [[org.clojure/clojure "1.7.0"]
                       [datomic-schema-grapher "0.0.1"]
                       [com.datomic/datomic-pro "0.9.5350"]
                       [me.raynes/fs "1.4.6"]
                       [clj-stacktrace "0.2.8"]
                       [acyclic/squiggly-clojure "0.1.5"]
                       ^:replace [org.clojure/tools.nrepl "0.2.12"]
                       ;; Consider using typed? [org.clojure/core.typed "0.3.22"]
                       ]
        }

 ;; VisualVM profiling opts
 :jvm-opts ["-Dcom.sun.management.jmxremote"
            "-Dcom.sun.management.jmxremote.ssl=false"
            "-Dcom.sun.management.jmxremote.authenticate=false"
            "-Dcom.sun.management.jmxremote.port=43210"]}

