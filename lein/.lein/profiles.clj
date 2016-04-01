{:user {
        :plugins [[cider/cider-nrepl "0.12.0-SNAPSHOT"]
                  [lein-ancient "0.6.8"]
                  [lein-bikeshed "0.3.0"]
                  [lein-kibit "0.1.2"]]

        :dependencies [[org.clojure/clojure "1.7.0"]
                       [org.clojure/tools.reader "1.0.0-alpha3"]
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

