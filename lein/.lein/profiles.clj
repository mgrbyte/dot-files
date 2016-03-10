{:user {
        :plugins [[cider/cider-nrepl "0.12.0-SNAPSHOT"]
                  [jonase/eastwood "0.2.3"]
                  [lein-kibit "0.1.2"]
                  [lein-typed "0.3.5"]
                  ]
        :dependencies [[org.clojure/clojure "1.8.0"]
                       [datomic-schema-grapher "0.0.1"]
                       [com.datomic/datomic-pro "0.9.5350"]
                       [me.raynes/fs "1.4.6"]
                       [clj-stacktrace "0.2.8"]
                       ;; Consider using typed? [org.clojure/core.typed "0.3.22"]
                       ]
        }}
