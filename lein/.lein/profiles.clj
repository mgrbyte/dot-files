{:user {
        :plugins [[cider/cider-nrepl "0.12.0-SNAPSHOT"]]

        :dependencies [[org.clojure/clojure "1.7.0"]
                       [datomic-schema-grapher "0.0.1"]
                       [com.datomic/datomic-pro "0.9.5350"]
                       [me.raynes/fs "1.4.6"]
                       [clj-stacktrace "0.2.8"]
                       [acyclic/squiggly-clojure "0.1.5"]
                       ^:replace [org.clojure/tools.nrepl "0.2.12"]
                       ;; Consider using typed? [org.clojure/core.typed "0.3.22"]
                       ]
        }}

