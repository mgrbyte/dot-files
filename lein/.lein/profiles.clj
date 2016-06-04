{:user {
          :dependencies [[cider/cider-nrepl "0.12.0-SNAPSHOT"] ; Emnacs
                         [acyclic/squiggly-clojure "0.1.5"] ; Emacs
                         ^:replace [org.clojure/tools.nrepl "0.2.12"] ; Emacs
                         ]
        :singing {:gpg-key "matthew.russell@wormbase.org"}}
 ;; VisualVM profiling opts
 :jvm-opts ["-Dcom.sun.management.jmxremote"
            "-Dcom.sun.management.jmxremote.ssl=false"
            "-Dcom.sun.management.jmxremote.authenticate=false"
            "-Dcom.sun.management.jmxremote.port=43210"]}
