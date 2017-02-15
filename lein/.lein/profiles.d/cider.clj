{:plugins [[cider/cider-nrepl "0.14.0"]
           [jonase/eastwood "0.2.3"
            :exclusions [org.clojure/clojure]]
           [refactor-nrepl "2.2.0"]]
 :dependencies [[acyclic/squiggly-clojure "0.1.6"]]
 :env {:squiggly
              ;; Quote options to avoid warnings
              ;; introduced with `lein-environ 1.1.0` plugin
              "{:checkers [:eastwood]
                :eastwood-exclude-linters [:unlimited-use :no-ns-form-found]}"}}



