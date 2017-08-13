{:plugins [[cider/cider-nrepl "0.15.1-SNAPSHOT"]
           [refactor-nrepl "2.4.0-SNAPSHOT"]]
 :dependencies [[acyclic/squiggly-clojure "0.1.8"]]
 :env {:squiggly
       ;; Quote options to avoid warnings
       ;; introduced with `lein-environ 1.1.0` plugin
       "{:checkers [:eastwood]
         :eastwood-exclude-linters [:unlimited-use :no-ns-form-found]}"}}



