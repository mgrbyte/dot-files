{:plugins [[cider/cider-nrepl "0.14.0"]
           [refactor-nrepl "2.2.0"]]
 :dependencies [[cider/cider-nrepl "0.14.0"]]
 :env {:squiggly
              ;; Quote options to avoid warnings
              ;; introduced with `lein-environ 1.1.0` plugin
              "{:checkers [:eastwood]
               :eastwood-exclude-linters [:unlimited-use]}"}}



