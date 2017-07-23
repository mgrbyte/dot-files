{:plugins [[cider/cider-nrepl "0.15.0"]
           [refactor-nrepl "2.3.1"]]
 :dependencies [[cider/cider-nrepl "0.15.0"]]
 :env {:squiggly
              ;; Quote options to avoid warnings
              ;; introduced with `lein-environ 1.1.0` plugin
              "{:checkers [:eastwood]
               :eastwood-exclude-linters [:unlimited-use]}"}}



