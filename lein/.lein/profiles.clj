{:repl {:plugins [[refactor-nrepl "2.4.0-SNAPSHOT"]
                  [cider/cider-nrepl "0.20.0"]]
        :dependencies
        [[org.clojure/tools.nrepl "0.2.13"]
         [acyclic/squiggly-clojure "0.1.9-SNAPSHOT" :exclusions [org.clojure/tools.reader]]
         [compliment "0.3.5"]]}
 :env {:squiggly
       "{:checkers [:eastwood]
         :eastwood-exclude-linters [:unlimited-use :no-ns-form-found]}"}}

