{:user {:plugins [[cider/cider-nrepl "0.17.0-SNAPSHOT"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]]}
 :repl {:dependencies
        [[acyclic/squiggly-clojure "0.1.9-SNAPSHOT"
          :exclusions [org.clojure/tools.reader]]
         [compliment "0.3.5"]]}
 :env {:squiggly
       "{:checkers [:eastwood]
         :eastwood-exclude-linters [:unlimited-use :no-ns-form-found]}"}}



