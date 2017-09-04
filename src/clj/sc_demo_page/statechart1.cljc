(ns sc-demo-page.statechart1
  (:require [clojure.zip :as zip]
            [clojure.set :as set]

            ))




(comment
  (def statechart
    {:type :and
     :states
           {:push {}
            :user {:type   :xor
                   :init   :anonymous
                   :states {:anonymous     {}
                            :authenticated {}}}
            :page {:type        :xor
                   :init        :home
                   :states      {:home    {}
                                 :betting {}}
                   :transitions [{:event  :goto-home
                                  :target :home}
                                 {:event  :goto-betting
                                  :target :betting}]
                   }}})

  )