(ns nike-sk.statechart
  (:require [statechart.core :as sc]))

(defn update-db [f & args]
  (fn [ctx]
    (apply update-in ctx [:fx :db] f args)))

(def statechart
  (sc/make {:type   :and
            :states {:push    {}
                     :content {:type   :and
                               :states {:page       {:type   :xor
                                                     :init   :homepage
                                                     :states {:homepage {:type   :and
                                                                         :enter  [(update-db assoc-in [:page] {:name   :page/home
                                                                                                               :layout :layout/column-3})]
                                                                         :states {:tabs {:type   :xor
                                                                                         :init   :top-5
                                                                                         :states {:top-5      {}
                                                                                                  :10-naj     {}
                                                                                                  :superkurzy {}
                                                                                                  :top-ponuka {}}}}}
                                                              :betting  {:enter [(update-db assoc-in [:page] {:name   :page/betting
                                                                                                              :layout :layout/column-3})]}}}
                                        :sport-menu {}
                                        :layout     {:type   :xor
                                                     :init   :3-col
                                                     :states {:3-col {}
                                                              :4-col {}}}
                                        :betslip    {}}}}}))