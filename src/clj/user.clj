(ns user
  (:require [sc-demo-page.statechart1 :as sc]
            [sc-demo-page.context :as ctx]
            [sc-demo-page.state :as state]
            [sc-demo-page.demo :as demo]
            [sc-demo-page.transition :as transition]))

(defn process-event [ctx event]
  (let [{:keys [configuration fx]} ctx
        fx (select-keys fx [:db])]
    (clojure.pprint/pprint fx)
    (sc/process-event demo/idx configuration fx event)))

(def x (-> (sc/initialize demo/idx {})
           (process-event [:goto-page :page/betting])))

(def ctx (-> (ctx/make-ctx demo/idx (:configuration x) (:fx x)
                           [:toggle-live])
             (ctx/pop-event)))

(ctx/current-event ctx)
(sc/eventless-transitions ctx)
(sc/event-transitions ctx)

(->> (ctx/current-configuration ctx)
     (filter state/atomic?)
     (sort-by state/entry-order)
     (map (fn [atomic-state]
            (->> (sc/select-event-transitions atomic-state (ctx/current-event-id ctx))
                 (filter #(transition/applicable-transition? % ctx))
                 first)))
     (remove nil?)
     distinct
     (sc/remove-conflicting-transitions ctx)
     )


(-> (sc/initialize demo/idx {})
    (process-event [:goto-page :page/betting])
    (update-in [:fx] select-keys [:db])
    (process-event [:toggle-live]))
