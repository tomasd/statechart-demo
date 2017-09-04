(ns sc-demo-page.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [sc-demo-page.events]
            [sc-demo-page.subs]
            [sc-demo-page.views :as views]
            [sc-demo-page.config :as config]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views/application]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (re-frame/dispatch-sync [:initialize-db])
  (js/setTimeout #(re-frame/dispatch [:goto-page :page/betting]) 300)
  (dev-setup)
  (mount-root))
