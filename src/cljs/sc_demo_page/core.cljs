(ns sc-demo-page.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            ;[sc-demo-page.events]
            [sc-demo-page.subs]
            [sc-demo-page.routes :as routes]
            ;[sc-demo-page.views :as views]
            [sc-demo-page.views1 :as views1]
            [sc-demo-page.config :as config]))


(defn dev-setup []
  (when config/debug?
    (enable-console-print!)
    (println "dev mode")))

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [views1/application]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (routes/app-routes)
  (re-frame/dispatch-sync [:initialize-db])
  (dev-setup)
  (mount-root))
