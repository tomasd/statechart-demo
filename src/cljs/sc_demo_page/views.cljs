(ns sc-demo-page.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]))

(defn layout [config]
  [:div
   [:div (:layout/top config)]
   [:div
    [:div]
    [:div (:layout/main config)]
    [:div]]])

(defn application []
  (let [page-layout (re-frame/subscribe [:page-layout])]
    (fn []
      (if @page-layout
        (layout @page-layout)
        [:div "No page"]))))
