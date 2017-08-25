(ns sc-demo-page.views
  (:require [re-frame.core :as re-frame]
            [reagent.core :as reagent]))

(defn layout [config]
  [:div
   [:div (:layout/top config)]
   [:div
    [:div {:style {:float "left"}} (:layout/left config)]
    [:div {:style {:float "left"}} (:layout/main config)]
    [:div {:style {:float "left"}} (:layout/right config)]]])

(defn application []
  (let [page-layout (re-frame/subscribe [:page-layout])]
    (fn []
      (if @page-layout
        (layout @page-layout)
        [:div "No page"]))))
