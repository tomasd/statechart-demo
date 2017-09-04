(ns sc-demo-page.views
  (:require [re-frame.core :as re-frame]
            [sc-demo-page.db :as db]
            [sc-demo-page.statechart :as sc]
            [reagent.ratom :refer-macros [reaction]]
            [clojure.string :as str]
            [ajax.core :refer [GET POST]]
            [reagent.core :as reagent]
            [sc-demo-page.components :as components]))

(defmulti page-layout (fn [layout positions] layout))

(defmethod page-layout :layout/column [layout positions]
  [:div
   [:div (:column-layout/top positions)]
   [:div.row
    [:div.col-2 (:column-layout/left positions)]
    [:div.col (:column-layout/center positions)]
    [:div.col-2 (:column-layout/right positions)]]
   [:div (:column-layout/bottom positions)]])

(defmethod page-layout :layout/blank [layout positions]
  [:div
   (:blank-layout/center positions)])

(defmulti page-positions (fn [page] page))

(defmethod page-positions :page/loading [_]
  {:blank-layout/center [:div [:i.fa.fa-spinner.fa-5x.fa-spin]]})

(defmethod page-positions :page/home [_]
  {:column-layout/top    [components/top-menu-panel]
   :column-layout/left   [components/menu-panel (re-frame/subscribe [:betting-page/filter])]
   :column-layout/center [:div
                          [components/omnifilter (reagent/atom {})]
                          [components/home-page-tabs]
                          [components/boxes-panel (re-frame/subscribe [:home-page/tab-boxes])]]})

(defmethod page-positions :page/betting [_]
  {:column-layout/top    [components/top-menu-panel]
   :column-layout/left   [components/menu-panel (re-frame/subscribe [:betting-page/filter])]
   :column-layout/center [:div
                          [components/omnifilter (re-frame/subscribe [:betting-page/filter])]
                          [components/boxes-panel (re-frame/subscribe [:betting-page/boxes])]]})

(defmethod page-positions :page/my-matches [_]
  {:column-layout/top    [components/top-menu-panel]
   :column-layout/left   [components/menu-panel]
   :column-layout/center [:div "My matches"]})

(defn application []
  (let [page (re-frame/subscribe [:page])]
    (fn []
      (let [{:keys [name layout]} @page]
        (page-layout layout (page-positions name))))))