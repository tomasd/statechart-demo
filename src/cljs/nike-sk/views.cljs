(ns nike-sk.views
  (:require [re-frame.core :as re-frame]
            [nike-sk.components :as components]
            [reagent.core :as reagent]))

(defmulti page-layout (fn [layout positions] layout))

(defmethod page-layout :layout/column-3 [layout positions]
  [:div
   [:div (:column-layout/top positions)]
   [:div.row
    [:div.col-2 (:column-layout/left positions)]
    [:div.col (:column-layout/center positions)]
    [:div.col-2 (:column-layout/right positions)]]
   [:div (:column-layout/bottom positions)]])

(defmethod page-layout :layout/column-4 [layout positions]
  [:div
   [:div (:column-layout/top positions)]
   [:div.row
    [:div.col-2 (:column-layout/left positions)]
    [:div.col
     [:div
      [:div.col (:column-layout/center-top positions)]
      [:div.row
       [:div.col (:column-layout/center positions)]
       [:div.col (:column-layout/detail positions)]]]]
    [:div.col-2 (:column-layout/right positions)]]
   [:div (:column-layout/bottom positions)]])

(defmethod page-layout :layout/blank [layout positions]
  [:div
   (:blank-layout/center positions)])

(defmulti page-positions (fn [page layout] [page layout]))

(defmethod page-positions :page/loading [_]
  {:blank-layout/center [:div [:i.fa.fa-spinner.fa-5x.fa-spin]]})

(defmethod page-positions [:page/home :layout/column-3] [_ _]
  {:column-layout/top    [components/top-menu-panel]
   :column-layout/left   [:div
                          [components/menu-panel]
                          [:hr]
                          [components/sport-menu-panel (reagent/atom {})]]
   :column-layout/center [:div
                          [components/omnifilter (reagent/atom {})]
                          [components/home-page-tabs]
                          [components/boxes-panel (re-frame/subscribe [:home-page/tab-boxes])]
                          [:div "Analyses"]]})

(defmethod page-positions [:page/home :layout/column-4] [_ _]
  {:column-layout/top        [components/top-menu-panel]
   :column-layout/left       [:div
                              [components/menu-panel]
                              [:hr]
                              [components/sport-menu-panel (reagent/atom {})]]
   :column-layout/center-top [components/omnifilter (reagent/atom {})]
   :column-layout/center     [:div
                              [components/home-page-tabs]
                              [components/boxes-panel (re-frame/subscribe [:home-page/tab-boxes])]]
   :column-layout/detail     [:div "Analyses"]
   })

(defmethod page-positions [:page/betting :layout/column-3] [_ _]
  (let [page-filter (re-frame/subscribe [:betting-page/filter])]
    {:column-layout/top    [components/top-menu-panel]
     :column-layout/left   [:div
                            [components/menu-panel]
                            [:hr]
                            [components/sport-menu-panel page-filter]]
     :column-layout/center [:div
                            [components/omnifilter page-filter]
                            [components/boxes-panel (re-frame/subscribe [:betting-page/boxes])]]}))

(defmethod page-positions [:page/betting :layout/column-4] [_ _]
  (let [page-filter (re-frame/subscribe [:betting-page/filter])]
    {:column-layout/top        [components/top-menu-panel]
     :column-layout/left       [:div
                                [components/menu-panel]
                                [:hr]
                                [components/sport-menu-panel page-filter]]
     :column-layout/center-top [components/omnifilter page-filter]
     :column-layout/center     [:div
                                [components/boxes-panel (re-frame/subscribe [:betting-page/boxes])]]
     :column-layout/detail     [:div "Detail"]
     })
  )

(defmethod page-positions [:page/my-matches :layout/column-4] [_]
  {:column-layout/top    [components/top-menu-panel]
   :column-layout/left   [components/menu-panel]
   :column-layout/center [:div "My matches"]})



(defn application []
  (let [page (re-frame/subscribe [:page])]
    (fn []
      (let [{:keys [name layout]} @page]
        (page-layout layout (page-positions name layout))))))