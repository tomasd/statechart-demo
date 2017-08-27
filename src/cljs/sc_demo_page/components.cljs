(ns sc-demo-page.components
  (:require
    [reagent.ratom :refer-macros [reaction]]
    [re-frame.core :as re-frame]
    [sc-demo-page.view-utils :as wu]
    [reagent.core :as reagent]
    [clojure.string :as str]))

(defn page-nav-link [page label]
  (let [current-page (re-frame/subscribe [:current-page-name])]
    (fn [page label]
      [:a.nav-link {:href     "#"
                    :class    (wu/classnames {"active" @(reaction (= page @current-page))})
                    :on-click (wu/handle-dispatch [:goto-page page])}
       label])))

(defn sport-menu-item [box-filter item]
  [:li
   [:a {:href     (:slug item)
        :on-click (wu/handle-dispatch [:set-menu (:slug item)])}
    (str (when @(reaction (= (:slug item) (get @box-filter :menu))) " * ")
         (:label item))]
   (when (seq (:items item))
     [:ul
      (for [[i item] (map-indexed vector (:items item))]
        ^{:key i} [sport-menu-item box-filter item])])])

(defn sport-menu-panel [box-filter]
  (let [menu (re-frame/subscribe [:menu])]
    (fn []
      [:ul
       (for [[i item] (map-indexed vector (:items @menu))]
         ^{:key i} [sport-menu-item box-filter item])])))

(defn top-menu-panel []
  (let [current-page   (re-frame/subscribe [:current-page-name])
        authenticated? (re-frame/subscribe [:authenticated-user?])]
    (fn []
      [:nav.navbar.navbar-expand-lg.navbar-light.bg-light
       [:a.navbar-brand {:href "#"} "Company logo"]
       [:button.navbar-toggler {:type "button"}
        [:span.navbar-toggler-icon]]

       [:div.collapse.navbar-collapse
        [:ul.navbar-nav.mr-auto
         [:li.nav-item {:class (wu/classnames {"active" @(reaction (or (= :page/home @current-page)
                                                                       (= :page/betting @current-page)
                                                                       (= :page/my-matches @current-page)))})}
          [:a.nav-link {:href     "#"
                        :on-click (wu/handle-dispatch [:goto-page :page/home])} "Betting"]]

         [:li.nav-item
          [:a.nav-link {:href "#"} "Other"]]
         ]
        (if @authenticated?
          [:div "Authenticated user" [:button {:type "button"
                                               :on-click (wu/handle-dispatch [:logout])} "Logout"]]
          [:div "Anonymous user" [:button {:type "button"
                                           :on-click (wu/handle-dispatch [:login])} "Login"]])]])))

(defn menu-panel [box-filter]
  [:div.nav.flex-column.nav-pills
   [page-nav-link :page/home "Home"]
   [page-nav-link :page/betting "Betting"]
   [page-nav-link :page/my-matches "My matches"]
   [:hr]
   [sport-menu-panel box-filter]])

(defn omnifilter [box-filter]
  [:div
   [:label [:input {:type      "checkbox"
                    :checked   @(reaction (:prematch @box-filter false))
                    :on-change (wu/handle-dispatch [:toggle-prematch])}] "Prematch"]
   [:label [:input {:type      "checkbox"
                    :checked   @(reaction (:live @box-filter false))
                    :on-change (wu/handle-dispatch [:toggle-live])}] "Live"]
   [:label [:input {:type      "checkbox"
                    :checked   @(reaction (:results @box-filter false))
                    :on-change (wu/handle-dispatch [:toggle-results])}] "Results"]
   ])

(defn tab [current-tab tab label cmd]
  [:li.nav-item
   [:a.nav-link {:href     "#"
                 :class    (wu/classnames {"active" @(reaction (= tab @current-tab))})
                 :on-click (wu/handle-dispatch cmd)
                 } label]])
(defn home-page-tabs []
  (let [current-tab (re-frame/subscribe [:home-page/current-tab])]
    (fn []
      [:ul.nav.nav-tabs
       [tab current-tab :top-5 "Top 5" [:set-tab :top-5]]
       [tab current-tab :10-naj "10 naj" [:set-tab :10-naj]]
       [tab current-tab :superkurzy "Superkurzy" [:set-tab :superkurzy]]
       [tab current-tab :top-ponuka "Top ponuka" [:set-tab :top-ponuka]]])))

(defn box-panel [box-store]
  (let [box                (reagent/cursor box-store [:box])
        prematch-hierarchy (reagent/cursor box [:prematchHierarchy])]
    (fn [box-store]
      [:div
       (:name @box)
       [:ul (doall (for [group @prematch-hierarchy]
                     (doall (for [{:keys [date betHeaders] :as x} group]
                              ^{:key date}
                              [:li date
                               [:ul
                                (doall (for [{:keys [header sportEvents]} betHeaders]
                                         (doall (for [{:keys [marketId sportEventId betIds]} sportEvents]
                                                  (doall (for [bet-id betIds]
                                                           (let [bet @(reagent/cursor box-store [:sport-events sportEventId :bets bet-id :bet])]
                                                             ^{:key bet-id}
                                                             [:li (str/join " vs " (:participants bet))])))))))]]))))]])))
(defn boxes-panel [rows]
  (let [box-ids (reagent/cursor rows [:box-ids])]
    (fn [rows]
      [:ul
       (for [box-id @box-ids]
         [:li {:key box-id}
          [box-panel (reagent/cursor rows [:boxes box-id])]])])))