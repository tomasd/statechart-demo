(ns nike-sk.components
  (:require
    [reagent.ratom :refer-macros [reaction]]
    [re-frame.core :as re-frame]
    [nike-sk.view-utils :as wu]
    [reagent.core :as reagent]
    [clojure.string :as str]
    [cljs-time.format :as date-format]))

(def custom-formatter (date-format/formatter "dd.MM."))
(defn layout-toggle []
  (let [layout (re-frame/subscribe [:layout])]
    (fn []
      [:div.btn-group
       [:label.btn
        [:input {:type      "radio"
                 :checked   (= :layout/column-3 @layout)
                 :on-change (wu/handle-dispatch [:toggle-layout])}]
        "3 columns"]
       [:label.btn
        [:input {:type      "radio"
                 :checked   (= :layout/column-4 @layout)
                 :on-change (wu/handle-dispatch [:toggle-layout])}]
        "4 columns"]])))

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
  (let [has-menu? (reaction (:menu @box-filter))]
    [:div
     [:button.btn.btn-secondary
      {:type     "button"
       :disabled (not @has-menu?)
       :on-click (wu/handle-dispatch [:clear-menu])}
      "× All"]
     (let [menu (re-frame/subscribe [:menu])]
       [:ul
        (for [[i item] (map-indexed vector (:items @menu))]
          ^{:key i} [sport-menu-item box-filter item])])]))

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
          [:div "Authenticated user" [:button {:type     "button"
                                               :on-click (wu/handle-dispatch [:logout])} "Logout"]]
          [:div "Anonymous user" [:button {:type     "button"
                                           :on-click (wu/handle-dispatch [:login])} "Login"]])]])))

(defn menu-panel []
  [:div.nav.flex-column.nav-pills
   [page-nav-link :page/home "Home"]
   [page-nav-link :page/betting "Betting"]
   [page-nav-link :page/my-matches "My matches"]
   ])

(defn day-radio [box-filter day]
  [:label.btn.btn-secondary [:input {:type      "radio"
                                     :checked   @(reaction (= (:date @box-filter) day))
                                     :on-change (wu/handle-dispatch [:set-date day])}] (date-format/unparse custom-formatter day)])
(defn clear-date [box-filter]
  (let [has-date? (reaction (some? (:date @box-filter)))]
    [:button.btn.btn-secondary {:type      "button"
                                :disabled  (not @has-date?)
                                :on-click (wu/handle-dispatch [:clear-date])} "× All"]))

(defn omnifilter [box-filter]
  (let [betting-days (re-frame/subscribe [:betting-days])
        ]
    [:div.row
     [:div.col-8
      [:div.btn-toolbar
       [:div.btn-group.mr-2
        [:label.btn.btn-secondary [:input {:type      "checkbox"
                                           :checked   @(reaction (:prematch @box-filter false))
                                           :on-change (wu/handle-dispatch [:toggle-prematch])}] " Prematch"]

        [:label.btn.btn-secondary [:input {:type      "checkbox"
                                           :checked   @(reaction (:live @box-filter false))
                                           :on-change (wu/handle-dispatch [:toggle-live])}] " Live"]

        [:label.btn.btn-secondary [:input {:type      "checkbox"
                                           :checked   @(reaction (:results @box-filter false))
                                           :on-change (wu/handle-dispatch [:toggle-results])}] " Results"]]]
      [:div.btn-group.mr-2 {:style {:max-width      "200px"
                                    :overflow-y "scroll"}}
       (for [[i day] (map-indexed vector @betting-days)]
         ^{:key i}
         [day-radio box-filter day])]

      [:div.btn-group
       [clear-date box-filter]]]





     [:div.col
      [layout-toggle]]
     ]))

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
                     (doall (for [[i {:keys [date betHeaders] :as x}] (map-indexed vector group)]
                              ^{:key (str i date)}
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

