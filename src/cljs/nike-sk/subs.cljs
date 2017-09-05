(ns nike-sk.subs
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub :page
  (fn [db]
    (get db :page)))

(re-frame/reg-sub :current-page-name
  :<- [:page]
  (fn [page _]
    (:name page)))

(re-frame/reg-sub :layout
  :<- [:page]
  (fn [page _]
    (:layout page)))

(re-frame/reg-sub :home-page
  (fn [db _]
    (:home-page db)))

(re-frame/reg-sub :home-page/current-tab
  :<- [:home-page]
  (fn [home-page]
    (:current-tab home-page)))

(re-frame/reg-sub :home-page/tab-boxes
  :<- [:home-page]
  (fn [home-page [_]]
    (get-in home-page [:box-store] {})))


(re-frame/reg-sub :betting-page
  (fn [db _]
    (:betting-page db)))
(re-frame/reg-sub :betting-page/boxes
  :<- [:betting-page]
  (fn [betting-page [_]]
    (get-in betting-page [:box-store] {})))
(re-frame/reg-sub :betting-page/filter
  :<- [:betting-page]
  (fn [betting-page [_]]
    (get-in betting-page [:filter] {})))

(re-frame/reg-sub :authenticated-user?
  (fn [db _]
    (get-in db [:user :authenticated?])))

(re-frame/reg-sub :menu
  (fn [db _]
    (:menu db)))

(re-frame/reg-sub :betting-days
  (fn [db _]
    (:betting-days db)))