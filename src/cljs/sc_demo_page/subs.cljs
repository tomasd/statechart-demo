(ns sc-demo-page.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
  :name
  (fn [db]
    (get-in db [:name])))

(re-frame/reg-sub
  :page
  (fn [db _]
    (get-in db [:page])))

(re-frame/reg-sub
  :page-layout
  (fn [db _]
    (get-in db [:page :layout])))

(re-frame/reg-sub
  :current-page
  (fn [db _]
    (get-in db [:page :name])))

(re-frame/reg-sub
  :configuration
  (fn [db _]
    (get-in db [:sc/configuration])))