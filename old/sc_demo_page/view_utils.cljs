(ns sc-demo-page.view-utils
  (:require [clojure.string :as str]
            [re-frame.core :as re-frame]))

(defn classnames [m]
  (->> m
       (filter val)
       keys
       (str/join " ")))

(defn handle-dispatch [cmd]
  (fn [e]
    (re-frame/dispatch cmd)
    (.preventDefault e)))