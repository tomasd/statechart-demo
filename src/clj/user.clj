(ns user
  (:require [statechart.core :as sc]
            [statechart.context :as ctx]
            [statechart.state :as state]
            [sc-demo-page.demo :as demo]
            [statechart.transition :as transition]
            [statechart.runtime :as runtime]))


(-> (sc/initialize {} demo/idx)
    (sc/process-event demo/idx [:goto-page :page/home])
    (sc/process-event demo/idx [:focus :first-name])

    ;(sc/process-event demo/idx [:toggle-live])
    ;(sc/process-event demo/idx [:login])
    )

