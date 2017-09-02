(ns sc-demo-page.statechart1
  (:require [clojure.zip :as zip]
            [clojure.set :as set]
            [sc-demo-page.state :as state]
            [sc-demo-page.context :as ctx]
            [sc-demo-page.transition :as transition]))


(defn select-event-transitions [state event]
  (concat (->> (state/state-transitions state)
               (filter #(= (transition/trigger-event %) event)))
          (lazy-seq (if-some [parent (state/parent-state state)]
                      (select-event-transitions parent event)))))

(defn select-eventless-transitions [state]
  (concat (->> (state/state-transitions state)
               (filter #(nil? (transition/trigger-event %))))
          (lazy-seq (if-some [parent (state/parent-state state)]
                      (select-eventless-transitions parent)))))

(defn state-zip [state]
  (zip/zipper
    (constantly true)
    state/substates
    (fn [state] (prn "edit"))
    state))

(defn make-transitions [state state-index]
  (let [{:keys [id transitions]} state]
    (cond-> state
      (some? transitions)
      (update :transitions
              (fn [transitions]
                (->> transitions
                     (mapv (fn [transition]
                             (let [{:keys [target]} transition]
                               (cond-> (assoc transition
                                         :source id
                                         :state-index state-index)
                                 (keyword? target)
                                 (assoc :target (conj id target)))))))))

      true (transition/new-transition))))

(defn index-states [state]
  (loop [loc (state-zip state)
         idx {}]
    (if (zip/end? loc)
      idx
      (let [value (zip/node loc)]
        (recur (zip/next loc)
               (assoc idx (:id value) value))))))




(defn make-statechart
  ([state]
   (let [state-index (state/new-index {})
         state       (make-statechart [] (assoc state :id []
                                                      :state-index state-index)
                                      state-index
                                      (volatile! 0))]
     (state/set-index state-index (index-states state))
     state))
  ([path state state-index entry-order]
   (let [order (vswap! entry-order inc)]
     (-> (reduce-kv (fn [state id substate]
                      (assoc-in state [:states id] (make-statechart (conj path id) substate state-index entry-order)))
                    state
                    (:states state))
         (assoc :id path
                :state-index state-index
                :order order)
         (make-transitions state-index)
         (state/new-state)))))

(defn effective-target-states [transition ctx]
  ; TODO Add history
  (->> (list (transition/target-state transition))
       (mapcat (fn [target]
                 (if (state/history? target)
                   (ctx/state-history target ctx)
                   [target])))))





(defn find-lcca [states]
  (let [[head & tail] states]
    (->> (state/proper-ancestors head nil)
         (filter #(or (state/compound? %) (state/component? %)))
         (filter (fn [ancestor]
                   (every? #(state/descendant? % ancestor) tail)))
         first)))

(defn transition-domain [t ctx]
  (let [states (effective-target-states t ctx)
        source (transition/source-state t)]
    (cond

      (empty? states)
      nil

      (and
        (transition/internal? t)
        (state/compound? source)
        (->> states
             (every? #(state/descendant? % source))))
      source

      :else
      (find-lcca (cons source states)))))



(defn transitions-exit-set [ctx enabled-transitions]
  (let [configuration (ctx/full-configuration ctx)]
    (->> enabled-transitions
         (filter transition/targetted-transition?)
         (mapcat (fn [t]
                   (let [domain (transition-domain t ctx)]
                     (->> configuration
                          (filter #(state/descendant? % domain))))))
         distinct)))

(declare add-descendants)
(defn add-ancestors [state ancestor ctx]
  (cons state (->> (state/proper-ancestors state ancestor)
                   (mapcat (fn [anc]
                             (cond-> [anc]
                               (state/component? anc)
                               (into (->> (state/substates anc)
                                          (mapcat #(add-descendants % ctx))))))))))
(defn add-descendants [state ctx]
  (cons state
        (if (state/history? state)
          (->> (ctx/state-history state ctx)
               (mapcat (fn [state]
                         (cons state (-> (concat
                                           (mapcat #(add-descendants % ctx) (state/initial-states state))
                                           (mapcat #(add-ancestors % state ctx) (state/initial-states state)))
                                         distinct)))))
          (cond
            (state/compound? state)
            (-> (concat
                  (mapcat #(add-descendants % ctx) (state/initial-states state))
                  (mapcat #(add-ancestors % state ctx) (state/initial-states state)))
                distinct)

            (state/component? state)
            (-> (mapcat #(add-descendants % ctx) (state/substates state))
                distinct)))))

(defn transitions-entry-set [ctx transitions]
  (->> transitions
       (mapcat (fn [t]
                 (let [ancestor               (transition-domain t ctx)
                       immediate-anc-children (->> (effective-target-states t ctx)
                                                   (map #(last (state/proper-ancestors % ancestor)))
                                                   (remove nil?))
                       other-children         (if (seq immediate-anc-children)
                                                (set/difference (into #{} (state/substates ancestor))
                                                                immediate-anc-children)
                                                [])
                       states                 (distinct (concat (add-descendants (transition/target-state t) ctx)
                                                                (->> (effective-target-states t ctx)
                                                                     (mapcat #(add-ancestors % ancestor ctx)))
                                                                ; this is different than scxml, we need to visit other children
                                                                ; from transition domain, as they are exited in trasitions-exit-set
                                                                (->> other-children
                                                                     (mapcat #(add-descendants % ctx)))))]
                   states)))
       (distinct)))



(defn update-configuration [ctx f & args]
  (-> (apply update-in ctx [:configuration] f args)
      (update-in [:configuration] #(->> %
                                        (filter state/atomic?)
                                        (into #{})))))


(defn invoke-executions [ctx executions]
  (reduce (fn [ctx execution]
            (execution ctx))
          ctx
          executions))

(defn exit-transitions-states [ctx enabled-transitions]
  (let [states-to-exit  (transitions-exit-set ctx enabled-transitions)
        exit-executions (->> states-to-exit
                             (sort-by state/entry-order)
                             (mapcat state/on-exit))]
    (-> ctx
        (ctx/save-history states-to-exit)
        (update-configuration clojure.set/difference (into #{} states-to-exit))
        (invoke-executions exit-executions))))

(defn enter-states [ctx states]
  (->> states
       ; sort by entry order = document order
       (sort-by state/entry-order)
       (reduce (fn [ctx state]
                 (-> ctx
                     (update-configuration (fnil conj #{}) state)
                     (invoke-executions (state/on-enter state))))
               ctx)))

(defn enter-transition-states [ctx enabled-transitions]
  (->> (transitions-entry-set ctx enabled-transitions)
       (enter-states ctx)))

(defn execute-transitions [ctx enabled-transitions]
  (let [executions (->> enabled-transitions
                        (mapcat transition/on-trigger))]
    (invoke-executions ctx executions)))

(defn microstep [ctx enabled-transitions]
  (-> ctx
      (exit-transitions-states enabled-transitions)
      (execute-transitions enabled-transitions)
      (enter-transition-states enabled-transitions)))



(defn xf-conflicting-transitions [ctx]
  (fn [xf]
    (let [filtered-transitions (volatile! #{})]
      (fn
        ([]
         (xf))
        ([acc]
         (->> @filtered-transitions
              (reduce xf acc)
              (xf)))
        ([acc t1]
         (let [transitions-to-remove (loop [filtered-transitions  @filtered-transitions
                                            transitions-to-remove #{}]
                                       (if-not (empty? filtered-transitions)
                                         (let [t2 (first filtered-transitions)]
                                           (if-not (empty? (clojure.set/intersection (into #{} (transitions-exit-set ctx [t1]))
                                                                                     (into #{} (transitions-exit-set ctx [t2]))))
                                             (if (state/descendant? (transition/source-state t1) (transition/source-state t2))
                                               (recur (rest filtered-transitions)
                                                      (conj transitions-to-remove t2))
                                               :preempted)))
                                         transitions-to-remove))]
           (if-not (= transitions-to-remove :preempted)
             (vswap! filtered-transitions #(-> %
                                               (clojure.set/difference transitions-to-remove)
                                               (conj t1))))
           acc))))))

(defn remove-conflicting-transitions [ctx transitions]
  (into [] (xf-conflicting-transitions ctx) transitions))

(defn select-transitions [ctx select-transitions]
  (->> (ctx/current-configuration ctx)
       (filter state/atomic?)
       (sort-by state/entry-order)
       (map (fn [atomic-state]
              (->> (select-transitions atomic-state)
                   (filter #(transition/applicable-transition? % ctx))
                   first)))
       (remove nil?)
       distinct
       (remove-conflicting-transitions ctx)))

(defn eventless-transitions [ctx]
  (select-transitions ctx select-eventless-transitions))
(defn event-transitions [ctx]
  (select-transitions ctx #(select-event-transitions % (ctx/current-event-id ctx))))

(defn- run [ctx]
  (loop [ctx (ctx/pop-event ctx)
         i   10]
    (let [enabled-transitions (eventless-transitions ctx)]
      (cond
        (zero? i)
        (do (prn "exit") ctx)

        (not (empty? enabled-transitions))
        (recur (microstep ctx enabled-transitions)
               (dec i))

        :else
        (let [value (event-transitions ctx)]
          (microstep ctx value))))))

(defn- make-result [ctx]
  {:fx            (:fx ctx)
   :configuration {:configuration (->> (ctx/full-configuration ctx)
                                       (sort-by state/entry-order)
                                       (map :id)
                                       (into []))
                   :history       (:history ctx)}})
(defn process-event [statechart configuration fx event]
  (let [ctx (loop [ctx (ctx/make-ctx statechart configuration fx event)]
              (if (seq (:internal-queue ctx))
                (let [ctx (run ctx)]
                  (recur ctx))
                ctx))]
    (make-result ctx)))


(defn initialize [statechart fx]
  (let [states (->> (state/initialize-statechart statechart)
                    (mapcat #(cons % (state/proper-ancestors % nil)))
                    (distinct)
                    )
        ctx    (enter-states (ctx/make-ctx statechart #{} fx) states)]
    (make-result ctx)))

(comment
  (def statechart
    {:type :and
     :states
           {:push {}
            :user {:type   :xor
                   :init   :anonymous
                   :states {:anonymous     {}
                            :authenticated {}}}
            :page {:type        :xor
                   :init        :home
                   :states      {:home    {}
                                 :betting {}}
                   :transitions [{:event  :goto-home
                                  :target :home}
                                 {:event  :goto-betting
                                  :target :betting}]
                   }}})

  )