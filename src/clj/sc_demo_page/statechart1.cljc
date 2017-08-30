(ns sc-demo-page.statechart1
  (:require [clojure.zip :as zip]
            [clojure.set :as set]))

(defprotocol State
  (initialize-statechart [this])
  (initial-states [this])
  (substates [this])
  (state-id [this])
  (state-type [this])
  (state-transitions [this])
  (entry-order [this])
  (on-exit [this])
  (on-enter [this])
  (get-state [this id]))

(defprotocol Transition
  (trigger-event [this])
  (target-state [this])
  (source-state [this])
  (internal? [this])
  (on-trigger [this])
  (applicable-transition? [this ctx]))

(defn compound? [state]
  (= (state-type state) :xor))

(defn component? [state]
  (= (state-type state) :and))

(defn atomic? [state]
  (not (or (compound? state)
           (component? state))))


(defn current-event [ctx]
  (:current-event ctx))

(defn current-event-id [ctx]
  (first (current-event ctx)))

(defn pop-event [ctx]
  (let [event (-> ctx :internal-queue peek)]
    (-> ctx
        (assoc :current-event event)
        (update :internal-queue pop))))

(defn make-ctx [configuration fx event]
  {:configuration  (->> configuration
                        (filter atomic?)
                        (into #{}))
   :internal-queue (conj clojure.lang.PersistentQueue/EMPTY event)
   :fx             fx})

(extend-type
  clojure.lang.PersistentArrayMap

  Transition
  (trigger-event [this]
    (:event this))
  (target-state [this]
    (get-state this (:target this)))
  (source-state [this]
    (get-state this (:source this)))
  (internal? [this]
    (:internal this false))
  (on-trigger [this]
    (:execute this))
  (applicable-transition? [this ctx]
    (let [event-id       (current-event-id ctx)
          {:keys [condition]} this
          trigger-event' (trigger-event this)]
      (and (or (nil? trigger-event')
               (= event-id trigger-event'))
           (or (nil? condition)
               (condition ctx)))))

  State
  (initialize-statechart [this]
    (case (:type this)
      :and
      (mapcat initialize-statechart (vals (:states this)))

      :xor
      (initialize-statechart (get-in this [:states (:init this)]))

      [this]))
  (initial-states [this]
    (case (:type this)
      :xor
      (list (get-in this [:states (:init this)]))

      :and
      (vals (:states this))

      (list)))
  (substates [this]
    (vals (:states this)))
  (state-id [this]
    (:id this))
  (state-type [this]
    (:type this))
  (state-transitions [this]
    (:transitions this))
  (entry-order [this]
    (:order this))
  (on-exit [this]
    (:exit this))
  (on-enter [this]
    (:enter this))
  (get-state [this id]
    (get (:state-index this) id)))

(defn targetted-transition? [transition]
  (some? (target-state transition)))

(defn parent-id [state]
  (let [id (state-id state)]
    (cond
      (= id []) nil
      (= (count id) 1) []

      :else (->> id
                 (butlast)
                 vec))))

(defn parent-state [state]
  (let [{:keys [state-index]} state]
    (get state-index (parent-id state))))

(defn select-event-transitions [state event]
  (concat (->> (state-transitions state)
               (filter #(= (trigger-event %) event)))
          (lazy-seq (if-some [parent (parent-state state)]
                      (select-event-transitions parent event)))))

(defn select-eventless-transitions [state]
  (concat (->> (state-transitions state)
               (filter #(nil? (trigger-event %))))
          (lazy-seq (if-some [parent (parent-state state)]
                      (select-eventless-transitions parent)))))

(defn state-zip [state]
  (zip/zipper
    (constantly true)
    substates
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
                                             (assoc :target (conj id target))))))))))))

(defn index-states [state]
  (loop [loc (state-zip state)
         idx {}]
    (if (zip/end? loc)
      idx
      (let [value (zip/node loc)]
        (recur (zip/next loc)
               (assoc idx (:id value) value))))))

(defprotocol IStatechartIndex
  (set-index [this idx]))
(deftype StatechartIndex [^:volatile-mutable idx]
  IStatechartIndex
  (set-index [this index]
    (set! idx index))
  clojure.lang.ILookup
  (valAt [o k]
    (get idx k))
  (valAt [o k not-found]
    (get idx k not-found)))

(defn make-statechart
  ([state]
   (let [state-index (StatechartIndex. {})
         state       (make-statechart [] (assoc state :id []
                                                      :state-index state-index)
                                      state-index
                                      (volatile! 0))]
     (set-index state-index (index-states state))
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
         (make-transitions state-index)))))
(defn effective-target-states [transition]
  ; TODO Add history
  (list (target-state transition)))

(defn descendant? [child parent]
  (let [parent-id (when parent
                    (state-id parent))
        child-id  (state-id child)]
    (and
      (< (count parent-id) (count child-id))
      (= (->> (map vector child-id parent-id)
              (take-while #(apply = %))
              (map first))
         parent-id))))

(defn proper-ancestors [state1 cap-state]
  (let [state1-id (state-id state1)
        state2-id (when (some? cap-state)
                    (state-id cap-state))]
    (->> (range 0 (count state1-id))
         (map #(subvec state1-id 0 %))
         (reverse)
         (take-while #(not= % state2-id))
         (map #(get-state state1 %)))))

(defn find-lcca [states]
  (let [[head & tail] states]
    (->> (proper-ancestors head nil)
         (filter #(or (compound? %) (component? %)))
         (filter (fn [ancestor]
                   (every? #(descendant? % ancestor) tail)))
         first)))

(defn transition-domain [t]
  ; TODO add internal type
  (let [states (effective-target-states t)
        source (source-state t)]
    (cond

      (empty? states)
      nil

      (and
        (internal? t)
        (compound? source)
        (->> states
             (every? #(descendant? % source))))
      source

      :else
      (find-lcca (cons source states)))))

(defn full-configuration [configuration]
  (->> configuration
       (mapcat #(cons % (proper-ancestors % nil)))
       (distinct)))

(defn transitions-exit-set [configuration enabled-transitions]
  (let [configuration (full-configuration configuration)]
    (->> enabled-transitions
         (filter targetted-transition?)
         (mapcat (fn [t]
                   (let [domain (transition-domain t)]
                     (->> configuration
                          (filter #(descendant? % domain))))))
         distinct)))

(declare add-descendants)
(defn add-ancestors [state ancestor]
  (cons state (->> (proper-ancestors state ancestor)
                   (mapcat (fn [anc]
                             (cond-> [anc]
                                     (component? anc)
                                     (into (->> (substates anc)
                                                (mapcat #(add-descendants %))))))))))
(defn add-descendants [state]
  (cons state (cond
                (compound? state)
                (-> (concat
                      (mapcat #(add-descendants %) (initial-states state))
                      (mapcat #(add-ancestors % state) (initial-states state)))
                    distinct)

                (component? state)
                (-> (mapcat #(add-descendants %) (substates state))
                    distinct))))

(defn transitions-entry-set [transitions]
  (->> transitions
       (mapcat (fn [t]
                 (let [ancestor               (transition-domain t)
                       immediate-anc-children (->> (effective-target-states t)
                                                   (map #(last (proper-ancestors % ancestor)))
                                                   (remove nil?))
                       other-children         (if (seq immediate-anc-children)
                                                (set/difference (into #{} (substates ancestor))
                                                                immediate-anc-children)
                                                [])
                       states                 (distinct (concat (add-descendants (target-state t))
                                                                (->> (effective-target-states t)
                                                                     (mapcat #(add-ancestors % ancestor)))
                                                                ; this is different than scxml, we need to visit other children
                                                                ; from transition domain, as they are exited in trasitions-exit-set
                                                                (->> other-children
                                                                     (mapcat #(add-descendants %)))))]
                   states)))
       (distinct)))

(defn current-configuration [ctx]
  (get-in ctx [:configuration]))

(defn update-configuration [ctx f & args]
  (-> (apply update-in ctx [:configuration] f args)
      (update-in [:configuration] #(->> %
                                        (filter atomic?)
                                        (into #{})))))


(defn invoke-executions [ctx executions]
  (reduce (fn [ctx execution]
            (execution ctx))
          ctx
          executions))

(defn exit-states [ctx enabled-transitions]
  (let [states-to-exit  (transitions-exit-set (current-configuration ctx) enabled-transitions)
        exit-executions (->> states-to-exit
                             (sort-by entry-order)
                             (mapcat on-exit))]
    (-> ctx
        (update-configuration clojure.set/difference (into #{} states-to-exit))
        (invoke-executions exit-executions))))

(defn enter-states [ctx enabled-transitions]
  (->> (transitions-entry-set enabled-transitions)
       ; sort by entry order = document order
       (sort-by entry-order)
       (reduce (fn [ctx state]
                 (-> ctx
                     (update-configuration (fnil conj #{}) state)
                     (invoke-executions (on-enter state))))
               ctx)))

(defn execute-transitions [ctx enabled-transitions]
  (let [executions (->> enabled-transitions
                        (mapcat on-trigger))]
    (invoke-executions ctx executions)))

(defn microstep [ctx enabled-transitions]
  (-> ctx
      (exit-states enabled-transitions)
      (execute-transitions enabled-transitions)
      (enter-states enabled-transitions)))



(defn xf-conflicting-transitions [configuration]
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
                                           (if-not (empty? (clojure.set/intersection (into #{} (transitions-exit-set configuration [t1]))
                                                                                     (into #{} (transitions-exit-set configuration [t2]))))
                                             (if (descendant? (:source t1) (:source t2))
                                               (recur (rest filtered-transitions)
                                                      (conj transitions-to-remove t2))
                                               :preempted)))
                                         transitions-to-remove))]
           (if-not (= transitions-to-remove :preempted)
             (vswap! filtered-transitions #(-> %
                                               (clojure.set/difference transitions-to-remove)
                                               (conj t1))))
           acc))))))

(defn remove-conflicting-transitions [configuration transitions]
  (into [] (xf-conflicting-transitions configuration) transitions))

(defn select-transitions [ctx select-transitions]
  (->> (current-configuration ctx)
       (filter atomic?)
       (sort-by entry-order)
       (map (fn [atomic-state]
              (->> (select-transitions atomic-state)
                   (filter #(applicable-transition? % ctx))
                   first)))
       (remove nil?)
       distinct
       (remove-conflicting-transitions (full-configuration (current-configuration ctx)))))

(defn process-event [configuration fx event]
  (let [ctx (loop [ctx (make-ctx configuration fx event)]
              (if (seq (:internal-queue ctx))
                (let [ctx (loop [ctx (pop-event ctx)
                                 i 10]
                            (let [enabled-transitions (select-transitions ctx select-eventless-transitions)]
                              (cond
                                (zero? i)
                                (do (prn "exit") ctx)

                                (not (empty? enabled-transitions))
                                (recur (microstep ctx enabled-transitions)
                                       (dec i))

                                :else
                                (let [value (select-transitions ctx #(select-event-transitions % (current-event-id ctx)))]
                                  (microstep ctx value)))))]
                  (recur ctx))
                ctx))]
    (select-keys ctx [:fx :configuration])))

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