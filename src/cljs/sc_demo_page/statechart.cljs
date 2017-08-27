(ns sc-demo-page.statechart
  (:require [clojure.set]
            [clojure.set :as set]))

(defprotocol Hsm
  (initialize [this ctx])
  (dispatch [this ctx event]))

(defrecord EventCtx [idx internal-queue event ctx])

(defrecord StateMachine [id machine-id parent-id type states child-states transitions enter exit init order])
(defrecord ComponentMachine [id machine-id parent-id type states child-states transitions enter exit order])
(defrecord AtomicMachine [id machine-id parent-id type transitions enter exit order])
(declare Machine)

(defn current-configuration [ctx]
  (get-in ctx [:ctx :db :sc/configuration]))

(defn update-configuration [ctx f & args]
  (apply update-in ctx [:ctx :db :sc/configuration] f args))

(defn update-db [ctx f & args]
  (apply update-in ctx [:ctx :db] f args))

(defn pop-event [ctx]
  (let [event (-> ctx :internal-queue peek)]
    (-> ctx
        (assoc-in [:event] event)
        (update :internal-queue pop))))

(defn push-event [ctx event]
  (update ctx :internal-queue conj event))

(defn current-event [ctx]
  (get-in ctx [:event]))

(defn current-event-id [ctx]
  (first (current-event ctx)))

(defn make-event-ctx
  ([idx ctx]
   (->EventCtx idx cljs.core.PersistentQueue.EMPTY nil ctx))
  ([idx ctx event]
   (-> (make-event-ctx idx ctx)
       (push-event event))))


(defn entry-order [s1 s2]
  (compare (:order s1) (:order s2)))

(defn sort-by-entry-order [coll]
  (sort-by :order coll))

(defn sort-machine-ids-by-entry-order [idx coll]
  (->> coll
       (map #(get idx %))
       (sort-by-entry-order)
       (map :machine-id)))

(defn sort-by-document-order [coll]
  (sort-by :order coll))

(defn exit-order [coll]
  (->> (sort-by :order coll)
       reverse))


(defn child-states-ids [sm]
  (:child-states sm []))

(defn child-states [sm]
  (-> sm :states vals))

(defn compound-machine? [m]
  (contains? #{Machine StateMachine ComponentMachine} (type m)))

(defn atomic? [m]
  (instance? AtomicMachine m))

(defn component? [m]
  (instance? ComponentMachine m))

(defn compound? [m]
  (instance? StateMachine m))

(defn descendant? [child-id parent-id]
  (and
    (< (count parent-id) (count child-id))
    (= (->> (map vector child-id parent-id)
            (take-while #(apply = %))
            (map first))
       parent-id)))

(defn proper-ancestor-ids [state1 state2]
  (->> (range 0 (count state1))
       (map #(subvec state1 0 %))
       (reverse)
       (take-while #(not= % state2))))

(defn root-chain [state]
  (cons state (proper-ancestor-ids state nil)))

(defn proper-ancestors [idx state1 state2]
  (->> (proper-ancestor-ids (:machine-id state1) (:machine-id state2))
       (map idx)))

(defn effective-target-states [transition]
  ; TODO Add history
  (list (:target transition)))


(defn find-lcca [idx states]
  (let [[head & tail] states]
    (let [tail-ids (map :machine-id tail)]
      (->> (proper-ancestors idx head nil)
           (filter (comp compound-machine?))
           (filter (fn [ancestor]
                     (every? #(descendant? % (:machine-id ancestor)) tail-ids)))
           first
           ))))


(defn transition-domain [idx t]
  ; TODO add internal type
  (let [states (effective-target-states t)
        domain (cond

                 (empty? states)
                 nil

                 ; should be transition of type internal
                 #_(and false (or (:internal t) (descendant? (:target t) (:source t))))
                 #_(:internal t)
                 (or                                        ;(descendant? (:target t) (:source t))
                   (and
                     (:internal t false)
                     (compound? (get idx (:source t)))
                     (->> states
                          (every? #(descendant? % (:source t))))))
                 (get idx (:source t))

                 :else
                 (find-lcca idx (map (partial get idx) (cons (:source t) states))))]
    domain))


(defn initial-states [sm]
  (let [value (cond
                (compound? sm)
                (list (get (:states sm) (:init sm)))

                (component? sm)
                (map (:states sm) (:child-states sm))

                :else
                (list))]
    value))

(defn initial-states-seq [sm]
  (tree-seq compound-machine? initial-states
            (if (instance? Machine sm)
              (.-root sm)
              sm)))

(defn iterate-machines [machine]
  (tree-seq compound-machine? #(map (:states %) (:child-states %)) machine))


(defn init-configuration [idx]
  (into #{} (map :machine-id (initial-states-seq idx))))

(declare add-descendats)
(defn add-ancestors [idx state ancestor]
  (cons state (->> (proper-ancestors idx state ancestor)
                   (mapcat (fn [anc]
                             (cond-> [anc]
                               (component? anc)
                               (into (->> (child-states anc)
                                          (mapcat #(add-descendats idx %))))))))))
(defn add-descendats [idx state]
  (cons state (cond
                (compound? state)
                (concat
                  (mapcat #(add-descendats idx %) (initial-states state))
                  (mapcat #(add-ancestors idx % state) (initial-states state)))

                (component? state)
                (mapcat #(add-descendats idx %) (child-states state)))))

(defn transitions-entry-set [idx transitions]
  (->> transitions
       (mapcat (fn [{:keys [target source] :as t}]
                 (let [ancestor               (transition-domain idx t)
                       immediate-anc-children (->> (effective-target-states t)
                                                   (map #(get idx %))
                                                   (map #(last (proper-ancestors idx % ancestor)))
                                                   (remove nil?)
                                                   (map :machine-id)
                                                   )
                       other-children         (if (seq immediate-anc-children)
                                                (set/difference (into #{} (map :machine-id (child-states ancestor)))
                                                                                               immediate-anc-children)
                                                [])
                       states                 (distinct (concat (add-descendats idx (get idx target))
                                                                (->> (effective-target-states t)
                                                                     (map #(get idx %))
                                                                     (mapcat #(add-ancestors idx % ancestor)))
                                                                ; this is different than scxml, we need to visit other children
                                                                ; from transition domain, as they are exited in trasitions-exit-set
                                                               (->> other-children
                                                                     (map #(get idx %))
                                                                     (mapcat #(add-descendats idx %)))))]
                   states)))))





(defn machines-index [machine]
  (->> (conj (mapcat iterate-machines (-> machine :states vals))
             machine)
       (map (juxt :machine-id identity))
       (into {})))


(def last-order (atom 0))
(defmulti make-state-machine (fn [parent-id sm]
                               (cond
                                 (empty? (:states sm)) :atomic
                                 :else (:type sm))))


(defn ->absolute-id [id parent-id]
  (if-not (vector? id)
    (conj parent-id id)
    id))


(defn- make-transitions [transitions parent-id machine-id]
  (mapv (fn [m]
          (let [{:keys [source target]} m]
            (cond-> (assoc m :source machine-id)
              target (update :target ->absolute-id parent-id))))
        transitions))


(defn- make-states [sm machine-id]
  (let [states (->> (:states sm)
                    (map (fn [[sub-id machine]]
                           (make-state-machine machine-id (assoc machine :id sub-id)))))]
    (-> sm
        (assoc :child-states (->> states
                                  (mapv :id))
               :states (->> states
                            (map (juxt :id identity))
                            (into {})))
        (update :transitions make-transitions machine-id machine-id))))

(defmethod make-state-machine :and [parent-id sm]
  (let [machine-id (conj parent-id (:id sm))]
    (-> sm
        (assoc :order (swap! last-order inc)
               :parent-id parent-id
               :machine-id machine-id)
        (make-states machine-id)
        (map->ComponentMachine))))

(defmethod make-state-machine :xor [parent-id sm]
  (let [machine-id (conj parent-id (:id sm))]
    (-> sm
        (assoc :order (swap! last-order inc)
               :parent-id parent-id
               :machine-id machine-id)
        (make-states machine-id)
        (map->StateMachine))))

(defmethod make-state-machine :atomic [parent-id sm]
  (let [machine-id (conj parent-id (:id sm))]
    (-> sm
        (assoc :type :atomic
               :order (swap! last-order inc)
               :machine-id machine-id
               :parent-id parent-id)
        (update :transitions make-transitions parent-id machine-id)
        (dissoc :states)
        (map->AtomicMachine))))

(defn in-state? [state]
  (fn [ctx]
    (contains? (current-configuration ctx) state)))
(defn not-in-state? [state]
  (fn [ctx]
    (not (contains? (current-configuration ctx) state))))
(defn event-pred? [predicate]
  (fn [ctx]
    (predicate (current-event ctx))))

(defn raise [event]
  (fn [ctx]
    (update-in ctx [:ctx :dispatch-n] (fnil conj []) event)))


(defn invoke-executions [ctx executions]
  (reduce (fn [ctx execution]
            (execution ctx))
          ctx
          executions))

(defn enter-states' [ctx states]
  (->> states
       ; sort by entry order = document order
       (sort-by-entry-order)
       (reduce (fn [ctx state]
                 (-> ctx
                     (update-configuration (fnil conj #{}) (:machine-id state))
                     (invoke-executions (:enter state))))
               ctx)))

(defn enter-states [ctx enabled-transitions]
  (->> (transitions-entry-set (:idx ctx) enabled-transitions)
       (enter-states' ctx)))





(defn transition-exit-set [idx configuration enabled-transitions]
  (->> enabled-transitions
       (remove (comp nil? :target))
       (mapcat (fn [t]
                 (let [domain-id (-> (transition-domain idx t)
                                     (:machine-id))]
                   (filter #(descendant? % domain-id) configuration))))
       (map (partial get idx))))


(defn exit-states [ctx enabled-transitions]
  (let [{:keys [idx]} ctx
        states-to-exit  (transition-exit-set idx (current-configuration ctx) enabled-transitions)
        exit-executions (->> states-to-exit
                             (remove (comp nil? :exit))
                             (sort-by-entry-order)
                             (mapcat :exit)
                             )]
    (-> ctx
        (update-configuration clojure.set/difference (map :machine-id states-to-exit))
        (invoke-executions exit-executions))))

(defn execute-transitions [ctx enabled-transitions]
  (let [executions (->> enabled-transitions
                        (mapcat :execute)
                        (remove nil?))]
    (invoke-executions ctx executions)))

(defn microstep [ctx enabled-transitions]
  (-> ctx
      (exit-states enabled-transitions)
      (execute-transitions enabled-transitions)
      (enter-states enabled-transitions)))

(defn applicable-transition? [ctx transition]
  (let [event-id (current-event-id ctx)
        {:keys [condition]} transition]
    (and (= event-id (:event transition))
         (or (nil? condition)
             (condition ctx)))))

(defn applicable-eventless-transition? [ctx transition]
  (let [{:keys [condition]} transition]
    (and (nil? (:event transition nil))
         (or (nil? condition)
             (condition ctx)))))



(defn xf-conflicting-transitions [idx configuration]
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
                                           (if-not (empty? (clojure.set/intersection (into #{} (map :machine-id) (transition-exit-set idx configuration [t1]))
                                                                                     (into #{} (map :machine-id) (transition-exit-set idx configuration [t2]))))
                                             (if (descendant? (:source t1) (:source t2))
                                               (recur (rest filtered-transitions)
                                                      (conj transitions-to-remove t2))
                                               :preempted)
                                             ))
                                         transitions-to-remove))]
           (if-not (= transitions-to-remove :preempted)
             (vswap! filtered-transitions #(-> %
                                               (clojure.set/difference transitions-to-remove)
                                               (conj t1))))
           acc))))))

(defn remove-conflicting-transitions [idx configuration transitions]
  (into [] (xf-conflicting-transitions idx configuration) transitions))

(defn select-eventless-transitions [ctx]
  (let [configuration (current-configuration ctx)
        idx           (:idx ctx)]
    (->> configuration
         (map (partial get idx))
         (filter atomic?)
         (sort-by-document-order)
         (map (fn [atomic-state]
                (->> (cons atomic-state (proper-ancestors idx atomic-state nil))
                     (mapcat :transitions)
                     (filter (partial applicable-eventless-transition? ctx))
                     first)))
         (remove nil?)
         distinct
         (remove-conflicting-transitions idx configuration))))

(defn select-transitions [ctx]
  (let [configuration (current-configuration ctx)
        idx           (:idx ctx)]
    (->> configuration
         (map (partial get idx))
         (filter atomic?)
         (sort-by-document-order)
         (map (fn [atomic-state]
                (->> (cons atomic-state (proper-ancestors idx atomic-state nil))
                     (mapcat :transitions)
                     (filter (partial applicable-transition? ctx))
                     first)))
         (remove nil?)
         distinct
         (remove-conflicting-transitions idx configuration))))


(defn process-event [idx ctx event]
  (let [ctx (make-event-ctx idx ctx event)
        ctx (loop [ctx ctx]
              (if (seq (:internal-queue ctx))
                (let [ctx (loop [ctx (pop-event ctx)]
                            (let [enabled-transitions (select-eventless-transitions ctx)]
                              (if-not (empty? enabled-transitions)
                                (recur (microstep ctx enabled-transitions))
                                (let [value (select-transitions ctx)]
                                  (microstep ctx value)))))]
                  (recur ctx))
                ctx))]
    (-> ctx
        (:ctx)
        ;; TODO toto tu asi nema byt, event by mal byt mimo :ctx
        (dissoc :event))))



(deftype Machine [root index]
  ILookup
  (-lookup
    [this key]
    (get index key))
  (-lookup
    [this key not-found]
    (get index key not-found))

  IFn
  (-invoke [this key]
    (get index key))
  (-invoke [this key not-found]
    (get index key not-found))

  Hsm
  (initialize [this ctx]
    (let [states (init-configuration this)]
      (-> {:ctx ctx}
          (enter-states' (map #(get this %) states))
          :ctx)))
  (dispatch [this ctx event]
    (process-event this ctx event)))

(defn make-machine [machine]
  (let [constructor (condp = (:type machine)
                      :xor map->StateMachine
                      :and map->ComponentMachine
                      map->StateMachine
                      )
        root        (constructor (make-states (merge {:id         []
                                                      :machine-id []
                                                      :parent-id  []
                                                      :order      (swap! last-order inc)}
                                                     machine) []))]
    (->Machine root
               (machines-index root))))

(comment
  (let [idx         (make-machine sm)
        init-config (init-configuration idx)
        ctx         (initialize idx {:db {:value 1}})]

    ; TODO Tu by sa nemalo nic stat, lebo :show-all je v :all je bez targetu
    (time (doall (->> [[:set-date nil]
                       [:set-menu-item nil]
                       [:show-all nil]]
                      (reductions (partial dispatch idx) ctx)
                      (map #(get-in % [:configuration]))
                      (map (fn [values]
                             (->> values
                                  (filter #(atomic? (get idx %)))
                                  vec))))))))





