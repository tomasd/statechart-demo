(ns statechart.path)

(defn parent [path]
  (-> path
      (vary-meta assoc ::relative? true)
      (vary-meta update ::parents (fnil inc 0))))

(defn sibling [path]
  (parent [path]))

(defn child [path]
  (-> path
      (with-meta {::children? true})))

(defn this []
  (child []))

(defn resolve-path [parent path]
  (cond
    (-> path meta ::relative?)
    (into (subvec parent 0 (- (count parent)
                              (-> path meta ::parents)))
          path)

    (-> path meta ::children?)
    (into parent path)

    (or (keyword? path)
        (-> path meta ::sibling?))
    (conj parent path)

    :else
    path))