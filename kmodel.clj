(ns kmodel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;data structs;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Person [freq cash sched])

(defrecord State [people avail])

(defrecord Transaction [buyer oid])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;stats;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def stats (atom (sorted-map)))

(defn do-stats [st ts]
  (let [scheds (for [p (:people st)] (:sched p))
        amount (count (for [s scheds :when (= (val (first s)) :NEED_SITTER)] s))]
    (do
      (swap! stats assoc ts amount)
      st)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;model update functions;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn update-scheds [state ts]
  (reduce (fn [st id]
            (update-in st
                       [:people id :sched]
                       #(-> %
                            (dissoc (key (first %)))
                            (assoc ts (rand-nth [:NEED_SITTER
                                                 :NO_PLAN
                                                 :NO_PLAN])))))
          state
          (range (count (:people state)))))

(defn do-txn [st tx]
  (let [s_id (get-in st [:avail (:oid tx) :pid])
        b_id (:buyer tx)
        date (get-in st [:avail (:oid tx) :date])]
    (-> st
        (update-in [:people b_id :cash] dec)
        (assoc-in [:people b_id :sched date] :HAVE_SITTER)
        (update-in [:people s_id :cash] inc)
        (assoc-in [:people s_id :sched date] :SITTING)
        (update-in [:avail] #(dissoc % (:oid tx))))))


(defn buy [st pid]
  (let [aval (:avail st)
        prsn (get-in st [:people pid])
        schd (:sched prsn)
        need (into (sorted-map) (filter #(= (val %) :NEED_SITTER) schd))
        oids (for [d need]
               (first
                 (keys
                   (filter #(= (key d)
                               (:date (val %)))
                           aval))))
        txns (for [t (take (:cash prsn) oids)
                   :when (not (nil? t))]
                (Transaction. pid t))]
    (if (empty? txns)
      st
      (reduce do-txn st txns))))


(defn sell [st pid]
  (let [aval (:avail st)
        prsn (get-in st [:people pid])
        schd (:sched prsn)
        free (for [d schd :when (= (val d) :NO_PLAN)] (key d))]
    (if (empty? free)
      st
      (assoc-in st
                 [:avail]
                 (reduce (fn [a b]
                            (conj a {(inc (count a))
                                     {:pid pid :date b}}))
                          aval
                          free)))))


(defn behave-people [state]
  (letfn [(behave-person [st pid]
            (-> st
              (buy pid)
              (sell pid)))]
    (reduce
      behave-person
      state
      (shuffle (range (count (:people state)))))))


(defn step [state ts]
  (-> state
      (do-stats ts)
      (update-scheds ts)
      behave-people))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [ic]
  (let [popnsize 100
        schdsize 10
        initcash ic
        timestep 100
        initpopn (into []
                  (take popnsize
                    (repeatedly
                      #(let [freq (rand)
                             cash initcash
                             sched (reduce
                                      (fn [m i]
                                        (conj m
                                          {i (rand-nth [:NO_PLAN
                                                        :NEED_SITTER])}))
                                      (sorted-map)
                                      (range schdsize))]
                        (Person. freq cash sched)))))
        final    (reduce step
                         (State. initpopn (sorted-map))
                         (range schdsize timestep))]
    (println (format "TOTAL LOSSES %s " (apply + (vals @stats))))))

(time (doseq [cash '(0 1 2 3 4 5 10 50)]
        (println "cash: " cash)
        (dotimes [_ 10]
          (run cash))))
