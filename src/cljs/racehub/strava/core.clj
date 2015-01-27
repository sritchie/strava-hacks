(ns racehub.strava.core
  (:require [schema.core :as s :include-macros true]))

;; Strava Schemas
;;
;; These are more for documentation than anything else.

(def WeekInterval "Example: 2015y05w" s/Str)
(def MonthInterval "Example: 2015y01m" s/Str)
(def Sport s/Str)

(s/defschema Activity
  {:id s/Int
   :name s/Str
   :description (s/maybe s/Str)
   :type Sport
   :workout_type (s/maybe s/Int)
   :trainer s/Int
   :start_date s/Int
   :utc_offset s/Int
   :moving_time s/Int
   :elapsed_time s/Int
   :distance s/Num
   :speed s/Num
   :elev_gain s/Int
   :avg_watts s/Int})

(s/defschema ActivityMap
  {:day_type s/Str
   :activities [Activity]
   :xt_activities [Activity]})

(s/defschema Goal
  {:type s/Str
   :goal s/Int})

(s/defschema Entry
  {:activities [Activity]
   :goal (s/maybe Goal)})

(s/defschema Interval
  {:id WeekInterval
   :month_interval MonthInterval
   :year s/Int
   :month s/Int
   :month_text (s/named s/Str "January, for instance.")
   :start_date s/Int
   :end_date s/Int
   :sport s/Str
   :display_type s/Str
   :moving_time s/Int
   :elapsed_time s/Int
   :distance s/Num
   :speed s/Int
   :elev_gain s/Int
   :entry Entry
   :by_day_of_week {s/Keyword ActivityMap}})

;; ## Code

(s/defn sports :- #{Sport}
  "Returns a set of "
  [m :- Interval]
  (set
   (map :type (-> m :entry :activities))))

(s/defn all-sports :- #{Sport}
  [ms :- [Interval]]
  (set (mapcat sports ms)))

(letfn [(extract [m k]
          (sequence (comp (map second)
                          (mapcat k))
                    (:by_day_of_week m)))]

  (s/defn activities :- [Activity]
    "Returns all activities that are in the active vector, not the cross
  training vector."
    [m :- Interval]
    (extract m :activities))

  (s/defn x-training :- [Activity]
    "Returns all activities that are in the active vector, not the cross
  training vector."
    [m :- Interval]
    (extract m :xt_activities)))

(s/defn filter-by-sport :- [Activity]
  "Returns a sequence of activities in the supplied set of intervals
  that match the supplied sport."
  [sport :- Sport ms :- [Interval]]
  (for [m ms, a (-> m :entry :activities)
        :when (= sport (:type a))]
    a))

(s/defn separate-by-sport
  :- {:day_type s/Str
      :activities [Activity]
      :xt_activities [Activity]}
  "The included sports are returned as activities. The rest are
  returned as cross-training activities. The function also sets a day
  type."
  [sports :- (s/either #{Sport} (s/eq :all))
   activities :- [Activity]]
  (let [separate (juxt filter remove)
        [a xt] (separate (if (= sports :all)
                           identity
                           (fn [a] (when (sports (:type a)) a)))
                         activities)]
    (assoc {:activities (vec a)
            :xt_activities (vec xt)}
      :day_type (cond (and (empty? a) (empty? xt)) "rest"
                      (empty? a) "active"
                      :else "training"))))

;; ## Updaters

(s/defn recompute-stats :- Interval
  [i :- Interval]
  (letfn [(agg [k] (transduce (map k) + (activities i)))]
    (assoc i
      :moving_time (agg :moving_time)
      :elapsed_time (agg :elapsed_time)
      :elev_gain (agg :elev_gain)
      :distance (agg :distance))))

(defn map-values
  "Maps the keyspace using the supplied function. Any duplicate keys
  will get knocked out in a nondeterministic order, so be careful!"
  [f m]
  (into {} (for [[k v] m]
             [k (f v)])))

(s/defn enable :- Interval
  "Enables activities with the sport type supplied. All other
  activities become cross-training."
  ([i :- Interval]
     (enable i :all))
  ([i :- Interval
    sports :- (s/either #{Sport} (s/eq :all))]
     (letfn [(swap-activities
               [{:keys [activities xt_activities] :as m}]
               (->> (into activities xt_activities)
                    (separate-by-sport sports)
                    (merge m)))]
       (recompute-stats
        (update i :by_day_of_week
                #(map-values swap-activities %))))))
