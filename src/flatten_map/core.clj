(ns flatten-map.core
  (:require [cheshire.core :as jp])
  (:gen-class))

(defn get-key
  "Returns key after appending prefix"
  [prefix key]
  (if (nil? prefix)
    key
    (str prefix "_" key)))

(defn flatten-kvs
  "Flatten the map and returns result"
  ([m] (flatten-kvs m nil))
  ([m prefix]
   (reduce
     (fn [l [k v]]
       (cond
         (map? v) (concat l (flatten-kvs v (get-key prefix (name k))))
         (vector? v) (flatten (concat l
                                (if (zero? (count v))
                                  [(get-key prefix (name k)) nil]
                                  (mapv
                                    #(concat [] (if (or (map? %) (vector? %))
                                                  (flatten-kvs % (get-key prefix (name k)))
                                                  (conj l [(get-key prefix (name k)) %])))
                                   v))))
         :else (concat l [(get-key prefix (name k)) v])))
    [] m)))

(defn list->map
  "Converts a list into map by pairing up elements"
  [lst]
  (->> (partition-all 2 lst)
       (group-by first)
       (mapv vec)
       (mapv (fn [[k v]] [k (mapv second v)]))
       (into {})
       (reduce
         (fn [l [k v]]
           (assoc l k (if (and (vector? v) (= 1 (count v)))
                        (first v)
                        v)))
         {})))

(comment
  (def input [{:id "RANDOM", :type "donut", :name "Cake", :ppu 0.55,
               :batters {:batter
                         [{:id "1001", :type "Regular"}
                          {:id "1002", :type "Chocolate"}
                          {:id "1003", :type "Blueberry"}
                          {:id "1004", :type "Devil's Food"},]}
               :topping [{:id "5001", :type "None"}
                         {:id "5002", :type "Glazed"}
                         {:id "5005", :type "Sugar"}
                         {:id "5007", :type "Powdered Sugar"}
                         {:id "5006", :type "Chocolate with Sprinkles"}
                         {:id "5003", :type "Chocolate"}
                         {:id "5004", :type "Maple"}]}])

  (if (vector? input)
    (mapv #(->> (flatten-kvs %) list->map) input)
    (list->map (flatten-kvs input))))

