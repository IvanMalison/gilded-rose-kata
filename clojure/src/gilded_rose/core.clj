(ns gilded-rose.core)

(defn item [item-name sell-in quality]
  {:name item-name :sell-in sell-in :quality quality})

(defprotocol Rule
  (get-next-quality [this item]))

(defrecord IncrementRule [rate]
  Rule
  (get-next-quality [this item]
    (+ (:quality item) (:rate this))))

(defrecord IncrementWithSellOutRule [rate]
  Rule
  (get-next-quality [this item]
    (let [multiplier (if (<= (:sell-in item) 0) 2 1)]
      (+ (:quality item) (* (:rate this) multiplier)))))

(defrecord RemappingRule [remap next]
  Rule
  (get-next-quality [this item]
    (let [next-quality (get-next-quality next item)]
      ((:remap this) next-quality))))

(defn ->ThresholdRule [operator threshold next]
  (->RemappingRule (fn [next-quality]
                      (if (operator next-quality threshold)
                        threshold
                        next-quality)) next))

(def ->NeverGreaterThan50 (partial ->ThresholdRule > 50))
(def ->NeverLessThan0 (partial ->ThresholdRule < 0))
(def ->DefaultRules (comp ->NeverGreaterThan50 ->NeverLessThan0))

(defrecord BackstagePassesRule []
  Rule
  (get-next-quality [this item]
                    (let [quality (:quality item)]
                      (condp > (:sell-in item)
                        0 0
                        6 (+ quality 3)
                        11 (+ quality 2)
                        (+ quality 1)))))

(defmulti get-next-quality-for-item :name)

(defmacro add-rule [item-name definition]
  (let [rule-name (gensym)]
    `(do
      (def ~rule-name ~definition)
      (defmethod get-next-quality-for-item ~item-name [~'item]
        (get-next-quality ~rule-name ~'item)))))


(add-rule "Conjured" (->DefaultRules (->IncrementWithSellOutRule -2)))
(add-rule "Aged Brie" (->DefaultRules (->IncrementRule 1)))
(add-rule "Backstage passes to a TAFKAL80ETC concert"
          (->DefaultRules (->BackstagePassesRule)))
(add-rule "Sulfuras, Hand Of Ragnaros"
          (reify Rule
            (get-next-quality [this item]
              (:quality item))))
(add-rule :default (->DefaultRules (->IncrementWithSellOutRule -1)))


(defn tick-item [item]
  (assoc item :sell-in (- (:sell-in item) 1)
         :quality (get-next-quality-for-item item)))

(defn update-quality [items]
  (map tick-item items))

(defn update-current-inventory[]
  (let [inventory
        [(item "Conjured" 2 1)
         (item "+5 Dexterity Vest" 10 20)
         (item "Aged Brie" 2 0)
         (item "Elixir of the Mongoose" 5 7)
         (item "Sulfuras, Hand Of Ragnaros" 0 80)
         (item "Backstage passes to a TAFKAL80ETC concert" 15 20)]]
    (update-quality inventory)))
