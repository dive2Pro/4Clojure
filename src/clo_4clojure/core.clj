(ns clo-4clojure.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn depn [x]
  (reduce (fn [p q]
            (if (first p)
              (if (= (last p) q)
                p
                (conj p q)
                )
              [q]
              )
            ) [] x)
  )

(defn pack_seq_dup [x]
  (reduce (fn [p q]
            (if
              (do (println p q)
                  (= (last (last p)) q)                     ;如果最后一个集合的最后一个数字和当前的 q 相同
                  )
              (conj (vec (drop-last p)) (conj (first (take-last 1 p)) q)) ; 放入相同集合
              (conj p [q])                                  ; 使用 q 构造一个新的集合
              )
            ) [] x)

  )


(defn pack_repeat [x]
  (reduce #(conj %1 %2 %2) [] x)
  )

(defn pack_repeat_x [arr times]
  (let [repeat (fn [val count]
                 (map (fn [_v] val) (range count))
                 )]
    (reduce #(concat %1 (repeat %2 times)) [] arr)
    )
  )
; find a way that works like js.bind
(defn pack_repeat_x2 [arr times]
  (let [repeat (fn [val count]
                 (map (fn [_v] val) (range count))
                 )]
      (mapcat #(repeat %1 times) arr)
    )
  )

(defn my-range [low high] (
                take (- high low) (iterate inc low)
                            ))

(defn my-max [& s]
  (last (sort s))
  )

(defn my-interleave [x-array y-array]
  (let [largest (min (count x-array) (count y-array))]
    (reduce (fn [p x]
              (concat p [(nth x-array x) (nth y-array x)])
              ) [] (range 0 largest))
  )
  )


(defn my-interpose [x ary]

    (cons (first ary)
          (mapcat
            (fn [y] [x y])
            (drop 0 ary)
            )
          )
  )

(defn drop-every-nth [prop-ary ind]
  ;(
  ;  let [x -1]
  ;  (filter #(do (inc x) (= 0 (% x index))) ary)
  ;  )
  ( (fn [ary index] (reduce #(concat %1 %2) [] (partition (- index 1)  index [] ary) ))
  prop-ary ind
   )
  )





















