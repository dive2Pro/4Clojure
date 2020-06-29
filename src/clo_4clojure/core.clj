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
  ((fn [ary index] (reduce #(concat %1 %2) [] (partition (- index 1) index [] ary)))
   prop-ary ind
   )
  )

(defn factorial [num]
  (
   (fn fac [total n] (if (= n 0) total (fac (* total n) (- n 1))))
   1 num
   )
  )


; 把 nums 分为 n 份
; 每一份中的数字间隔 n
;
; 以 n 个 array 组成一个集合
; 遍历 nums, 根据 当前的 index % n 的结果
; 将 nums[index] 放入该位置处的 array
; BUG: 只能更新单个item, 而且由于原子性, 这些修改不会影响到原来的 num-array
(defn reverse-interleave-failure [nums n]
  (let [num-array (map (fn [_a] []) (make-array Boolean/TYPE n))]
    (
      map-indexed (fn [i v]
                    (
                      assoc (nth num-array i) (count (nth num-array i)) v)
                    )
                  ) nums
    )
  )

; 如何把  map-indexed 和 reduce 进行结合呢?
(defn reverse-interleave [nums n]
    (let [i (volatile! -1)]
        (reduce (fn [p q] (do (vswap! i inc)
                              (println p (deref i) (count (nth p (mod (deref i) n))) q)
                        (assoc-in p [ (mod (deref i) n) (count (nth p (mod (deref i) n)))]  q) )
                        )
          (mapv (fn [_a] []) (make-array Boolean/TYPE n)) nums)
      )
  )





















