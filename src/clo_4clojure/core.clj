(ns clo-4clojure.core
  (:require [clojure.string :as str]))
(require 'clojure.set)

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
(defn reverse-interleave-use-volatile [nums n]
  (let [i (volatile! -1)]
    (reduce (fn [p q] (do (vswap! i inc)
                          (println p (deref i) (count (nth p (mod (deref i) n))) q)
                          (assoc-in p [(mod (deref i) n) (count (nth p (mod (deref i) n)))] q))
              )
            (mapv (fn [_a] []) (make-array Boolean/TYPE n)) nums)
    )
  )


(defn reverse-interleave [nums n]
  (reduce (fn [p q]
            (assoc-in p [(mod (get q 0) n) (count (nth p (mod (get q 0) n)))] (get q 1)))
          (mapv (fn [_a] []) (make-array Boolean/TYPE n))
          (map-indexed (fn [i v] [i v]) nums)
          )
  )


(defn exc [coll n]
  (let [exc-pos (mod n (count coll))]
    (concat (drop exc-pos coll) (take exc-pos coll))
    )
  )

; split a sequence
(defn split-sequence [n coll] (
                                list (take n coll)
                                     (drop n coll)
                                     ))


; split by type

(defn split-by-type [coll]
  (vals (group-by type coll))
  )

;(let [[a b & c :as d] [1 2 3 4 5]] [ a b c d])
;(+ (first (last p)) 1)
(defn longest-increasing-seq [coll]
  (let [
        v
        (
          reduce (fn [p q]
                   (if
                     (empty? (last p))
                     [[q] [q]]
                     (if (< (last (last p)) q)
                       [(first p) (conj (last p) q)]
                       (if (< (count (first p)) (count (last p)))
                         [(last p) [q]]
                         [(first p) [q]]
                         )
                       )
                     )
                   ) [[] []] coll
                 )
        ]
    (let [i (nth v (if (< (count (first v)) (count (second v))) 1 0))]
      (if (< (count i) 2) [] i)
      )
    )
  )

(defn r-demo [coll]
  (->> (map vector coll (range))
       (partition-by #(apply - %))
       (map #(map first %))
       (filter #(> (count %) 1))
       (sort-by (comp - count))
       first
       vec)
  )

(defn my-partition [n coll]
  (
   (fn dep-process [result n-coll]
     (if (< (count n-coll) n)
       result
       (dep-process (
                      conj
                      result
                      (take n n-coll)
                      ) (drop n n-coll))
       )
     )
   []
   coll
   )
  )


(defn my-frequencies [coll]
  (reduce (fn [p q]
            (assoc p q (+ 1 (if (number? (get p q)) (get p q) 0)))
            ) (hash-map) coll)
  )


(defn my-distinct [coll]
  (map #(first %)
       (reduce
         (fn [p q] (assoc p q 1))
         (hash-map) coll)
       )
  )

(defn my-comp [& fns] (
                        fn [& args]
                        (
                          reduce
                          (fn [pfn q] (q pfn))

                          (if (= (count args) 1)
                            (first args)
                            args
                            )
                          (reverse
                            (if
                              (> (count args) 1)
                              (update-in (into [] fns) [(- (count fns) 1)] (fn [v] (fn [_v] (apply v _v))))
                              fns
                              )
                            )
                          )
                        ))

(defn other-comp [& fns]
  (fn [& args]
    (first
      (reduce (fn [arg f]
                (list (apply f arg))
                ) args (reverse fns))
      )
    )
  )


(defn other-comp-no-reverse [& fns]
  (reduce (fn [p g]
            #(p (apply g %&))
            ) fns)
  )

(defn my-juxt [& fns]
  (fn [& args]
    (for [f fns] (apply f args))
    )
  )

(defn my-reduction
  [func & args]
  (let [result (func (first args))]
    )
  )

(defn my-lazy [] (lazy-seq (cons (str 1) (my-lazy))))

(defn my-zipmap [arr1 arr2]
  (
    into
    (array-map)
    (map vector arr1 arr2)
    )

  )


(defn my-iterate [func x]
  (lazy-seq (cons x (my-iterate func (func x))))

  )

(defn my-group-by [func coll]
  (reduce
    (fn [p c]
      (assoc p (func c) (concat (get p (func c)) [c]))
      )

    (hash-map) coll)

  )

(defn my-type [x] (
                    cond
                    (= (conj x {}) x) :map
                    (empty? x) (cond
                                 (= (clojure.set/union x #{}) #{}) :set
                                 (= (conj (conj x 0) 1) [0 1]) :vector
                                 :else :list
                                 )
                    (= (clojure.set/union x x) x) :set
                    (= (first (conj x x)) x) :list
                    :else :vector
                    )
  )

(defn my-gcd [a b]
  (if (zero? b) a
                (my-gcd b (mod a b))
                )
  )

(defn is-prime2 [x v max]
  (if (<= x max)
    (if
      (= (mod v x) 0)
      false
      (is-prime2 (inc x) v max)
      )
    true
    )
  )


(defn is-prime [x]
  (is-prime2 2 x (Math/sqrt x))
  )

(defn gene-prime-from [x]
  (if (is-prime x) x (gene-prime-from (inc x)))
  )

(defn my-gene-prime-numbs [size]
  (cond (zero? size) []
        (= 1 size) [2]
        (< 1 size)
        (reduce (fn [p c]
                  (conj p
                        ; 🔥 计算 prime
                        (gene-prime-from (+ (last p) 1))
                        )
                  ) [2] (range (- size 1)))
        )
  )

(defn my-gpn [size]
  (letfn [
          (is-p2 [x v max]
            (if (<= x max)
              (if
                (= (mod v x) 0)
                false
                (is-p2 (inc x) v max)
                )
              true
              )
            )
          (is-p [x] (is-p2 2 x (Math/sqrt x)))
          (gpf [x] (if (is-p x) x (gpf (inc x))))
          ]
    (cond (zero? size) []
          (= 1 size) [2]
          (< 1 size)
          (reduce (fn [p c]
                    (conj p
                          (gpf (+ (last p) 1))
                          )
                    ) [2] (range (- size 1)))
          )
    )

  )

(defn my-merge-with [f & args]
  (reduce
    ; c 为 {} 对象
    (fn [p c]
      (reduce
        (fn [ip ikey]
          (assoc
            ip
            ikey
            (if
              (get ip ikey)
              (f (get ip ikey) (get c ikey))
              (get c ikey)
              )
            )
          )
        p
        (keys c)
        )
      )
    (hash-map) args)

  )



(defn my-split-sort-str [s]
  (sort
    #(compare (clojure.string/lower-case %1)
              (clojure.string/lower-case %2)
              )
    (filter
      #(
         not
         (
           = % ""
             )
         )
      (str/split s #"[^a-zA-Z]")
      )
    )
  )


; TODO
(defn my-tic-tac-toe-analyze
  "board =
   [
    [:o :e :x]
    [:e :o :x]
    [:o :e :x]
   ]
   :e    -> empty
   :x,:o -> player
   find out who's winner
   if not one was win, then return nil
   if neither player has won, return nil
   "
  [board]

  )

(defn my-split-skip [s]
  (subs

    (reduce str ""
            (map #(str "," %)
                 (filter
                   (fn [x]
                     (let [r (Math/sqrt (Integer/parseInt x))]
                       (= (Math/ceil r) r)
                       )
                     )
                   (clojure.string/split s #",")
                   )
                 )
            )
    1
    )
  )

(defn calc-co-prime [n]
  (letfn [(

            gcd [a b]
            (if (zero? b) a
                          (gcd b (mod a b))
                          )
            )]
    (count
      (filter int?
              (for [x (range 1 n)]
                (if (= (gcd x n) 1) x
                                    nil
                                    )
                )
              )
      )
    )
  )

(defn group-by-same-words [s-v]
  ((comp
     ;#(apply clojure.set/union %)
     #(map (fn [v] (if (= 1 (count v)) nil (set v))) %) vals)
   (group-by
     #(
        sort
        (
          clojure.string/split % #""
                               )
        )
     s-v)
   )
  )

(defn anagram-finder [s-v]
  (set
    (map set

         (filter #(< 1 (count %))
                 (vals
                   (group-by (fn [v]
                               (sort (clojure.string/split v #""))
                               ) s-v)
                   )
                 )
         )
    )
  )


(defn my-trampoline [f & args]
  (loop [i (apply f args)]
    (if (not (fn? i))
      i
      (recur (i))
      )
    )
  )

(defn Triangle-Minimal-Path
  "
  从顶到底找到一条最短（值最少）的路径
  只能找上下相邻的

   '([1] a
    [2 4] b c
   [5 4 1] d e f
  [1 5 3 2]) g h i j
  : 1-> 4 -> 1 -> 4
  keywords: 贪心算法
  要穷举？
  Dijkstra 算法
  "
  [triangle-path]
  ;+ HIGHLIGHT 找到相邻关系
  ; 跟 index 相关
  ; 上层和下层的关系： top-index 低一层的 [top-index, top-index + 1]
  ;+
  (
    let [
         ds (hash-map)
         ; [[v inner-i] outer-i]
         ; map (fn ) (str outer-i "-" inner-i)
         ;
         weight (hash-map)
         ; weight = #{key1 v1 key v2}
         pathTo
         ;(apply conj
         ;         (flatten
         (map-indexed (fn [o-i v]
                        (map-indexed (fn [in-i in-v]
                                       (hash-map
                                         (str o-i "-" in-i)
                                         in-v
                                         )
                                       ) v)
                        )
                      triangle-path)
         ;)


         ;)

         ]
    ;(into hash-map (flatten pathTo))
    pathTo
    )
  ; TODO 合适的数据结构
  ; 回溯
  ; 记录路径
  ;
  ; 路径的数据结构
  ; pathTo = {a: [b c] b : [d e]  c: [ e :f] }
  ; weight = triangle-path[level_index - column_index]
  ; ["1-1"] = a
  ;   [level_index - column_index]
  ; 从 a 点开始算
  ; ds = {b: 3 , c : 5, [d,e,f,g,h,i,j]: Max}
  ;
  ; 如何结尾？ pathTo[key] = nil 的时候
  ; 入栈
  ;     0. pathTo[a] = [b c]
  ;     1. 算出 b c 两点在 ds 中最小的数 -> b
  ;     2. b 可以到 [d e]
  ;     3. 计算
  ;         ds[d] = ds[b] + weight[d]
  ;         ds[e] = ds[b] + weight[e]
  ;     4. pathTo[a] = [d e c]
  ;        回到 1
  ; (map-indexed )

  ; TODO 路径记录

  )


























