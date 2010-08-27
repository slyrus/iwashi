
(ns iwashi.scratch
  (:use [iwashi.array]
        :reload))

(let [q (make-2-bit-array (bigint 128))]
  (doall (map #(set-item q % (mod % 4))
              (take 32 (iterate inc 0))))
  (map #(item q %)
       (take 32 (iterate inc 0))))

(let [q (make-2-bit-array (bigint 4e9))]
  (doall (map #(set-item q (+ (bigint 3e9) %) (mod % 4))
              (take 32 (iterate inc 0))))
  (map #(item q (+ (bigint 3e9) %))
       (take 32 (iterate inc 0))))

(let [q (make-4-bit-array (bigint 1e9))]
  (doall (map #(set-item q (+ (int 1e7) %) (mod % 16))
              (take 32 (iterate inc 0))))
  (map #(item q (+ (int 1e8) %))
       (take 32 (iterate inc 0))))

(let [q (make-5-bit-array (bigint 1e9))]
  (doall (map #(set-item q % (mod % 32))
              (take 64 (iterate inc 0))))
  (map #(item q %)
       (take 64 (iterate inc 0))))

