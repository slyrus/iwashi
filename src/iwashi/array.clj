
(ns iwashi.array
  (:require [clojure.contrib.math :as math]))

(defprotocol BigArray
  ;; can't use count becuase it returns an int
  (length [obj])
  (item [obj pos])
  (set-item [obj pos val]))

(defrecord Int2Array [_length _store]
  BigArray
  (length [obj] _length)

  (item [obj pos]
        (let [idx (bit-shift-right pos 3)
              bit-start (bit-shift-left (mod pos 8) 1)]
          (bit-and (bit-shift-right
                    (int (aget _store idx))
                    bit-start)
                   0x3)))
  (set-item [obj pos val]
            (let [idx (bit-shift-right pos 3)
                  bit-start (bit-shift-left (mod pos 8) 1)]
              (aset-char
               _store
               idx
               (char (bit-or
                      (bit-and (int (aget _store idx))
                               (bit-clear (bit-clear 0xffff bit-start)
                                          (inc bit-start)))
                      (bit-shift-left (bit-and val 0x3) bit-start)))))))

(defn make-2-bit-array [length]
  (Int2Array. length (char-array (bit-shift-right (+ 7 length) 3))))

(defrecord Int4Array [_length _store]
  BigArray
  (length [obj] _length)

  (item [obj pos]
        (let [idx (bit-shift-right pos 2)
              bit-start (bit-shift-left (mod pos 4) 2)]
          (bit-and (bit-shift-right
                    (int (aget _store idx))
                    bit-start)
                   0xf)))
  (set-item [obj pos val]
            (let [idx (bit-shift-right pos 2)
                  bit-start (bit-shift-left (mod pos 4) 2)]
              (aset-char
               _store
               idx
               (char (bit-or
                      (bit-and (int (aget _store idx))
                               (cond (= bit-start 0)
                                     0xfff0
                                     (= bit-start 4)
                                     0xff0f
                                     (= bit-start 8)
                                     0xf0ff
                                     (= bit-start 12)
                                     0x0fff))
                      (bit-shift-left (bit-and val 0xf) bit-start)))))))

(defn make-4-bit-array [length]
  (Int4Array. length (char-array (bit-shift-right (+ 3 length) 2))))

(defrecord Int5Array [_length _store]
  BigArray
  (length [obj] _length)

  (item [obj pos]
        (let [idx (math/floor (/ pos 3))
              bit-start (* (mod pos 3) 5)]
          (bit-and (bit-shift-right
                    (int (aget _store idx))
                    bit-start)
                   0x1f)))
  (set-item [obj pos val]
            (let [idx (math/floor (/ pos 3))
                  bit-start (* (mod pos 3) 5)]
              (aset-char
               _store
               idx
               (char (bit-or
                      (bit-and (int (aget _store idx))
                               (cond (= bit-start 0)  2r1111111111100000
                                     (= bit-start 5)  2r1111110000011111
                                     (= bit-start 10) 2r1000001111111111))
                      (bit-shift-left (bit-and val 0x1f) bit-start)))))))

(defn make-5-bit-array [length]
  (Int5Array. length (char-array (math/floor (/ (+ 2 length) 3)))))

                                 
