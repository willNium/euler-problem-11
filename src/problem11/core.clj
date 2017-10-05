(ns problem11.core)
(require '[clojure.string :as str])

(defn trim-zero [string]
  (if (str/last-index-of string "0" 0) (subs string 1) string))

(defn reverse-vec-from-string [string]
  (into [] (reverse (as-> string s
                 (str/split s #" ")
                 (map trim-zero s)
                 (map read-string s)))))

(defn vec-from-string [string]
  (into [] (as-> string s
                 (str/split s #" ")
                 (map trim-zero s)
                 (map read-string s))))

(defn get-subsets [size coll]
  (mapcat #(partition size 1 %) coll))

(defn get-product [coll]
  (map #(reduce * %) coll))

(defn small-diag [coll column]
  (loop [list coll
         y column
         row 0
         ans []]
    (if (not (= nil (get list (+ 1 y))))
      (recur list (+ 1 y) (+ 1 row) (conj ans (get (get list row) y)))
      (conj ans (get (get list row) y)))))

(defn get-diag [list]
  (loop [y 0 diags []]
    (if (not (= nil (get (get (into [] list) 0) y)))
      (recur (+ 1 y) (conj diags (small-diag (into [] list) y)))
      diags)))

(defn horiz [list size]
  (->> list
       (map vec-from-string)
       (into [])
       (get-subsets size)
       get-product
       (apply max)))

(defn horiz-diag [list size]
  (->> list
       (map vec-from-string)
       get-diag
       (get-subsets size)
       get-product
       (apply max)))

(defn vert-diag [list size]
  (->> list
       (map vec-from-string)
       get-diag
       (get-subsets size)
       get-product
       (apply max)))

(defn reverse-horiz-diag [list size]
  (->> list
       (map reverse-vec-from-string)
       get-diag
       (get-subsets size)
       get-product
       (apply max)))

(defn reverse-vert-diag [list size]
  (->> list
       (map reverse-vec-from-string)
       get-diag
       (get-subsets size)
       get-product
       (apply max)))

(defn vert [list size]
  (->> list
       (map vec-from-string)
       (apply map vector)
       (into [])
       (get-subsets size)
       get-product
       (apply max)))

