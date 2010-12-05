(ns htmlgen.core
  (:require [clojure.string :as str]))

(def *balanced-elements* #{
                           :a
                           :html
                           :body
                           :head
                           :p
                           :b
                           :i
                           :strong
                           :form
                           :title
                           :script
                           :em
                           :ul
                           :li
                           :textarea
                           :table
                           :tr
                           :th
                           :td
                           :div
                           :span
                           :meta
                           :style
                           :font
                           :link

                           })
(def *unbalanced-elements* #{
                             :hr
                             :br
                             :input
                             :img
                             })
(def *balanced-element-tags* (into {} (map (fn [x]
                                             (let [tag-str (.substring (str x) 1)
                                                   open-tag (str "<" tag-str)
                                                   close-tag (str "</" tag-str ">")]
                                               [x [open-tag close-tag]])) *balanced-elements*)))

(def *unbalanced-element-tags* (into {} (map (fn [x]                                               
                                               [x (str "<" (.substring (str x) 1))]) *unbalanced-elements*)))

(defn is-balanced-tag? [x] (*balanced-elements* x))
(defn is-unbalanced-tag? [x] (*unbalanced-elements* x))

(defn str-apply [x]
  (if (or (seq? x) (vector? x))
    (apply str x)
    (str x)))

(defn any? [pred coll]
  (if (not (empty? coll))
    (loop [f (first coll)
           r (rest coll)]
      (if (empty? r)
        (pred f)
        (if (pred f)
          true
          (recur (first r) (rest r)))))
    false))

(defn primitive? [x]
  (any? #(%1 x) [float? keyword? integer? string? char? decimal? false? nil? true?]))

(defn attr-encode [x] x)

(defn gen-attr-lhs [x]
  (str (str/replace-first (str x) ":" "") "=\""))

(defn gen-attr [k v]
  (if v
    (let [v-string (attr-encode (if (string? v)
                                  v
                                  (str v)))]
      (str (gen-attr-lhs k) v-string "\""))
    nil))

(defn flatten-attrs [iattrs]
  (loop [result {}
         c (first iattrs)
         t (rest iattrs)]
    (if (not (nil? c))
      (if (keyword? c)
        (let [tf (first t)
              tr (rest t)]
          (recur (assoc result c tf) (first tr) (rest tr)))
        (recur (into result c) (first t) (rest t)))
      (if (and (not (empty? t)) (not (nil? t)))
        (recur result (first t) (rest t))
        result))))

(defn gen-attrs [iattrs]
  (let [fattrs (flatten-attrs iattrs)]
   (str/join " " (filter #(not (nil? %1)) (for [[k v] fattrs] (gen-attr k v))))))

(defn gen-open-tag-fragment [open & iattrs]
  (if iattrs
    (str open " " (gen-attrs iattrs))
    open))

(defn gen-open-tag [open & iattrs]
  (str (apply gen-open-tag-fragment (cons open iattrs)) ">"))

(defn partition-attributes [attrs]
  (loop [cattrs {}
         oattrs []
         c (first attrs)
         t (rest attrs)]
    (if (not (nil? c))
      (if (keyword? c)
        (let [tf (first t)
              tr (rest t)]
          (if (primitive? tf)
            (recur (assoc cattrs c tf) oattrs (first tr) (rest tr))
            (recur cattrs (into oattrs [tf c]) (first tr) (rest tr))))
        (recur cattrs (conj oattrs c) (first t) (rest t)))
      (if (and (not (empty? t)) (not (nil? t)))
        (recur cattrs oattrs (first t) (rest t))
        [(apply concat cattrs) oattrs]))))

(declare process-tags)

(defn simplify [elts]
  (loop [result []
         strings []
         h (first elts)
         t (rest elts)]
    (cond
     (string? h) (recur result (conj strings h) (first t) (rest t))
     (nil? h) (if (not (empty? t))
                (recur result strings (first t) (rest t))
                (if (not (empty? strings))
                  (conj result (apply str strings))
                  result))
     :else (if (not (empty? strings))
             (recur (conj result (apply str strings) h) [] (first t) (rest t))
             (recur (conj result h) strings (first t) (rest t))))))

(defn process-tag
  ([tag] (process-tag tag nil))
  ([tag body]
     (if (keyword? tag)
       (or (when-let [[prefix close] (*balanced-element-tags* tag)]
             (let [open (gen-open-tag prefix)]
               (into [open] (conj (process-tags body) close))))
           (when-let [unbalanced-open (*unbalanced-element-tags* tag)]
             (let [[constants variables] (partition-attributes body)
                   prefix (apply gen-open-tag-fragment (cons unbalanced-open constants))]
               (if variables
                 [prefix " " `(gen-attrs ~variables) ">"]
                 [(str prefix ">")]))))
       (let [itag (first tag)
             attrs (rest tag)
             [open close] (*balanced-element-tags* itag)
             [constants variables] (partition-attributes attrs)
             prefix (apply gen-open-tag-fragment (cons open constants))]
         (into (if variables
                   [prefix " " `(gen-attrs ~variables) ">"]
                   [(str prefix ">")]) (conj (process-tags body) close))))))

(defn is-nested-tag? [term]
  (if (or (list? term) (vector? term))
    (let [candidate (first term)]
      (if (or (list? candidate) (vector? candidate))
        (keyword? (first candidate))
        (keyword? candidate)))
    false))
    
(defn is-tag? [term]
  (or (keyword? term) (is-nested-tag? term)))

(defn process-tag-term [term]
  (if (is-tag? term)
    (if (keyword? term)
      (simplify (process-tag term))
      (let [tag (first term)
            body (rest term)]
        (simplify (process-tag tag body))))
    (if (primitive? term)
      (str term)
      `(str-apply ~term))))

(defn process-tags [terms]
  (simplify (loop [result []
                   h (first terms)
                   t (rest terms)]
              (if h
                (if (is-tag? h)
                  (recur (into result (process-tag-term h)) (first t) (rest t))
                  (recur (conj result (if (primitive? h)
                                        (str h)
                                        `(str-apply ~h))) (first t) (rest t)))
                (if (not (empty? t))
                  (recur result (first t) (rest t))
                  result)))))

(defmacro html [& body]
  (let [result (process-tags body)]
    (if (not (empty? result))
      (if (= (count result) 1)
        (first result)
        `(apply str ~result))
      "")))