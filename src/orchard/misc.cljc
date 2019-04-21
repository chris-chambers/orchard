(ns orchard.misc
  (:require
   [clojure.string :as str]))

(def ^:const windows-prefix
  "Windows")

(defn os-windows? []
  #?(:clj (.startsWith (System/getProperty "os.name") windows-prefix)
     :cljr (= (.Platform Environment/OSVersion) PlatformID/Win32NT)))

#?(:clj
   (defn boot-fake-classpath
     "Retrieve Boot's fake classpath.
  When using Boot, fake.class.path contains the original directories with source
  files, which makes it way more useful than the real classpath.
  See https://github.com/boot-clj/boot/issues/249 for details."
     []
     (System/getProperty "fake.class.path")))

(defn boot-project?
  "Check whether we're dealing with a Boot project.
  We figure this by checking for the presence of Boot's fake classpath."
  []
  #?(:clj  (not (nil? (boot-fake-classpath)))
     :cljr false))

(defn as-sym
  [x]
  (cond
    (symbol? x) x
    (string? x) (if-let [[_ ns sym] (re-matches #"(.+)/(.+)" x)]
                  (symbol ns sym)
                  (symbol x))))

(defn update-vals
  "Update the values of map `m` via the function `f`."
  [f m]
  (reduce (fn [acc [k v]]
            (assoc acc k (f v)))
          {} m))

(defn update-keys
  "Update the keys of map `m` via the function `f`."
  [f m]
  (reduce (fn [acc [k v]]
            (assoc acc (f k) v))
          {} m))

(defn deep-merge
  "Merge maps recursively. When vals are not maps, last value wins."
  [& xs]
  (let [f (fn f [& xs]
            (if (every? map? xs)
              (apply merge-with f xs)
              (last xs)))]
    (apply f (filter identity xs))))

(def java-api-version
  #?(:clj
     (try
       (let [java-ver (System/getProperty "java.version")
             [major minor _] (str/split java-ver #"\.")
             major (Integer/parseInt major)
             minor (Integer/parseInt minor)]
         (if (> major 1)
           major
           (or minor 7)))
       (catch Exception _ 7))
     ;; FIXME: What should we do here?  CLR doesn't have a Java API at all!
     :cljr 7))

(defmulti transform-value "Transform a value for output" type)

(defmethod transform-value :default [v] (str v))

;; FIXME: Need to handle each number type in the CLR?
#?(:clj (defmethod transform-value Number [v] v))

(defmethod transform-value nil [v] nil)

;; FIXME: Will it be FileInfo in the CLR?
#?(:clj
   (defmethod transform-value java.io.File
     [v]
     (.getAbsolutePath ^java.io.File v)))

(defmethod transform-value clojure.lang.Sequential
  [v]
  (list* (map transform-value v)))

(defmethod transform-value clojure.lang.Symbol
  [v]
  (let [[the-ns the-name] [(namespace v) (name v)]]
    (if the-ns
      (str the-ns "/" the-name)
      the-name)))

(defmethod transform-value clojure.lang.Keyword
  [v]
  (transform-value (.sym ^clojure.lang.Keyword v)))

(defmethod transform-value clojure.lang.Associative
  [m]
  (->> (for [[k v] m] ; bencode keys must be strings
         [(str (transform-value k)) (transform-value v)])
       (into {})))

;; handles vectors
(prefer-method transform-value clojure.lang.Sequential clojure.lang.Associative)
