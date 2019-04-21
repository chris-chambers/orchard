(ns orchard.info
  (:require
   [clojure.edn :as edn]
   #?(:clj  [clojure.java.io :as io]
      :cljr [clojure.clr.io :as io])
   #?(:clj [clojure.java.javadoc :as javadoc])
   [clojure.string :as str]
   #?(:clj [orchard.classloader :refer [class-loader]])
   #?(:clj [orchard.java :as java])
   [orchard.meta :as m]
   [orchard.misc :as u]))

#?(:clj (def resource io/resource)
   :cljr
   (defn resource
     [name]
     (->> (clojure.lang.RT/GetFindFilePaths)
          (map #(FileInfo. (Path/Combine % name)))
          (filter #(.Exists %))
          (map io/as-uri)
          first)))

(def see-also-data
  (edn/read-string (slurp (resource "see-also.edn"))))

(defn see-also
  [ns sym]
  (let [var-key (str ns "/" sym)]
    (->> (get see-also-data var-key)
         (filter (comp resolve u/as-sym)))))

(defn info
  [ns sym]
  (or
   ;; it's a special (special-symbol?)
   (m/special-sym-meta sym)
   ;; it's a var
   (m/var-meta (m/resolve-var ns sym))
   ;; sym is an alias for another ns
   (m/ns-meta (get (m/resolve-aliases ns) sym))
   ;; it's simply a full ns
   (m/ns-meta (find-ns sym))
   ;; it's a Java class/member symbol...or nil
   #?(:clj  (java/resolve-symbol ns sym)
      ;; FIXME: Equivalent of `orchard.java` for the CLR (maybe `clojure.reflect`?)
      :cljr nil)))

#?(:clj
   (defn info-java
     [class member]
     (java/member-info class member)))

(defn- resource-full-path [relative-path]
  #?(:clj  (io/resource relative-path (class-loader))
     :cljr (resource relative-path)))

(defn resource-path
  "If it's a resource, return a tuple of the relative path and the full resource path."
  [x]
  (or (if-let [full (resource-full-path x)]
        [x full])
      (if-let [[_ relative] (re-find #".*jar!/(.*)" x)]
        (if-let [full (resource-full-path relative)]
          [relative full]))
      ;; handles load-file on jar resources from a cider buffer
      (if-let [[_ relative] (re-find #".*jar:(.*)" x)]
        (if-let [full (resource-full-path relative)]
          [relative full]))))

(defn file-path
  "For a file path, return a URL to the file if it exists and does not
  represent a form evaluated at the REPL."
  [x]
  (when (seq x)
    #?(:clj
       (let [f (io/file x)]
         (when (and (.exists f)
                    (not (-> f .getName (str/starts-with? "form-init"))))
           (io/as-url f)))

       :cljr
       (let [f (FileInfo. x)]
         (when (and (.Exists f)
                    (not (-> f .Name (str/starts-with? "form-init"))))
           (io/as-uri f))))))

(defn file-info
  [path]
  (let [[resource-relative resource-full] (resource-path path)]
    (merge {:file (or (file-path path) resource-full path)}
           ;; Classpath-relative path if possible
           (if resource-relative
             {:resource resource-relative}))))
#?(:clj
   (defn javadoc-info
     "Resolve a relative javadoc path to a URL and return as a map. Prefer javadoc
  resources on the classpath; then use online javadoc content for core API
  classes. If no source is available, return the relative path as is."
     [^String path]
     {:javadoc
      (or (resource-full-path path)
          ;; [bug#308] `*remote-javadocs*` is outdated WRT Java
          ;; 8, so we try our own thing first.
          (when (re-find #"^(java|javax|org.omg|org.w3c.dom|org.xml.sax)/" path)
            (format "http://docs.oracle.com/javase/%d/docs/api/%s"
                    u/java-api-version path))
          ;; If that didn't work, _then_ we fallback on `*remote-javadocs*`.
          (some (let [classname (.replaceAll path "/" ".")]
                  (fn [[prefix url]]
                    (when (str/starts-with? classname prefix)
                      (str url path))))
                @javadoc/*remote-javadocs*)
          path)})

   :cljr
   (defn javadoc-info [] nil))

#?(:clj
   (do
     ;; TODO: Seems those were hardcoded here accidentally - we should
     ;; probably provide a simple API to register remote JavaDocs.
     (javadoc/add-remote-javadoc "com.amazonaws." "http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/")
     (javadoc/add-remote-javadoc "org.apache.kafka." "https://kafka.apache.org/090/javadoc/index.html?")))
