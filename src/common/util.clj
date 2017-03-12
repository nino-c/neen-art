(ns common.util)

(def ^:dynamic *debug* true)

(defmacro dbg [& body]
  `(let [x# ~@body]
    (clojure.pprint/pprint (str "dbg: " (quote ~@body) "=" (x#)) x#)))

(defn dbg*
  "Output to *out* if *debug* is on"
  [& args]
  (if *debug*
      (apply #(.println System/out %) (list args)))
  (if (= (count args) 1)
      (first args)
      args))

(defn nth*
  "Variation on `nth1 function, adds modular handling of index"
  [col idx]
  (if (>= idx (count col))
      (nth col (mod idx (count col)))
      (nth col idx)))