(ns extr.core
  (:require [clojure.string :as str])
  (:use [clojure.core.match :only (match)])
  (:import (java.io BufferedReader FileReader)))

(def cli-keywords
  #{"%command" "%endcommand" "%attribute" "%endattribute" "%action" "%endaction"})



(defn- parse-attr [ir keyword line]
  ir)

(defn- parse-action-body [ir line]
  ir)

(defn- in-quotes [line]
  (str/replace
   (re-find #"\"[a-zA-Z0-9\p{Space}\p{Punct}&&[^\"]]+\"" line)
   #"\"" ""))

(defn- conj-to-peek [ir map]
  (conj (pop ir) (conj (peek ir) map)))

(defn- parse-cli [ir line]
  (let [keyword (re-find #"%[a-zA-Z0-9]+" line)]
    (if (cli-keywords keyword)
      ;; (cond
      ;;  (zero? (compare keyword "%command")) (conj ir {:command (in-quotes line)})
      ;;  (zero? (compare keyword "%action")) (conj-to-peek {:action (in-quotes)}))
      (match [keyword]
             ["%command"] (conj ir {:command (in-quotes line)})
             ["%action"] (conj-to-peek ir {:action (in-quotes line)}))
      (parse-attr ir keyword line))))

(defn- build-ir [ir [first-char & xs :as line]]
  (cond
   (nil? first-char) ir
   (= \# first-char) ir
   (= \% first-char) (parse-cli ir line)
   :else (parse-action-body ir line)))

(defn- strip-line [line]
  (str/replace-first line #"\s+" ""))

(defn parse-file [file-name]
  (with-open [rdr (BufferedReader. (FileReader. file-name))]
    (reduce build-ir [] (map strip-line (line-seq rdr)))))
