(ns straw
  (:use-macros [clojure.core.match.js :only [match]])
  (:require    [goog.dom :as dom]
                [cljs.nodejs :as node]))

;; ## This application does not compile under :advanced

;; ## All of these libraries are available via npm
(def commander (node/require "commander"))
(def http (node/require "http"))
(def jsdom (node/require "jsdom"))
(def url (node/require "url"))
(def fs (node/require "fs"))

;; Borrowed from http://mmcgrana.github.com/2011/09/clojurescript-nodejs.html
(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.strobj (reduce (fn [m [k v]]
               (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))

;; Borrowed from http://mmcgrana.github.com/2011/09/clojurescript-nodejs.html
(defn url-parse
  "Returns a map with parsed data for the given URL."
  [u]
  (let [raw (js->clj (.parse url u))]
    {:protocol (.substr (get raw "protocol")
                        0 (dec (.length (get raw "protocol"))))
     :host (get raw "hostname")
     :port (js/parseInt (get raw "port"))
     :method "GET"      ;; this is ghetto
     :path (get raw "pathname")}))
     ;:path-parts (.split (get raw "pathname") "/")
;
(def page-data (atom []))

(defn list-handler [val]
  (. val (split ",")))

(doto commander
  (.version "0.0.1")
  (.usage "[options] <url>")
  (.option "-o, --outfile [type]" "the file to download to")
  ;(.option "-r, --showresources <items>" "a comma separated list of tags to parse for" list-handler)
  ;(.option "-nc, --noclobber" "")
  (.option "-r, --recursive" "Turn on recursive retrieving")
  (.option "-l, --level [depth]" "Specify recursive maximum depth level, default is 5")
  (.parse process.argv))

(defn download-file
  [url]
    (let [params (url-parse url)
          req (. http (request (clj->js params)
             (fn [res]
               (doto res
                 (.setEncoding "utf8")
                 (.on "data" (fn [chunk]
                                 (swap! page-data assoc (count @page-data) chunk)))
                 (.on "end" (fn [] 
                              (let [parts (js->clj (.split (:path params) "/"))
                                    headers (into {} (for [l (js->clj (.headers res))] 
                                                          [(keyword (l 0)) (l 1)])) ]
                                (if (or (not    (.outfile commander))
                                        (not (= (.outfile commander) "-")))
                                  (let [file-name (if (.outfile commander)
                                                      (.outfile commander)
                                                      (first (reverse parts)))]
                                    (. fs (writeFileSync file-name (apply str @page-data) )))
                                  ;; intended that passing -o - would print to stdout, but commander can't handle this
                                  (prn (apply str @page-data)) )
                                ;; need an AND here to catch if it's not HTML content
                                (if (.recursive commander) 
                                         ;(. (js/RegExp "text/html") (test (:content-type header)) )) 
                                    (let [window (.. jsdom (jsdom (apply str @page-data)) (createWindow))
                                          tag-list ["a" "img"]]
                                      (doseq [tag tag-list]
                                        (let [elems (.. window document (getElementsByTagName tag))]
                                          (prn (str "--- Showing: " tag " ---"))
                                          (doseq [i (range (.length elems))]
                                            (let [elem (. elems (item i))
                                                  new-url (cond (= "a" tag) (. elem (getAttribute "href"))
                                                                       (= "img" tag) (. elem (getAttribute "src"))
                                                                       :else "") 
                                                  new-url-params (cond (. new-url (match #"^/")) {:path new-url
                                                                                                  :host (:host params)
                                                                                                  :port (:port params)
                                                                                                  :protocol (:protocol params)}
                                                                       (. new-url (match #"^https?://")) (url-parse new-url)
                                                                       :else {})
                                                  new-path-parts (if (:path new-url-params) (.split (:path new-url-params) "/") []) 
                                                  new-full-url (apply str [(:protocol new-url-params) 
                                                                           "://"
                                                                            (:host new-url-params)
                                                                            (:path new-url-params)]) ]
                                              ;; NOTE: this currently throws errors if the innerHTML is not plain textnode
                                              (prn (str new-full-url))
                                              (if (= tag "img")
                                                  (download-file new-full-url))
                                            )) )) )))
                              ))) )))]
        (doto req
          (.write "data\n")
          (.write "data\n")
          (.end) ) ) )

(defn start [& _]
  (let [url-list (js->clj (.args commander))]
    (if (url-list 0)
      (download-file (url-list 0) ))))

(set! *main-cli-fn* start)
