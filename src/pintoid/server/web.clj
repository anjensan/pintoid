(ns pintoid.server.web
  (:use [pintoid.server cswiring utils])
  (:require
   [ring.util.response :refer [response redirect]]
   [compojure.core :refer [defroutes GET POST]]
   [compojure.route :refer [resources]]
   [chord.http-kit :refer [wrap-websocket-handler]]
   [hiccup.page :refer [html5 include-js include-css]]
   [clojure.tools.logging :as log]
   ))


(defn page-index []
  (html5
   [:head
    [:title "PINTOID"]
    (include-css "/css/main.css")]
   [:body
    [:div.flatcontainer
     [:header
      [:h1 "PINTOID" [:span "enjoy"]]]
     [:svg {:id "bt" :xmlns "http://www.w3.org/2000/svg"
            :version "1.1" :width "100%" :height "100"
            :viewBox "0 0 100 102" :preserveAspectRatio "none"}
      [:path {:d "M0 0 L50 100 L100 0 Z"}]]
     [:section.join
       [:a.join-button {:href "/game"} "Join the game"]]]]))


(defn page-game []
  (html5
   [:head
    [:title "Pintoid"]
    (include-js "/js/pintoid.js")
    (include-css "/css/main.css")]
   [:body {:style "background: lightgray;"}
    [:div.content]]))


(defn game-ws-handler [req]
  (log/info "connection from" (:remote-addr req))
  (if (:ws-channel req)
    (add-new-client-connection req)
    {:status 404 :body "WebSocket only!"}))


(defroutes app-routes
  (GET "/" [] (response (page-index)))
  (GET "/game" [] (response (page-game)))
  (GET "/ws" [] (wrap-websocket-handler game-ws-handler {:format :transit-json}))
  (resources "/js" {:root "js"})
  (resources "/img" {:root "img"})
  (resources "/css" {:root "css"}))
