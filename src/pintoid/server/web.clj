(ns pintoid.server.web
  (:use [pintoid.server cswiring utils ecs])
  (:require
   [pintoid.server.cswiring :as csw]
   [taoensso.timbre :as timbre]
   [ring.util.response :refer [response redirect]]
   [compojure.core :refer [defroutes GET POST]]
   [compojure.route :refer [resources]]
   [chord.http-kit :refer [wrap-websocket-handler]]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.form :as hf]))

(defn page-index [req]
  (response
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
      (hf/form-to {:id "joinform"} [:post "join-game"]
                  (hf/text-field {:autocomplete false} "name")
                  (hf/submit-button "Join")
                  )]])))

(defn page-game [req]
  (timbre/tracef "Page game req: %s" req)
  (if-not (get-in req [:session :pid])
    (redirect "/" :temporary-redirect)
    (response
     (html5
      [:head
       [:title "Pintoid"]
       (include-js "/js/pintoid.js")
       (include-css "/css/main.css")
       [:style "* {padding: 0; margin: 0}"]
       ]
      [:body {:style "background: dimgray; overflow: hidden"}
       [:div.content]]))))

(defn join-game [req]
  (timbre/tracef "Join game req: %s" req)
  (let [pid (or (get-in req [:session :pid])
                (csw/generate-player-pid))]
    (csw/create-player-avatar pid req)
    (-> (redirect "/game" :see-other)
        (assoc-in [:session :pid] pid)
        )))

(defn game-ws-handler [req]
  (timbre/debugf "WS connection from %s" (:remote-addr req))
  (let [pid (get-in req [:session :pid])
        wsc (:ws-channel req)]
    (cond
      (nil? pid) {:status 401 :body "Session expired?"}
      (nil? wsc) {:status 403 :body "WebSocket only!"}
      :else (csw/attach-ws-connection pid wsc))))

(defroutes app-routes
  (GET "/" [] page-index)
  (GET "/game" [] page-game)
  (POST "/join-game" [] join-game)
  (GET  "/game-sock" [] (wrap-websocket-handler game-ws-handler {:format :transit-json}))
  (resources "/js" {:root "js" :mime-types {"js" "text/javascript; charset=utf8"}})
  (resources "/img" {:root "img"})
  (resources "/css" {:root "css"}))
