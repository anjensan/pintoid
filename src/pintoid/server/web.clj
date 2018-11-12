(ns pintoid.server.web
  (:require
   [clojure.string :as s]
   [pintoid.server.cswiring :as csw]
   [taoensso.timbre :as timbre]
   [ring.util.response :refer [response redirect]]
   [ring.middleware params session cookies]
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
                  (hf/text-field
                   {:pattern "^[a-zA-Z0-9_-]{1,16}$"
                    :title   "no more than 16 alphanumeric characters"}
                   "name"
                   (get-in req [:cookies "nick" :value] "anonymous"))
                  (hf/submit-button "PLAY")
                  )]])))

(defn page-game [req]
  (timbre/tracef "Page game req: %s" req)
  (if-not (contains? @csw/avatars (get-in req [:session :pid]))
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

(defn- limit-str [s n]
  (if (> (count s) n) (subs s 0 n) s))

(defn- coerce-nick [n]
  (-> n
      (s/replace #"[^a-zA-Z0-9_-]" "*")
      (limit-str 16)))

(defn join-game [req]
  (timbre/tracef "Join game req: %s" req)
  (let [pid (or (get-in req [:session :pid])
                (csw/generate-player-pid))
        nick (coerce-nick (get-in req [:params "name"] "anonymous"))
        ip (get req :remote-addr)]
    (csw/create-player-avatar pid {:ip ip :nick nick})
    (-> (redirect "/game" :see-other)
        (assoc-in [:session :pid] pid)
        (assoc-in [:cookies "nick"] {:value nick :max-age 2592000})
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
  (resources "/snd" {:root "snd"})
  (resources "/css" {:root "css"}))

(defn ring-handler []
  (-> #'pintoid.server.web/app-routes
      (ring.middleware.params/wrap-params)
      (ring.middleware.cookies/wrap-cookies)
      (ring.middleware.session/wrap-session)))
