(ns pintoid.server.handler
   (:require
    [ring.util.response :refer [response redirect]]
    [compojure.core :refer [defroutes GET POST]]
    [compojure.route :refer [resources]]
    [chord.http-kit :refer [wrap-websocket-handler]]
    [hiccup.page :refer [html5 include-js include-css]]
    [hiccup.form :refer [form-to email-field submit-button]]
    ))


(defn render-game-page []
  (html5
   [:head
    [:title "Pintoid"]
    (include-js "/js/pixi.js")
    (include-js "/js/pintoid.js")
    (include-css "/css/main.css")]
   [:body {:style "background: lightgray;"}
    [:div.content]
    ]))


(defn render-index-page []
  (redirect "/game"))


(defn handle-gogame-form [{email "email" :as xxx}]
  ; put 'email' to session & redirect to game
  (assoc (redirect "/game") :session {:email email}))


(defn ws-handler [req]
  (println "New WS connection" req)
  ;; TODO: spawn player handler
  )


(defroutes app-routes
  (GET "/" [] (response (render-index-page)))
  (GET "/ws" [] (-> ws-handler (wrap-websocket-handler {:format :json})))
  (GET "/game" {p :params} (response (render-game-page)))
  (resources "/js" {:root "js"})
  (resources "/img" {:root "img"})
  (resources "/css" {:root "css"}))
