(ns pintoid.server.handler
  (:use [pintoid.server.cs-comm])
  (:require
   [ring.util.response :refer [response redirect]]
   [compojure.core :refer [defroutes GET POST]]
   [compojure.route :refer [resources]]
   [chord.http-kit :refer [wrap-websocket-handler]]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.form :refer [form-to email-field submit-button]]
   ))


(defn game-page []
  (html5
   [:head
    [:title "Pintoid"]
    (include-js "/js/pixi.js")
    (include-js "/js/pintoid.js")
    (include-css "/css/main.css")]
   [:body {:style "background: lightgray;"}
    [:div.content]
    ]))

(defn index-page []
  (html5
   [:head
    [:title "PINTOID"]
    (include-css "/css/bootstrap.min.css")
    (include-css "/css/main.css")]
   [:body
    [:div {:class "flatcontainer"}
      [:header
        [:h1 "PINTOID" [:span "enjoy"]]]
      [:svg {:id "bt" :xmlns "http://www.w3.org/2000/svg"
             :version "1.1" :width "100%" :height "100"
             :viewBox "0 0 100 102" :preserveAspectRatio "none"}
       [:path {:d "M0 0 L50 100 L100 0 Z"}]]
      [:section {:class "join"}
        [:div {:class "row"}
          [:div {:class "col-md-4"}]
            ;; [:img {:src "/img/map.svg" :width "600px" :height "600px"}]]
          [:div {:class "col-md-4"}
           [:a {:class "btn btn-default btn-lg join-button" :href "/game"} "Join the game"]]
          [:div {:class "col-md-4"}]
        ]]
     ]]))


(defn ws-handler [req]
  (println "connection from" (:remote-addr req))
  (if (:ws-channel req)
    (add-new-client-connection req)
    (do
      (println "UUPS no WS" req)
      {:status 404 :body "uups, no WS"})))


(defn wrap-ws-check [h]
  (fn [req]
    (when-not (:websocket? req)
      (println "NO WS CHAN" req))
    (h req)))


(defroutes app-routes
  (GET "/" [] (response (index-page)))
  (GET "/ws" [] (-> ws-handler
                    (wrap-websocket-handler {:format :json-kw})
                    (wrap-ws-check)
                    ))
  (GET "/game" {p :params} (response (game-page)))
  (resources "/js" {:root "js"})
  (resources "/img" {:root "img"})
  (resources "/css" {:root "css"}))
