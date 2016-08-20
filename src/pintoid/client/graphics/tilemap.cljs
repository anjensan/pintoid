(ns pintoid.client.graphics.tilemap
  (:require
   [cljsjs.pixi]
   [pintoid.client.graphics.layer :as gl]
   [pintoid.client.graphics.utils
    :refer [minmax -inf +inf point->vec vec->point]]
   [pintoid.client.graphics.layer :as gl])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [defjsclass call-super foreach!]]))


(defn- bounding-rectangle [points]
  (let [xs (map first points)
        ys (map second points)]
    [[(reduce min xs) (reduce min ys)]
     [(reduce max xs) (reduce max ys)]]))


(defn- advance-bounds [[[x1 y1] [x2 y2]] a]
  [[(js/Math.floor (- x1 a)) (js/Math.floor (- y1 a))]
   [(js/Math.ceil (+ x2 a)) (js/Math.ceil (+ y2 a))]])


(defn- recreate-tiles-impl [view-rect wt bounds advance tiles create-tile! drop-tile!]
  (let [wtapply #(->> % vec->point (.apply wt) point->vec)
        [zx zy] (wtapply [0 0])
        [cx cy] (wtapply [1 0])
        [rx ry] (wtapply [0 1])
        cdx (- cx zx), cdy (- cy zy)
        rdx (- rx zx), rdy (- ry zy)
        -a (- advance), +a (+ 1 advance)
        [[bx1 by1] [bx2 by2]] (bounding-rectangle
                               (map wtapply [[-a -a] [+a -a] [-a +a] [+a +a]]))
        [[vrx1 vry1] [vrx2 vry2]] view-rect
        vrps' (map #(->> % vec->point (.applyInverse wt) point->vec)
                   [[vrx1 vry1] [vrx2 vry1] [vrx2 vry2] [vrx1 vry2]])
        [[c1 r1] [c2 r2]] (advance-bounds (bounding-rectangle vrps') advance)
        [[c1' r1'] [c2' r2']] bounds
        maybe-crs (vec
                   (for [c (range (max c1 c1') (inc (min c2 c2')))
                         r (range (max r1 r1') (inc (min r2 r2')))]
                     [c r]))
        visible? (fn [[c r]]
                   (let [dx (+ zx (* c cdx) (* r rdx) (- zx))
                         dy (+ zy (* c cdy) (* r rdy) (- zy))]
                   (and
                    (<= (+ bx1 dx) vrx2)
                    (<= (+ by1 dy) vry2)
                    (>= (+ bx2 dx) vrx1)
                    (>= (+ by2 dy) vry1))))
        ]
    (as-> tiles $
      (reduce
       (fn [a [cr t]]
         (if (not (visible? cr))
           (do (when t (drop-tile! t))
               (dissoc a cr))
           a))
       $
       tiles)
      (reduce
       (fn [a cr]
         (if (or (contains? tiles cr) (not (visible? cr)))
           a
           (assoc a cr (create-tile! cr))))
       $
       maybe-crs))))


(defjsclass TilemapSpriteImpl js/PIXI.Container

  (constructor [this [twidth theight] bounds tile-advance view-advance create-tile]
    (call-super TilemapSpriteImpl this .constructor)
    (set! (.-tiles this) {})
    (set! (.-extravr this) (or view-advance 0))
    (set! (.-params this)
          {:twidth twidth
           :theight theight
           :bounds bounds
           :advance (or tile-advance 0)
           :create-tile create-tile}))

  ("updateTransform" [this]
   (call-super TilemapSpriteImpl this "updateTransform")
   (let [wt (aget this "worldTransform")
         tx' (aget wt "tx"), ty' (aget wt "ty")
         [tx ty] (.-oldtxy this)
         [[vrx1 vry1] [vrx2 vry2] :as vrect] (gl/get-sprite-view-rect this)
         mt (.-extravr this)
         mtx (* mt (- vrx2 vrx1))
         mty (* mt (- vry2 vry1))]
     (when (or
          (> (js/Math.abs (- tx tx')) mtx)
          (> (js/Math.abs (- ty ty')) mty))
       (set! (.-oldtxy this) [tx' ty'])
       (.recreateTiles
        this
        wt
        [[(- vrx1 mtx) (- vry1 mty)]
         [(+ vrx2 mtx) (+ vry2 mty)]]))))

  (recreateTiles [this wt vrect]
    (let [{:keys [bounds twidth theight advance create-tile]} (.-params this)
          tiles (.-tiles this)
          tx (aget wt "tx"), ty (aget wt "ty")
          wt (.. wt
                 (clone)
                 (translate (- tx) (- ty))
                 (scale twidth theight)
                 (translate tx ty))
          tiles' (recreate-tiles-impl
                  vrect wt bounds advance tiles
                  (fn [[c r :as cr]]
                    (let [s (create-tile cr)]
                      (set! (.. s -position -x) (* c twidth))
                      (set! (.. s -position -y) (* r theight))
                      s))
                  (fn [x]
                    (.destroy x)
                    (foreach! [c (.-children x)] (.destroy c))))
          ta (into [] (remove nil?) (vals tiles'))]
      (when (not= (keys tiles) (keys tiles'))
        (run! #(do (set! (.-parent %) this) (.updateTransform %)) ta)
        (set! (.-children this) (apply array ta))
        (set! (.-tiles this) tiles')))))


(defn make-tilemap-sprite-factory
  [create-tile
   {:keys [cache-as-bitmap
           view-advance
           tile-bounds
           tile-advance
           tile-size
           tile-group]
    :or {cache-as-bitmap false
         tile-bounds [[-inf -inf] [+inf +inf]]
         tile-advance 0.5
         view-advance 0.125
         tile-size [32 32]
         tile-group [1 1]}
    :as proto}]
  (let [[otw oth] tile-size
        [gw gh :as group-size] tile-group
        [[tbc1 tbr1] [tbc2 tbr2]] tile-bounds
        subtiles (vec (for [c (range gw), r (range gh)] [c r]))
        [[tb11 tb12] [tb21 tb22]] tile-bounds
        create-group (fn [[gc gr]]
                       (let [cont (js/PIXI.Container.)]
                         (foreach! [[c r] subtiles]
                           (let [cc (+ (* gc gw) c)
                                 rr (+ (* gr gh) r)]
                             (when (and (<= tbc1 cc tbc2) (<= tbr1 rr tbr2))
                               (let [s (create-tile [cc rr])]
                                 (.addChild cont s)
                                 (set! (.. s -position -x) (* c otw))
                                 (set! (.. s -position -y) (* r oth))))))
                         (set! (.-cacheAsBitmap cont) cache-as-bitmap)
                         cont))
        tsize [(* otw gw) (* oth gh)]
        tbounds [[(js/Math.floor (/ tb11 gw)), (js/Math.floor (/ tb12 gh))]
         [(js/Math.ceil (/ tb21 gw)), (js/Math.ceil (/ tb22 gh))]]
        tadvance (/ tile-advance (min gw gh))]
    (fn [_]
      (TilemapSpriteImpl.
       tsize tbounds tadvance view-advance create-group))))
