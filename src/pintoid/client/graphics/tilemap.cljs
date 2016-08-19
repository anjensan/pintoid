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
  (let [xs (mapv first points)
        ys (mapv second points)]
    [[(reduce min xs) (reduce min ys)]
     [(reduce max xs) (reduce max ys)]]))


(defn- advance-bounds [[[x1 y1] [x2 y2]] a]
  [[(js/Math.floor (- x1 a)) (js/Math.floor (- y1 a))]
   [(js/Math.ceil (+ x2 a)) (js/Math.ceil (+ y2 a))]])


(defn- all-rect-inner-ceil-points [view-rect wt tile-bounds tile-advance]
  (let [[[vrx1 vry1] [vrx2 vry2]] view-rect
        wtapply #(->> % vec->point (.apply wt) point->vec)
        [zx zy] (wtapply [0 0])
        [cx cy] (wtapply [1 0])
        [rx ry] (wtapply [0 1])
        cdx (- cx zx), cdy (- cy zy)
        rdx (- rx zx), rdy (- ry zy)
        -a (- tile-advance), +a (+ 1 tile-advance)
        [[bx1 by1] [bx2 by2]] (bounding-rectangle
                               (mapv wtapply [[-a -a] [+a -a] [-a +a] [+a +a]]))
        vrps' (map #(->> % vec->point (.applyInverse wt) point->vec)
                   [[vrx1 vry1] [vrx2 vry1] [vrx2 vry2] [vrx1 vry2]])
        [[c1 r1] [c2 r2]] (advance-bounds (bounding-rectangle vrps') tile-advance)
        [[c1' r1'] [c2' r2']] tile-bounds]
    (for [c (range (max c1 c1') (inc (min c2 c2')))
          r (range (max r1 r1') (inc (min r2 r2')))
          :let [dx (+ zx (* c cdx) (* r rdx) (- zx))
                dy (+ zy (* c cdy) (* r rdy) (- zy))]
          :when (and
                 (<= (+ bx1 dx) vrx2)
                 (<= (+ by1 dy) vry2)
                 (>= (+ bx2 dx) vrx1)
                 (>= (+ by2 dy) vry1))]
      [c r])))


(defn- recreate-tiles-impl [tiles cells create-tile drop-tile!]
  (let [bset (set cells)]
    (as-> tiles $
      (transient $)
      (transduce
       (remove bset)
       (completing
        (fn [a cr]
          (when-let [t (get a cr)]
            (drop-tile! t))
          (dissoc! a cr)))
       $
       (keys tiles))
      (transduce
       (remove #(contains? tiles %))
       (completing
        (fn [a cr]
          (assoc! a cr (create-tile cr))))
       $
       cells)
      (persistent! $))))


(defjsclass TilemapSpriteImpl js/PIXI.Container

  (constructor [this [twidth theight] bounds advance create-tile]
    (call-super TilemapSpriteImpl this .constructor)
    (set! (.-tiles this) {})
    (set! (.-params this)
          {:twidth twidth
           :theight theight
           :bounds bounds
           :advance (or advance 0)
           :create-tile create-tile}))

  (updateTransform [this]
    (call-super TilemapSpriteImpl this .updateTransform)
    (.recreateTiles this))

  (recreateTiles [this]
    (let [{:keys [bounds twidth theight advance create-tile]} (.-params this)
          tiles (.-tiles this)
          vrect (gl/get-sprite-view-rect this)
          tx (.. this -worldTransform -tx)
          ty (.. this -worldTransform -ty)
          wt (.. this
                 -worldTransform
                 (clone)
                 (translate (- tx) (- ty))
                 (scale twidth theight)
                 (translate tx ty))
          cells (all-rect-inner-ceil-points vrect wt bounds advance)
          tiles' (recreate-tiles-impl
                  tiles
                  cells
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
           tile-bounds
           tile-advance
           tile-size
           tile-group]
    :or {cache-as-bitmap false
         tile-bounds [[-inf -inf] [+inf +inf]]
         tile-advance 0.5
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
       tsize tbounds tadvance create-group))))
