(ns pintoid.client.graphics.tilemap
  (:require
   [cljsjs.pixi]
   [pintoid.client.graphics.sprite :as s]
   [pintoid.client.graphics.layer :as gl]
   [pintoid.client.graphics.utils
    :refer [minmax -inf +inf point->vec vec->point]]
   [pintoid.client.graphics.layer :as gl])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [defjsclass call-super]]))


(defn- rectangle-intersect? [[[ax1 ay1] [ax2 ay2]] [[bx1 by1] [bx2 by2]]]
  (let [[ax1' ax2'] (minmax ax1 ax2)
        [ay1' ay2'] (minmax ay1 ay2)
        [bx1' bx2'] (minmax bx1 bx2)
        [by1' by2'] (minmax by1 by2)]
    (and
     (<= (max ax1' bx1') (min ax2' bx2'))
     (<= (max ay1' by1') (min ay2' by2')))))


(defn- bounding-rectangle [points]
  (let [xs (map first points)
        ys (map second points)]
    [[(reduce min xs) (reduce min ys)]
     [(reduce max xs) (reduce max ys)]]))


(defn- rectangle+vector [[[x1 y1] [x2 y2]] [x y]]
  [[(+ x x1) (+ y y1)] [(+ x x2) (+ y y2)]])


(defn- advance-bounds [[[x1 y1] [x2 y2]] a]
  [[(js/Math.floor (- (min x1 x2) a))
    (js/Math.floor (- (min y1 y2) a))]
   [(js/Math.ceil (+ (max x1 x2) a))
    (js/Math.ceil (+ (max y1 y2) a))]])


(defn- calculate-view-bounds [view-rect wt tile-advance]
  (as-> view-rect $
   (map #(->> % vec->point (.applyInverse wt) point->vec) $)
   (bounding-rectangle $)
   (advance-bounds $ tile-advance)))


(defn- rect-to-all-coords [[[x1 y1] [x2 y2]]]
  (for [x [x1 x2] y [y1 y2]]
    [x y]))


(defn- all-rect-inner-ceil-points [view-rect wt tile-bounds tile-advance]
  (let [[zx zy] (point->vec (.apply wt (vec->point [0 0])))
        [cx cy] (point->vec (.apply wt (vec->point [1 0])))
        [rx ry] (point->vec (.apply wt (vec->point [0 1])))
        cdx (- cx zx), cdy (- cy zy)
        rdx (- rx zx), rdy (- ry zy)
        -a (- tile-advance), +a (+ 1 tile-advance)
        rbounds (bounding-rectangle
                 (map
                  (comp point->vec #(.apply wt %) vec->point)
                  [[-a -a] [-a +a] [+a +a] [+a -a]]))
        tile-visible? (fn [c r]
                        (rectangle-intersect?
                         view-rect
                         (rectangle+vector
                          rbounds
                          [(+ (* c cdx) (* r rdx)) (+ (* c cdy) (* r rdy))])))
        vrps' (mapv #(->> % vec->point (.applyInverse wt) point->vec)
                    (rect-to-all-coords view-rect))
        [[c1 r1] [c2 r2]] (advance-bounds (bounding-rectangle vrps') tile-advance)
        [[c1' r1'] [c2' r2']] tile-bounds]
    (for [c (range (max c1 c1') (inc (min c2 c2')))
          r (range (max r1 r1') (inc (min r2 r2')))
          :when (tile-visible? c r)]
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
    (set! (.-tms-state this)
          {:tiles {}
           :vbnds [0 -1 0 -1]
           :twidth twidth
           :theight theight
           :bounds bounds
           :advance (or advance 0)
           :create-tile create-tile}))

  (updateTransform [this]
    (call-super TilemapSpriteImpl this .updateTransform)
    (.recreateTiles this))

  (recreateTiles [this]
    (let [{:keys [tiles bounds twidth theight vbnds advance create-tile]
           :as state} (.-tms-state this)
          tx (.. this -worldTransform -tx)
          ty (.. this -worldTransform -ty)
          wt (.. this
                 -worldTransform
                 (clone)
                 (translate (- tx) (- ty))
                 (scale twidth theight)
                 (translate tx ty))
          vrect (gl/get-sprite-view-rect this)
          vbnds' (calculate-view-bounds vrect wt advance)]
      (when (not= vbnds vbnds')
        (let [cells (all-rect-inner-ceil-points vrect wt bounds advance)
              tiles' (recreate-tiles-impl
                      tiles
                      cells
                      (fn [[c r :as cr]]
                        (let [s (create-tile cr)]
                         (set! (.. s -position -x) (* c twidth))
                         (set! (.. s -position -y) (* r theight))
                        s))
                      (fn [x]
                        (.destroy x true)))
              ta (into [] (remove nil?) (vals tiles'))]
          (run! #(do (set! (.-parent %) this) (.updateTransform %)) ta)
          (set! (.-children this) (apply array ta))
          (set! (.-tms-state this) (assoc state :tiles tiles' :vbnds vbnds')))))))


(defn make-tilemap-sprite
  [create-tile
   {:keys [cache-as-bitmap
           tile-bounds
           tile-advance
           tile-size
           tile-group]
    :or {cache-as-bitmap false
         tile-bounds [[-inf -inf] [+inf +inf]]
         tile-advance 0.5
         tile-size [16 16]
         tile-group [4 4]}
    :as proto}]
  (let [[otw oth] tile-size
        [gw gh :as group-size] tile-group
        subtiles (vec (for [c (range gw), r (range gh)] [c r]))
        [[tb11 tb12] [tb21 tb22]] tile-bounds
        create-group (fn [[gc gr]]
                       (let [cont (js/PIXI.Container.)]
                         (doseq [[c r] subtiles]
                           (let [cc (+ (* gc gw) c)
                                 rr (+ (* gr gh) r)
                                 s (create-tile [cc rr])]
                             (.addChild cont s)
                             (set! (.. s -position -x) (* c otw))
                             (set! (.. s -position -y) (* r oth))))
                         (set! (.-cacheAsBitmap cont) cache-as-bitmap)
                         cont))
        s (TilemapSpriteImpl.
           [(* otw gw) (* oth gh)]
           [[(js/Math.floor (/ tb11 gw)), (js/Math.floor (/ tb12 gh))]
            [(js/Math.ceil (/ tb21 gw)), (js/Math.ceil (/ tb22 gh))]]
           (/ tile-advance (min gw gh))
           create-group)]
    (s/set-sprite-properties! s proto)))


(defmethod s/construct-sprite-object :random-tilemap [proto props]
  (let [tsprites (:tiles proto)
        create-tile #(s/make-sprite (nth tsprites (mod (hash %) (count tsprites))))
        s (make-tilemap-sprite create-tile proto)]
    (s/set-sprite-properties! s props)))
