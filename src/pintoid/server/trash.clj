
;; возвращает по предыдущей коорднате, текущей и текущему ускорению новую координату
(defn verle [x0 x1 a dt] 
  (let [
      x2 (+
          (* 2 x1)
          (- x0)
          (* a dt dt)
          )
    ] x2))

;; версия для 2-д варианта
(defn verle-2d [xy-prev xy a dt] 
  (let [
      [x0 y0] xy-prev
      [x1 y1] xy
      [ax ay] a
    ]
    [ (verle x0 x1 ax dt) (verle y0 y1 ay dt)])
)

;; разложение вектора между двумя точками на dx dy и квадрат расстояния 
(defn dist-decomposition [p1 p2]
  (let [ 
    [x1 y1] p1
    [x2 y2] p2
    dx (- x2 x1)
    dy (- y2 y1)
    r2 (+ (* dx dx) (* dy dy))
    ; d (.sqrt js/Math r2)
    ]
    [dx dy r2]
    ))


;; подсчет гравитационной силы между точками p1 p2 с массами m1 m2
(defn gravity [m1 m2 p1 p2]

  (let [
    G 9.1 ; гравитационная постоянная
    [rx ry r2] (dist-decomposition p1 p2)
    r (.sqrt js/Math r2) ;TODO: поменять на квадратный корень clojure
    F (* G m1 m2 (/ 1 r2))
    r3' (/ 1 (* r2 r))
    ]
    [
      (* rx F r3')
      (* ry F r3')
    ]
 ))

;; сумма двух векторов
(defn add-vector [p1 p2] 
  (let [
    [x1 y1] p1
    [x2 y2] p2
    ]
    [(+ x1 x2) (+ y1 y2)]))

;; умножение вектора на скаляр
(defn mul-vector [p c]
  (let [
      [x1 y1] p
    ]

    [(* x1 c) (* y1 c)])
  )

;; расчет гравитации между двумя объектами и добавление этой силы к результирующей силе
(defn add-force-between [obj1 obj2]
  (let [
    F (gravity  (:mass obj1) (:mass obj2) 
                (:position obj1) (:position obj2))
    ]
  [(assoc obj1 :force (add-vector (:force obj1) F)) (assoc obj2 :force (add-vector (:force obj2) F))]
  ))

;; добавление к объекту (ракете) силы тяги
(defn add-force [obj f]
  (assoc obj :force (add-vector (:force obj) f)))


(defn update-acceleration [obj]
  (let [
      acceleration (mul-vector (:force obj) (/ 1 (:mass obj)))
    ]
  (assoc obj :acceleration acceleration)))


(defn update [] 
  (update-forces)
  (update-accelerations)
  (update-positions)
)
