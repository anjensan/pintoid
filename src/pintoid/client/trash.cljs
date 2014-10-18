(ns pintoid.client.trash)

;; Интерполяция параболы по 3 точкам

(defn tri-inter [t1 t2 t3 v1 v2 v3]
  (let [ 
      x1 t1
      x2 t2
      x3 t3
      ; _ (println ">>" x1)
      y1 v1
      y2 v2
      y3 v3
      dy21 (- y2 y1)
      dx21 (- x2 x1)
      a-top (- y3 (/  (+  (* x3 dy21) 
                          (* x2 y1)
                          (- (* x1 y2)))
                      (- x2 x1)
                  ))
      a-bottom (+ (* x3 (+ x3 (- x1) (- x2)))
                  (* x1 x2))
      a (/ a-top a-bottom)
      b (-  (/  (- y2 y1) 
                (- x2 x1))
            (* a (+ x1 x2)))
      c (+  (/  (-  (* x2 y1) 
                    (* x1 y2))
                (- x2 x1)) 
            (* a x1 x2)
            )]
      (fn [time] 
        (+ (* a time time) (* b time) c))))
