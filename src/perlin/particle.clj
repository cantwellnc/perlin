(ns particle
  (:require [quil.core :as q])
  (:import [processing.core PVector]))


(defrecord Particle [pos vel acc])

(defn p-update [p]
  (-> p
      (assoc :vel (.limit (.add (:vel p) (:acc p)) 1))
      (assoc :pos (.add (:pos p) (:vel p)))
      (assoc :acc (.mult (:acc p) 0))))

(comment
  (p-update (Particle. (PVector. 0 0) (PVector. 0 0) (PVector. 0 0))))

(defn apply-force [p force]
  (assoc p :acc (.add (:acc p) force)))


(comment
  (let [p (Particle. (PVector. 0 0) (PVector. 0 0) (PVector. 0 0))
        force (PVector. 1 1)]
    (apply-force p force)))



(defn show-p [p]
  (q/stroke 0 100)
  (q/stroke-weight 1)
  (let [x (.x (:pos p))
        y (.y (:pos p))]
    (q/point x y)))


(defn get-x [p] (-> p :pos (#(.x %))))

(defn get-y [p] (-> p :pos (#(.y %))))

(defn edges [p w h]
  (let [wrap-right-edge (fn [_p]
                          (if (> (get-x _p) w)
                            (Particle. (PVector. 0 (get-y p)) (:vel _p) (:acc _p))
                            _p))
        wrap-left-edge (fn [_p]
                         (if (< (get-x _p) 0)
                           (Particle. (PVector. w (get-y p)) (:vel _p) (:acc _p))
                           _p))
        wrap-top-edge (fn [_p]
                        (if (> (get-y _p) h)
                          (Particle. (PVector. (get-x _p) 0) (:vel _p) (:acc _p))
                          _p))
        wrap-bottom-edge (fn [_p]
                           (if (< (get-y _p) 0)
                             (Particle. (PVector. (get-x _p) h) (:vel _p) (:acc _p))
                             _p))]
    (->
     p
     wrap-right-edge
     wrap-left-edge
     wrap-bottom-edge
     wrap-top-edge)))

(comment
  (let [p (Particle. (PVector. 20 20) (PVector. 0 0) (PVector. 0 0))
        zero-particle (Particle. (PVector. 0 0) (PVector. 0 0) (PVector. 0 0))
        w 10
        h 15]
    (assert (= (edges p w h) zero-particle))))



(defn clamp [val length scale]
  (let [max-val (int (- (/ length scale) 1))
        ;; note the - 1; this is because these vals are being used as list indices!
        min-val 0]
    (cond
      (> val max-val) max-val
      (< val min-val) min-val
      :else (int val)))
  )

(defn follow [p vectors scale width height]

  (let [nearest-x (clamp (/ (get-x p) scale) width scale)
        nearest-y (clamp (/ (get-y p) scale) height scale)
        force (get-in vectors [nearest-x nearest-y])]
    (apply-force p force)))


(comment
  (let [particle (Particle. (PVector. 20 20) (PVector. 0 0) (PVector. 0 0))]
    (->
     particle
     (follow  [[(PVector. 1 0) (PVector. 1 0) (PVector. 1 0)] [(PVector. 1 0) (PVector. 1 0) (PVector. 1 0)] [(PVector. 1 0) (PVector. 1 0) (PVector. 1 0)]] 10)
     p-update)))
