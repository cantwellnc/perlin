(ns perlin.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [particle :as p])
  (:import [processing.core PVector]
           [particle Particle]))


;; What is Perlin Noise? 
;; Ken Perlin developed this noise algorithm for TRON 
;; - originally used for procedural textures in computer animation
;; - he won an oscar for technical achievement for this!
;; 1D :
;; - "smooth" noise

;; What will we need? 




(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  (q/background 255)
  
  ; setup function returns initial state. 

  {:x 0
   :z 0
   :scale 10
   :particles (for [_ (range 500)]
                (p/Particle.
                 (PVector. (rand-int (q/width)) (rand-int (q/height)))
                 (PVector. 0 0)
                 #_(PVector. (Math/cos (rand (* 2 Math/PI))) (Math/sin (rand (* 2 Math/PI))))
                 (PVector. 0 0)))
   :cols (Math/floor (/ (q/width) 10))
   :rows (Math/floor (/ (q/height) 10))

  ;;  this loop has this weird vector structure to make sure we are only creating #cols * #rows  
  ;;  noise vectors. before, the dimensions were all wacky bc we were just iterating over the noise range.
  ;;  note also the casting of the lazy for-comp (which yields a seq) into a vec to play nicely with get-in
   :flow-field (->> (for [[_ xoff] (map vector
                                        (range #_(:x state) 0 (q/width) 0.01)
                                        (range (Math/floor (/ (q/width) 10))))]
                      (->> (for [[_ yoff] (map vector
                                               (range #_(:x state) 0 (q/height) 0.01)
                                               (range (Math/floor (/ (q/height) 10))))]
                             (let [noise (* 255 (q/noise xoff yoff 0))
                                   v (PVector/fromAngle (* 2 Math/PI noise 1.310))]
                               (.setMag v 2.5)
                               v))
                           (into [])))
                    (into []))})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (-> state
      (assoc :z (+ (:z state) 0.001))
      (assoc :particles (for [particle (:particles state)]
                          (->
                           particle
                           (p/follow (:flow-field state) (:scale state) (q/width) (q/height))
                           p/p-update
                           (#(p/edges % (q/width) (q/height))))))
      
      ;; DRY this up to make it so the flow field code isn't copied in multiple places
      (assoc :flow-field (->> (for [[_ xoff] (map vector
                                                  (range #_(:x state) 0 (q/width) 0.01)
                                                  (range (Math/floor (/ (q/width) 10))))]
                                (->> (for [[_ yoff] (map vector
                                                         (range #_(:x state) 0 (q/height) 0.01)
                                                         (range (Math/floor (/ (q/height) 10))))]
                                       (let [noise (* 255 (q/noise xoff yoff (:z state)))
                                             v (PVector/fromAngle (* 2 Math/PI noise 1.31023))]
                                         (.setMag v 0.5)
                                         v))
                                     (into [])))
                              (into [])))
      ))


(comment
  (defn dot [[x y] [x' y']]
    (+ (* x x') (* y y')))


  (defn magnitude [vec] (Math/sqrt (dot vec vec)))

  (defn normalize [vec]
    (map #(/ % (magnitude vec)) vec))


  (defn normal? [vec] (= (magnitude vec) 1.0))


  (defn unit-vector [x]
    [(Math/cos x) (Math/sin x)])


  (defn random-direction-at [[x y]]
  ;; this actually needs to be pseudorandom in the sense
  ;; that it always needs to return the same value for the same x,y
    )


  (comment
  ;; ok good, my helpers are working :) 
    (assert (= (normalize (unit-vector 0.3)) (unit-vector 0.3)))
    (assert (normal? (unit-vector 0.555)))

    (normal? (random-direction-at [2 2])))



  (defn v+ [v1 v2] (map + v1 v2))

  (defn v- [v1 v2] (map - v1 v2))

  (defn v* [v1 v2] (map * v1 v2))



  (defn noise [[x y :as input-point]]
  ;; define the grid points around (x, y) 
    (let [upper-left [(Math/ceil x) (Math/ceil y)]
          upper-right [(Math/ceil x) (Math/ceil y)]
          lower-left [(Math/floor x) (Math/floor y)]
          lower-right [(Math/floor x) (Math/floor y)]
          grid-pts [lower-left lower-right upper-left upper-right]
          [x-bias y-bias] lower-left
        ;; compute diffs from grid points
          diffs (map (fn [grid-point] (map #(- %1 %2) grid-point input-point)) grid-pts)
          random-dirs (map random-direction-at grid-pts)
        ;; get dot product
          [s t u v] (map dot random-dirs diffs)
          smoothing-kernel (fn [x bias] (- (* 3 (Math/pow (- x bias) 2)) (* 2 (Math/pow (- x bias) 3))))]
    ;; 3p² - 2p³
    ;; return the average weighted sum of these
    ;; a = s + Sx (t - s)
    ;; b = u + Sx (v - u)
      (let [a
            (+ s (* (smoothing-kernel x x-bias) (- t s)))

            b (+ u (* (smoothing-kernel x x-bias) (- v u)))]
        (print random-dirs)


        (+ a (* (smoothing-kernel y y-bias) b)))))


  (noise [5 300]))


(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.

  (comment
    ;; visualization of 1-d perlin noise
    (q/begin-shape)
    (doseq [[x xoff] (map vector
                          (range (q/width))
                          (range (:x state) (q/width) 0.01))]
      (q/stroke 0)
      (q/vertex x (* (q/height) (q/noise xoff))))
    (q/end-shape))


  (comment
    ;; original 2d perlin noise code
    (doseq [[x xoff] (map vector
                          (range (q/width))
                          (range (:x state) (q/width) 0.01))]
      (doseq [[y yoff] (map vector (range (q/height)) (range 0 (q/height) 0.01))]
        (let [with-noise (* 255 (q/noise xoff yoff))]

          (q/set-pixel x y (q/color with-noise))))))

  (doseq [particle (:particles state)]
    (p/show-p particle))

  ;; todo: fix this so that we calculate the flow field once for 
  ;; all vectors + make sure it actually looks like a grid. 
  (doseq [[x vecs] (map vector
                        (range (:cols state)) (:flow-field state))]
    (doseq [[y v] (map vector
                       (range (:rows state)) vecs)]
      (q/with-rotation
        [(.heading v)] ;; note that heading must be a vector due to a quirk of the quil api
        (q/with-translation
          [(* x (:scale state)) (* y (:scale state))]
          ;; todo: separate out the drawing steps 
          (q/stroke-weight 1)
          (q/stroke 0 3)
          (q/line 0 0 (:scale state) 0)))))


  (comment
    (doseq [[x xoff] (map vector
                          (range (:cols state))
                          (range (:x state) (q/width) 0.01))]
      (doseq [[y yoff] (map vector (range (:rows state)) (range 0 (q/height) 0.01))]
        (let [noise (* 255 (q/noise xoff yoff (:z state)))
              v (PVector/fromAngle (* 2 Math/PI noise))]
          (q/with-rotation
            [(.heading v)] ;; note that heading must be a vector due to a quirk of the quil api
            (q/with-translation
              [(* x (:scale state)) (* y (:scale state))]
              (q/stroke-weight 1)
              (q/stroke (q/random 255))
              (q/line 0 0 (:scale state) 0))))))))


(q/defsketch perlin
  :title "perlin noise flow field"
  :size [500 500]

  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state

  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
