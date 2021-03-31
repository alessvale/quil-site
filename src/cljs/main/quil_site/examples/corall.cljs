(ns quil-site.examples.corall
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
			quil-site.main ;DELETE
			))



;;;; Example by Alessandro Valentino
;;	http://www.kimri.org/
;;;;

(def pi 3.1415)

(def max_part 100)

(defn drawPart [p]
 (let [{x :x y :y size :size color :color} p]
    
    (q/no-stroke)
    (q/fill color)
    (q/ellipse x y (* 2 size) (* 2 size))
 )
)


(defn makePart [id]
   "Create a particle through an id in order to distinguish them"
  (let [angle (q/random 0.0 (* 2 pi))
  r 600]
  {
     :id id
     :x (+ (/ (q/width) 2) (* r (Math/cos angle)))
     :y (+ (/ (q/width) 2) (* r (Math/sin angle)))
     :size (* (q/random 0.01 0.03) (q/width))
     :color (rand-nth [[253 89 1 (q/random 100 200)] [250 171 54 (q/random 100 200)]])
     :fixed false
  }
    )
  )


(defn computeForce [p]
   "Compute force towards the center"
  (let [F_x (- (/ (q/width) 2.0 ) (:x p))
      F_y (- (/ (q/height) 2.0)  (:y p))]
      (mapv #(/ % (Math/sqrt (+ (* F_x F_x) (* F_y F_y)))) [F_x F_y])
     )
)



(defn movePart [p]
  "Only perform update if the particle is not fixed"
  (if (= (:fixed p) true)
  p
  (let [{x :x y :y fixed :fixed} p
    [F_x F_y] (computeForce p)]
  (-> p 
    (update :x (fn [val] (+ val (q/constrain (* 0.9 (/ 1000 (q/width)) F_x) -2.0 2.0))))
    (update :y (fn [val] (+ val (q/constrain (* 0.9 (/ 1000 (q/height)) F_y) -2.0 2.0))))
    )
  ))
)


(defn check [[p q]] 
   "Check distance between two particles "
  (let [ {x1 :x y1 :y s1 :size id1 :id} p 
          {x2 :x y2 :y s2 :size id2 :id} q]
    (if (= id1 id2)
    false
    (< (q/dist x1 y1 x2 y2) (+ s1 s2))
    )
  ))


(defn is_fixed [p particles]
   "Return true if the particle should be fixed"
  (let [c (count particles)]
    (reduce #(or %1 %2) false (mapv check (mapv vector (repeatedly c (fn [] p)) particles)))
  )
)


(defn check_other [p particles]
   "Check particle"
  (let [num (count particles)]
      "Only perform computation if the particle is not fixed"
      (if (= (:fixed p) true)
        p
      (update p :fixed (fn [val] (is_fixed p particles)))
  )
  ))


(defn spawn [particles]
  "Spawning a new particle, and assigns a new unique id"
  (let [c (count particles)
        m (apply max (mapv #(:id %) particles))]      
    (if (and (< (Math/random) 0.01) (< c max_part))
      (concat particles [(makePart (+ m 1))])
      particles
    )
  ))

(defn setup []

  "Set the framerate "
  (q/frame-rate 60)
  
  "Color mode" 
  (q/color-mode :rgb)
  
  "Helper var for the center particle"
    (def center {
      :id 0
      :x (/ (q/width) 2)
      :y (/ (q/height) 2)
      :size (* 0.1 (q/width))
      :color [0 95 96]
      :fixed true
     })
   { 
    :particles (concat [center] (mapv makePart (into [] (range 1 5))))
    }
  )

(defn update-state [state]
  
    "We update the particles set, then check if it is possible to spawn new particles"
    {
      :particles (spawn (mapv movePart (mapv (fn [p] (check_other p (:particles state))) (:particles state))))
    }
  )

(defn draw-state [state]
  
  (q/background 0 128 131)

  (doseq [p (:particles state)]
    (drawPart p)
    )
  )


(defn run-sketch [host size] ;DELETE
(q/sketch
  :host host
  :size [size size]
  :title "Corall"
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
) ;DELETE

(quil-site.main/register-example! "corall" "Alessandro Valentino" run-sketch) ;DELETE
