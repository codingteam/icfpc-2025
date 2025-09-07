(ns icfpc2015.main
  (:use clojure.core.logic))

; Let's explore a simpler system. Imagine we have two rooms, each with two doors. Each door can lead to any of the rooms (including to itself).
; Route: 0 1 0 1
; Route result: 0 0 1 1 0
; Map:
;   ↕     ↕
;  "0" ↔ "1"
;

(def plan [0 1 0 1])
(def labels [0 0 1 1 0])
(def room-count 2)

(defmacro with-room [room & body]
  (let [label (gensym)
        doors (take room-count (repeatedly #(gensym))]
  `(fresh [label# d0# r0# d1# r1#]
    (let [~room {:label label#
                 :doors [{:room r0# :door d0#}
                         {:room r1# :door d1#}]}]
      (conde

      )
      ~@body
    )
  )
)

(defn -main [& args]
  (println (macroexpand (with-room room1)))
)

;; (def route [])
;;
;; (defn do-solve []
;;   (run* [rooms-q]
;;     (let [room0 {:label l0 :doors [{:room r00 :door d01}]}
;;           room1])
;;
;;     (with-room room0
;;       (with-room room1
;;
;;         (== rooms [room0 room1])
;;
;;         (== (:label room0) 0)
;;         ()
;;       )
;;     )
;; ;;     (fresh [room1 room2]
;; ;;     )
;;   )
;; )
;;
;; (defn -main [& args]
;;   (println (do-solve)))
