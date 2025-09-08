(ns icfpc2025.main
  (:use clojure.pprint
        clojure.core.logic))

; Let's explore a simpler system. Imagine we have two rooms, each with two doors. Each door can lead to any of the rooms (including to itself).
; Route: 0 1 0 1
; Route result: 0 0 1 1 0
; Map:
;   ↕     ↕
;  "0" ↔ "1"
;

(def plan [0 1 0 1])
(def labels [0 0 1 1 0])
(def door-in-room-count 2)
(def room-count 2)

(defmacro with-room [room & body]
  (let [label (gensym)
        door-target-rooms (take door-in-room-count (repeatedly gensym))
        door-target-doors (take door-in-room-count (repeatedly gensym))]
    `(fresh ~(vec (concat [label] door-target-rooms door-target-doors))
      (let [~room {:label ~label
                   :doors ~(vec (map (fn [r d] {:room r :door d}) door-target-rooms door-target-doors))}]
        ~@body
      )
    )
  )
)

(def room-number-range (range room-count))
(def door-number-range (range door-in-room-count))

(defn setup-room
  "General structure here: each door gets its own root conde block, containing a lot of conditions for each sub-door of each other room."
  [all-rooms room]
  (let [room-index (-> all-rooms (.indexOf room))]
    (->>
      (:doors room)
      (map-indexed
        (fn [door-index door]
          (membero (:door door) door-number-range)
          (membero (:room door) room-number-range)
          (conde
            (concat
              (->>
                all-rooms
                (map-indexed
                  (fn [other-room-index other-room]
                    (if (= other-room-index room-index)
                      ; For room looping back to itself, all the doors should be short-circuited.
                      [
                        [(== (:room door) other-room-index)
                         (== (:door door) door-index)]
                      ]
                      ; For the other room connected to the current door, generate a list of clauses about possible
                      ; connections to each of the other room's doors.
                      (->>
                        (:doors other-room)
                        (map-indexed
                          (fn [other-door-index other-door]
                            ; If the current door connects to the other room, corresponding door of the other room should
                            ; connect to here.
                            [(== (:room door) other-room-index)
                             (== (:door door) other-door-index)
                             (== (:room other-door) room-index)
                             (== (:door other-door) door-index)]
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(defn do-solve []
  (run* [rooms-q]
    (with-room room0
      (with-room room1
        (let [rooms [room0 room1]]
          (setup-room rooms room0)
          (setup-room rooms room1)

          (== rooms-q rooms)

          ; Now, concrete facts:
          (== (:doors room0) [{:door 0 :room 0} {:door 1 :room 1}])
          (== (nth (:doors room1) 0) {:door 0 :room 1})
          ; let the engine to guess where the door 1 from the room 1 connects to
        )
      )
    )
  )
)

(defn -main [& args]
  (println (do-solve))
)
