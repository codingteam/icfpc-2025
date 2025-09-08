(ns icfpc2025.main
  (:use clojure.pprint
        clojure.core.logic))

(def door-in-room-count 6)
(def room-count 3)

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

(defn setup-roomo
  "General structure here: each door gets its own root block, containing a lot of conditions for each sub-door of each other room."
  [all-rooms room]
  (let [room-index (-> all-rooms (.indexOf room))]
    (and*
      (->>
        (:doors room)
        (map-indexed
          (fn [door-index door]
            (and*
              [
                (membero (:door door) door-number-range)
                (membero (:room door) room-number-range)
                (or*
                  (->>
                    all-rooms
                    (map-indexed
                      (fn [other-room-index other-room]
                        (if (= other-room-index room-index)
                          ; For room looping back to itself, all the doors should be short-circuited.
                          (conde
                            [(== (:room door) other-room-index)
                             (== (:door door) door-index)]
                          )
                          ; For the other room connected to the current door, generate a list of clauses about possible
                          ; connections to each of the other room's doors.
                          (or*
                            (->>
                              (:doors other-room)
                              (map-indexed
                                (fn [other-door-index other-door]
                                  ; If the current door connects to the other room, corresponding door of the other room
                                  ; should connect to here.
                                  (and*
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
              ]
            )
          )
        )
      )
    )
  )
)

; Example exploration plan and result:
(def plan   [  3 0 1 0 4 5 3 5 2 4 2 1])
(def result [0 0 1 2 2 1 1 1 1 0 1 0 2])

(defn facto [rooms]
  (and*
    [
      (== (:label (nth rooms 0)) (first result))

      (and*
        (map
          (fn [prev-label door-index next-label]
            (or*
              (map-indexed
                (fn [prev-room-index prev-room]
                  (and*
                    [
                      (== (:label prev-room) prev-label)
                      (or*
                        (map-indexed
                          (fn [next-room-index next-room]
                            (and*
                              [
                                (== (:label next-room) next-label)
                                (== (:room (nth (:doors prev-room) door-index)) next-room-index)
                                (or*
                                  (map
                                    (fn [next-room-door]
                                      (and*
                                        [
                                          (== (:door next-room-door) door-index)
                                          (== (:room next-room-door) prev-room-index)
                                        ]
                                      )
                                    )
                                    (:doors next-room)
                                  )
                                )
                              ]
                            )
                          )
                          rooms
                        )
                      )
                    ]
                  )
                )
                rooms
              )
            )
          )
          result
          plan
          (rest result)
        )
      )
    ]
  )
)

(defn do-solve []
  (run 1 [rooms-q]
    (with-room room0
      (with-room room1
        (with-room room2
          (let [rooms [room0 room1 room2]]
            (conde
              [
                (setup-roomo rooms room0)
                (setup-roomo rooms room1)
                (setup-roomo rooms room2)

                ; Now, concrete facts:
                (facto rooms)

                (== rooms-q rooms)
              ]
            )
          )
        )
      )
    )
  )
)

(defn -main [& args]
  (pprint (do-solve))
)
