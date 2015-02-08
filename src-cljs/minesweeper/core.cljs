(ns minesweeper.core
  (:require [clojure.string :as string]
            [reagent.core :as r]
            ))

(enable-console-print!)

(def difficulties 
  {:beginner      [8 8 10]
   :intermediate  [16 16 40]
   :expert        [30 16 99]})

;; control the sound
(def playing (r/atom nil))
;; current time
(def time-tick (r/atom 0))


(defn set-tick[]
  (reset! time-tick (.getTime (js/Date.))))

;; game status
(def game-state 
  (r/atom
   {
  ;  :board []
  ;  :neighbor []
  ;  :state [] ;; :hide, :open, :mark, :question

     :first true
     :start 0
    }))


(defn play-sound [result]
  (when (.-HTMLAudioElement js/window)
    (let [sounds {:failed ["sad", "bomb"] :completed ["success", "victory"]}] 
;;    (println "playing " (sounds result))
    (let [audio-obj (js/Audio. (str "sounds/" (rand-nth (sounds result)) ".mp3"))]
    (reset! playing audio-obj) 
    (.play audio-obj )))))


(defn update-timer []
  (let [game @game-state]
;;    (println "timer triggered")
    (when-not (or (game :first) (game :end))
;;      (println "Changed seconds")
      (set-tick))))


(defn stop-sound [] (when-let [ audio-obj @playing] (.pause audio-obj)))


(def setting (r/atom { :difficulty :beginner}))

(defn gen-board [x y mines]
  (->> (repeat mines 1)
       (concat (repeat (- (* x y) mines) 0 ))
       (shuffle)
       (partition x)
       (map vec)
       (vec)))

(defn neighbor-xy [x y]
  ;;return all the neighbor's index but doesn't include itself
  ;;may be out of bound
  (for [a (list (dec x)  x (inc x)) 
        b (list (dec y) y (inc y)) :when (not (and (= x a) (= y b)))] [a b]))
   

(defn count-neighbor [xx yy board ]
  (->> 
    (for [x (range xx) y (range yy)] 
      (->> 
        (neighbor-xy x y)
        (map #(get-in board % 0))
        (apply +)))
    (partition xx)
    (map vec)
    (vec) ))

(defn init-game [ x y mines] 
  (let [board (gen-board x y mines)]
  {:x x
   :y y
   :board board
   :counts (count-neighbor x y  board) ;mines of the neighbors
   :state (vec (for [a (range y)]
                 (vec (for [b (range x)] 
                     :hide ))))
   :mines mines
   :end false
   :first true
   :start 0 ;;timestamp
   }))

(defn all-open? [game]
  (= (->> (game :board)
          (apply concat)
          (filter #(not= 1 %))
          (count))
     (->> (game :state)
          (apply concat)
          (filter #(= :open %))
          (count))))

(defn complete-game [game]
  (play-sound :completed)
  (-> game
      (assoc-in [:end] :completed)))

(defn fail-game [game]
  (play-sound :failed)
  (-> game
      (assoc-in [:end] :failed)))

(defn open-neighbors [game col mark-to-open]
  (if (seq col)
    (let [ [x y]    (peek col)  
         can-open #{:hide :question}
         to-open  (->> (neighbor-xy x y)
                       (filter #(can-open (get-in game (into [:state] %)) ))
                       (filter (complement mark-to-open))) ] 
      (recur 
        (reduce #(assoc-in % (into [:state] %2) :open) game to-open)
        (into (pop col) (filter #(zero? (get-in game (into [:counts] %) 99)) to-open))
        (into mark-to-open to-open)))
    ;; end of traverse
    (if (all-open? game) (complete-game game) game)))

;; make the first click lucky
;; and swap
(defn swap-cell [game c1 c2]
  (println "swapping" c1 c2)
  (let [c1v      (get-in game (into [:board ] c1) )
        c2v      (get-in game (into [:board ] c2) )
        to-count (-> #{c1 c2} 
                     (into (apply neighbor-xy c1))
                     (into (apply neighbor-xy c2)))
        ;swap the bomb
        game     (-> game
                     (assoc-in (into [:board] c1) c2v)
                     (assoc-in (into [:board] c2) c1v))]
    (assoc game :counts 
           (reduce #(if (get-in % %2)  ;could be outofbound
                      (assoc-in % %2 (apply count-neighbor %2))
                      %)
                      (game :counts) to-count))))
        

;; find a cell without bomb
(defn find-space [game]
  (let [rx (take (game :x) (drop (rand-int (game :x)) 
                 (cycle  (range (game :x)))))
        ry (take (game :y) (drop (rand-int (game :y))
                 (cycle  (range (game :y)))))]
  (last (for [x rx y ry  :while (zero? (get-in game [:board x y]))]
    [x y]))))


(defn first-click [game x y]
  (set-tick)
  (if (= 1 (get-in game [:board x y]) )
    (swap-cell game [x y] (find-space game))
    game))


(defn click-game [game x y]
  (let [game (if (game :first) 
               (-> game
                (first-click x y)
                (assoc :start (.getTime (js/Date.)) )
                (assoc :first false))
               game) ]
    (condp = (get-in game [:state x y])
      :mark game
      :open game 
      (let [game (assoc-in game [:state x y] :open)]
           (cond 
             (= (get-in game [:board x y ]) 1) (fail-game game)  ;bomb
             (all-open? game) (complete-game game)               ;all openned
             (zero? (get-in game [:counts x y])) (open-neighbors game [[x y]] #{[x y]}) ;open all neighors
             :else game)))))


(defn mark-game [game x y]
  (let [state (condp = (get-in game [:state x y])   
               :hide :mark
               :open :open
               :mark :question
               :question :hide)
        game (assoc-in game [:state x y ] state) ]
    (condp = state
      :mark (update-in game [:mines] dec)
      :question (update-in game [:mines] inc)
      game)))

;;;
;;;
;;; open those unopened neighbors
;;; if counts == marked neighbors
;;;
(defn explore-game [game x y]
  (if (not= (get-in game [:state x y]) :open)
    game
    (let [to-open (->> (neighbor-xy x y)
                       (filter #(#{:hide :question} (get-in game (into [:state] %) :outbound))))
          marked  (->> (neighbor-xy x y)
                       (filter #(= :mark (get-in game (into [:state] %) :outbounded)))
                       (count))
          counts  (get-in game [:counts x y])]
      (cond
        (= 0 counts) game
        (not= marked counts) game
        :else (reduce #(apply click-game % %2) game to-open)))))

(defn new-game! [difficulty]
  (stop-sound)
  (reset! time-tick 0)
  (reset! game-state (apply init-game (difficulties difficulty))))

(defn print-game [game]
  (println "=================")
  (when (game :end)
  (println "*    " (name (game :end)) "     *")
  (println "================="))
  (doall (map #(println (apply str (map str %))) (game :board)))
  (println "=================")
  (doall (map #(println (apply str (map (comp str first name ) %))) (game :state)))
  (println "=================")
  (doall (map #(println (apply str (map str %))) (game :counts)))
  (println "================="))

(defn click! [x y]
  (when-not (@game-state :end)
    (swap! game-state click-game x y)))

(defn mark! [x y]
  (when-not (@game-state :end)
    (swap! game-state mark-game x y)))

(defn explore! [x y]
  (when-not (@game-state :end)
    (swap! game-state explore-game x y)))
;;
;; :open -> dblclick
;; :mark -> rightclick
;; :question -> click, rightclick
;; :hide -> click, rightclick
(defn cell [x y ]
  (let [game  @game-state
        state (get-in game [:state x y] )
        c     (get-in game [:counts x y])
        value (get-in game [:board x y])]
       [:span.cell
          {
          :on-click #(click! x y)
          :on-context-menu  #(do (mark! x y ) false)
          :on-double-click #(explore! x y)
          :class (cond 
                   (= state :open) (if (zero? value)
                                     (str "open_" c)
                                     "cell_mine")
                   :else (str "cell_" (name state)))}
         (cond 
                   (= state :open) (cond
                                     (= 1 value) "X"
                                     (> c 0) (str  c)
                                     :else "0")
                   :else (first (name state)))

          ]))
(defn game-row [game y]
  (into [:div.game-row ] (for [x (range (game :x))] (cell x y ))))

(defn format [n]
  (let [s (str n)]
    (str (apply str (repeat (- 3 (count s)) 0)) s)))
(defn timer-component[]
;;  (println "trigger timer and rendering timer component ")
  (js/setTimeout update-timer 200)
  [:input#display {:type      "text" 
                   :read-only true
                   :value     (format (min 999 (quot (- @time-tick (@game-state :start)) 1000)))} ])


;; UI components
(defn game-table []
  (let [game @game-state] 
    (into [:div.board-panel 
           [:div.control-row.row  
            [:input#display {:type "text" 
                             :read-only true
                             :value (format (game :mines))} ]
            [:button#control {:on-click #(new-game! :beginner)} "\u263A" ] 
            (timer-component)
            ]]

        (for [y (range (game :y))]
          (game-row game y)))))

;; Render the root component
(defn start []
  (r/render-component 
   [game-table]
   (.getElementById js/document "root")))

