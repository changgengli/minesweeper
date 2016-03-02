(ns minesweeper.core
  (:require [clojure.string :as string]
            [reagent.core :as r]))

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
  (r/atom { :first true :start 0 }))


(defn play-sound [result]
  (when (.-HTMLAudioElement js/window)
    (let [sounds {:failed ["sad", "bomb"] :completed ["success", "victory"]}] 
;;    (println "playing " (sounds result))
    (let [audio-obj (js/Audio. (str "sounds/" (rand-nth (sounds result)) ".mp3"))]
    (reset! playing audio-obj) 
    (.play audio-obj )))))


(defn update-timer []
  (let [game @game-state]
    (when-not (or (game :first) (game :end))
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

(defn surround [x y]
  ;;return all the neighbor's index but doesn't include itself
  ;;may be out of bound
  (for [a (list (dec x)  x (inc x)) 
        b (list (dec y) y (inc y)) :when (not (and (= x a) (= y b)))] [a b]))
   

(defn count-single-surround [board x y]
;;  (println "counting cell " x y "for board " board)
  (if (zero? (get-in board [x y]))
        (->> 
          (surround x y)
          (map #(get-in board % 0))
          (apply +))
        \X ))

;;count the whole board
(defn count-surround [xx yy board ]
;;  (println "counting " xx yy board)
  (->> 
    (for [x (range yy) y (range xx)] 
      (count-single-surround board x y))
    (partition xx)
    (map vec)
    (vec) ))

(defn init-game [ x y mines] 
  (println "new game ...")
  (let [board (gen-board x y mines)]
  {:x x
   :y y
   :board board
   :counts (count-surround x y  board) ;mines of the neighbors
   :states (vec (for [a (range y)]
                 (vec (for [b (range x)] 
                     :hide ))))
   :end false
   :first true
   :start 0 ;;timestamp
   :remains mines
   :need-open (- (* x y) mines)
   }))


(defn complete-game [game]
  (.ga  js/window 'send', 'event', 'win-game', 'click')
  (play-sound :completed)
  (-> game
      (assoc-in [:end] :completed)))

(defn fail-game [game]
  (.ga  js/window 'send', 'event', 'fail-game', 'click')
  (play-sound :failed)
  (-> game
      (assoc-in [:end] :failed)))

(defn open-cell [game x y]
  (-> game 
      (assoc-in [:states x y] :open)
      (update-in [:need-open] dec)))

(defn open-surround [game col mark-to-open]
  (if (seq col)
    (let [ [x y]    (peek col)  
         can-open #{:hide :question}
         to-open  (->> (surround x y)
                       (filter #(can-open (get-in game (into [:states] %)) ))
                       (filter (complement mark-to-open))) ] 
      (recur 
        (reduce #(apply open-cell %  %2) game to-open)
        (into (pop col) (filter #(zero? (get-in game (into [:counts] %) 99)) to-open))
        (into mark-to-open to-open)))
    ;; end of traverse
    (if (zero? (game :need-open)) (complete-game game) game)))

;; make the first click lucky
;; and swap
(defn swap-cell [game c1 c2]
;  (println "swapping" c1 c2)
  (let [c1v      (get-in game (into [:board ] c1) )
        c2v      (get-in game (into [:board ] c2) )
        to-count (-> #{c1 c2} 
                     (into (apply surround c1))
                     (into (apply surround c2)))
        ;swap the bomb
        game     (-> game
                     (assoc-in (into [:board] c1) c2v)
                     (assoc-in (into [:board] c2) c1v))]
    (assoc game :counts 
           (reduce #(if (get-in % %2)  ;could be outofbound
                      (assoc-in % %2 (apply count-single-surround (game :board) %2))
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
  ;;google analytics event
  (.ga  js/window 'send', 'event', 'new-game', 'click')
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
    (condp = (get-in game [:states x y])
      :flag game
      :open game 
      (let [game (open-cell game x y)]
           (cond 
             (= (get-in game [:board x y ]) 1)  (fail-game game)  ;bomb
             (zero? (game :need-open))           (complete-game game)               ;all openned
             (zero? (get-in game [:counts x y])) (open-surround game [[x y]] #{[x y]}) ;open all neighors
             :else game)))))


(defn mark-game [game x y]
  (let [state (condp = (get-in game [:states x y])   
               :hide :flag
               :open :open
               :flag :question
               :question :hide)
        game (assoc-in game [:states x y ] state) ]
    (condp = state
      :flag (update-in game [:remains] dec)
      :question (update-in game [:remains] inc)
      game)))

;;;
;;;
;;; open those unopened neighbors
;;; if counts == marked neighbors
;;;
(defn explore-game [game x y]
  (if (not= (get-in game [:states x y]) :open)
    game
    (let [to-open (->> (surround x y)
                       (filter #(#{:hide :question} (get-in game (into [:states] %) :outbound))))
          marked  (->> (surround x y)
                       (filter #(= :flag (get-in game (into [:states] %) :outbounded)))
                       (count))
          counts  (get-in game [:counts x y])]
      (cond
        (= 0 counts) game
        (not= marked counts) game
        :else (reduce #(apply click-game % %2) game to-open)))))


(defn print-game [game]
  (println "=*- " (name (or (game :end) :enjoy)) "-*=")
  (println "=================")
  (doall (map #(println (apply str (map str %))) (game :board)))
  (println "=================")
  (doall (map #(println (apply str (map (comp str first name ) %))) (game :states)))
  (println "=================")
  (doall (map #(println (apply str (map str %))) (game :counts)))
  (println "================="))

(defn new-game! [difficulty]
  (stop-sound)
  (reset! time-tick 0)
  (reset! game-state (apply init-game (difficulties difficulty))))


(defn click! [x y]
;;  (println @game-state)
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
;; :flag -> rightclick
;; :question -> click, rightclick
;; :hide -> click, rightclick
(def mine-str "\uD83D\uDCA3")
(def flag-str \u2691)
(def cross-mark \u274c)


;;output
;; burst:  open a bomb
;; hide_mine
;; bad_flag  : flag a non-bomb cell
;; open open_[0-8]  : open a cell 
;; cell_hide
;; cell_question
;; cell_flag
;;
(defn class-of [end? mine state surround]
  (cond
    (and end? (= 1 mine) (= state :open) )  "open burst" 
    (and end? (= 1 mine))                   "hide_mine"
    (and end? (= state :flag) (zero? mine)) "bad_flag"
    (= state :open)                         (str "open_" surround " open")
    (= state :flag)                         "cell_flag"
    end?                                    (str "cell_" (name state)) ;;disable actions
    :else                                   (str "cell_action cell_" (name state)) ))
(defn text-of [end? mine state surround]
  (cond 
    (and (= state :open) (= 1 mine)) mine-str
    (and end? (= 1 mine)) mine-str
    (and end? (= state :flag) (zero? mine)) cross-mark
    (= state :open) surround
    (= state :flag) flag-str
    (= state :question) \?
    (= state :hide) "@"))

(defn cell [end? x y mine state surround]
  (println "rending cell " x y)
  [:span.cell
   { :on-click #(click! x y)
     :on-context-menu (fn [e] (do (mark! x y ) (.preventDefault e) nil))
     :on-double-click #(explore! x y)
     :class (class-of end? mine state surround)
      } (text-of end? mine state surround)])


;; each row
(defn game-row [end? x mines states surrounds]
;  (println "rending row " x)
  (into [:div.game-row ] 
        (map vector (repeat cell) (repeat end?) (repeat x) (range) mines states surrounds)));

(defn format [n]
  (let [s (str (max 0 n))]
    (str (apply str (repeat (- 3 (count s)) 0)) s)))
;; the timer
(defn timer-component[]
;;  (println "trigger timer and rendering timer component ")
  (js/setTimeout update-timer 200)
  [:input.display {:type      "text" 
                   :read-only true
                   :value     (format (min 999 (quot (- @time-tick (@game-state :start)) 1000)))} ])

(defn control-component [] 
;;  (println "rendering control-component" )
  (let [game @game-state
        end? (game :end) ] 
           [:div.control-row  
            [:input.display {:type "text" 
                             :read-only true
                             :value (format (game :remains))} ]
            [:button#control 
             {:on-click #(new-game! (rand-nth [:beginner, :intermediate, :expert]))} 
             (or ({:failed \u2639} end?) \u263A) ] 
            (timer-component) ]))

;; The board
(defn game-table []
;;  (println "rendering " @game-state)
  (let [game @game-state
        end? (game :end) ] 
    (into 
      [:div.board-panel ] 
      (map vector (repeat game-row) (repeat end? ) (range) (game :board) (game :states) (game :counts)))))

;; Render the root component
(defn ^:export start []
  (new-game! :beginner)
  (r/render-component [control-component] (.getElementById js/document "controls"))
  (r/render-component [game-table] (.getElementById js/document "rows")))

