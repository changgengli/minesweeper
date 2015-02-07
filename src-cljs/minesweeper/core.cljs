(ns minesweeper.core
  (:require [clojure.string :as string]
            [reagent.core :as r]
            ))

(enable-console-print!)

(def difficulties 
  {:beginner      [5 5 5]
   :intermediate  [16 16 40]
   :expert        [30 16 99]})

;; control the sound
(def playing (r/atom nil))
(defn play-sound [result]
  (when (.-HTMLAudioElement js/window)
    (let [sounds {:failed "sad" :completed "success"}] 
    (println "playing " (sounds result))
    (let [audio-obj (js/Audio. (str "sounds/" (sounds result) ".mp3"))]
    (reset! playing audio-obj) 
    (.play audio-obj )))))

(defn stop-sound [] (when-let [ audio-obj @playing] (.pause audio-obj)))


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
  (doall println board)
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
   :end false
   }))

(defn all-open? [game]
  (println "all open? "  
    (->> (game :board)
          (apply concat)
          (filter #(not= 1 %))
          )
     (->> (game :state)
          (apply concat)
          (filter #(= :open %))
          ))
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

(defn click-game [game x y]
    (condp = (get-in game [:state x y])
      :mark game
      :open game 
      (let [game (assoc-in game [:state x y] :open)]
           (cond 
             (= (get-in game [:board x y ]) 1) (fail-game game) 
             (all-open? game) (complete-game game)
             (zero? (get-in game [:counts x y])) (open-neighbors game [[x y]] #{[x y]})
             :else game))))

(defn mark-game [game x y]
  (assoc-in game [:state x y ]
             (condp = (get-in game [:state x y])   
               :hide :mark
               :open :open
               :mark :question
               :question :hide)))


;; game status
(def game-state 
  (r/atom
   {
  ;  :board []
  ;  :neighbor []
  ;  :state [] ;; :hide, :open, :mark, :question
  ;  :failed
    }))

(defn new-game! [difficulty]
  (stop-sound)
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
    (swap! game-state click-game x y)
    (print-game @game-state)
    ))

(defn mark! [x y]
  (when-not (@game-state :end)
    (swap! game-state mark-game x y)
    (print-game @game-state)))


(defn cell [x y ]
  (let [game  @game-state
        state (get-in game [:state x y] )
        c     (get-in game [:counts x y])
        value (get-in game [:board x y])]
       [:td
          {
          :on-click #(click! x y)
          :on-contextmenu  #(mark! x y)
          :class (cond 
                   (= state :open) (if (zero? value)
                                     (str "open_" c)
                                     "mine")
                   :else (name state))
          } 
         (cond 
                   (= state :open) (if (zero? value) (str  c)
                                     "X")
                   :else (first (name state)))

          ]))

(new-game! :beginner)
(click! 0 0)
[@game-state]
(print-game @game-state)

(identity game-state)

;; UI components
(defn game-table[]
  (let [game @game-state] 
  [:div 
   [:p "XXAFDFAS" "&lt;" ]
   [:table
      [:tbody
        [:tr {:key "Control" } 
         [:td {:col-span (game :x)} 
          [:button {:on-click #(new-game! :beginner)}  "&#x1F60A;" ]]]
        (for [y (range (game :y))]
          [:tr {:key (str "row_" y)}
            (for [x (range (game :x))] (cell x y ))])]]]))





;; Render the root component
(defn start []
  (r/render-component 
   [game-table]
   (.getElementById js/document "root")))




