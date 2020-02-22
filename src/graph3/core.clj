(ns graph3.core
  (:gen-class)
  (:use [seesaw core color font])
  (:require [clojure.reflect :refer [reflect]]
            [graph3.parse :refer [string->fn]]
            [seesaw.behave :as behave])
  (:import   (java.awt Stroke BasicStroke)
             (org.jfree.data.xy XYSeriesCollection XYSeries XYDataset)
             (org.jfree.chart.title TextTitle)
             (org.jfree.chart.renderer.xy XYLineAndShapeRenderer)
             (org.jfree.chart.plot XYPlot PlotOrientation)
             (org.jfree.chart.block BlockBorder)
             (org.jfree.chart JFreeChart ChartPanel ChartFactory)))

(def ^:private default-frame-values
  { :step 100
   :x-range-initial [-10 10]
   :x-range-min [-1 1]
   :x-range-max [-100 100]
   :width 700
   :height 700
   :image-width 500
   :image-height 500
   :stroke 1
   :background-color (color 240 240 240)})

(defn fn->series
  [f label low high step]
  (let [series (new XYSeries label)
        points (map (fn [n] [n (f n)])
                    (range low high (/ (- (inc high) low) step)))]     
    (doseq [[x y] points] (.add series x y))
    series))

(defn coll-series->dataset [coll]
  (let [series-coll (new XYSeriesCollection)]
    (doseq [s coll] (.addSeries series-coll s))
    series-coll))

(defn new-series-collection
  "Takes map of label -> fn. Returns XYSeriesCollection."
  ([m] (let [{[low high] :x-range-initial step :step} default-frame-values]
         (new-series-collection m low high step)))
  ([m low high step]
   (let [coll (reduce-kv
               (fn [acc k v] (conj acc (fn->series v k low high step)))
               []
               m)]
     (coll-series->dataset coll))))

(defn series-collection->chart [series-collection]
  (ChartFactory/createXYLineChart
   ""
   "x"
   "y"
   series-collection
   PlotOrientation/VERTICAL
   true
   true
   false))

(defn chart->image [chart width height]
  (.createBufferedImage chart width height))

(defn- set-chart-backgroud-color
  "Sets the background coloe of chart to c (a color),
  returns chart."
  [chart c]
  (-> chart (.getXYPlot) (.setBackgroundPaint c))
  chart)

;; FIXME: set-chart-stroke is not working; same result with varying n.

(defn- set-chart-stroke [chart n]
  (let [renderer (doto (new XYLineAndShapeRenderer)
                   (.setBaseStroke (new BasicStroke (float n))))]
    (-> chart (.getXYPlot) (.setRenderer renderer))
    chart))

(defn- fns->chart
  [m low high]
  (let [{background :background-color stroke :stroke step :step}
        default-frame-values]
    (-> m
        (new-series-collection low high step)
        series-collection->chart
        (set-chart-backgroud-color background)
        (set-chart-stroke stroke)
        )))

(defn- fns->chart-image
  "Given map, returns BufferedImage displaying
  the graphs of functions in it."
  [m low high]
  (let [{width :image-width height :image-height} default-frame-values]
    (-> m
        (fns->chart low high)
        (chart->image width height))))

;; helper functions for seesaw

(defn- get-widget
  "Returns a single item specified by id."
  [root id]
  (select root [(keyword (str "#" (name id)))]))

(defn- get-widgets-class
  "Returns a collection of items specified by a class tag."
  [root class-id]
  (select root [(keyword (str "." (name class-id)))]))

(defn- image-shower
  "Returns fn which displays BufferedImage objects."
  [im]
  (fn [c g] (.drawImage g im 0 0 nil)))

(defn- view-chart
  "Experimental method just for displaying function graphs."
  [m low high]  
  (let [im (fns->chart-image m low high)
        {:keys [width height]} default-frame-values 
        f (frame :width width :height height
                 :content (canvas :id :chart
                                  :paint (image-shower im)))]
    (show! f)))

;;(view-chart {"x" (fn [x] x) "x**2" (fn [x] (* x x ))} -10 20)

;;;;;;;;;;;;;;
;; make ui  ;;
;;;;;;;;;;;;;;

(def the-graph (atom {}))

(def the-range (atom []))

(defn- reset-graph! [] (reset! the-graph {}))

(defn- reset-range! []
  (reset! the-range (get default-frame-values :x-range-initial)))

(reset-range!)

(defn- graph-registered? [] (seq @the-graph))

(defn- get-chart-image
  "Returns chart as BufferedImage. The stateful atom registering
  functions get read."
  []
  (when (graph-registered?)
    (let [m @the-graph
          [low high] @the-range]
      (fns->chart-image m low high))))

(defn- update-canvas [root]
  (let [im (get-chart-image)]
    (config!
     (get-widget root :chart)
     :paint
     (when (some? im) (image-shower im)))))

(defn- buttons []
  (horizontal-panel
   :items  (->> [:view :reset :clear :close]
                (map (fn [k] (button :text (name k) :class :font :id k))))))

(defn- input-text []
  (horizontal-panel
   :items [(label :text "f(x)=" :class :font)
           (text :editable? true :id :input :class :font)]))

(defn- reset-input [root]
  (config! (get-widget root :input) :text "")
  root)

(defn- scaler []
  (let [{[v-low v-high] :x-range-initial
         [low high] :x-range-max} default-frame-values]
    (vertical-panel
     :items
     [(horizontal-panel
       :items
       [(label :text "x-max" :class :font)
        (slider :value v-high
                :min low
                :max high
                :id :x-max
                :class :scale)])
      (horizontal-panel
       :items
       [(label :text "x-min" :class :font)
        (slider :value v-low
                :min low
                :max high
                :id :x-min
                :class :scale)])])))

(defn- reset-scalers [root]
  (let [x-min-scaler (get-widget root :x-min)
        x-max-scaler (get-widget root :x-max)
        [low high] (:x-range-initial default-frame-values)]
    (config! x-min-scaler :value low)
    (config! x-max-scaler :value high)))

(defn- update-frame
  "Updates root based on the current values of
  the stateful atoms."
  [root]
  (do (update-canvas root) ))

(defn- reset-frame!
  "Resets root to the initial state , returns root.
  The stateful atoms (graph and scale) are also reset."
  [root]
  (do (reset-graph!)
      (reset-range!)
      (update-frame root)
      (reset-input root)
      (reset-scalers root)))

;; actions

(defn- parse-input
  "Returns [s fn] if the input is correct or nil."
  [root]
  (let [w (get-widget root :input)
        s (config w :text)
        f (string->fn s)]
    (when f [s f])))

(defn- scale-ok? []
  (let [[low high] @the-range] (< low high)))

(defn- view-action [root]
  (listen (get-widget root :view)
          :mouse-pressed
          (fn [e]
            (when (scale-ok?)
              (if-let [[s f] (parse-input (to-root e))]
                (do
                  (swap! the-graph assoc s f)
                  (update-canvas (to-root e)))
                (alert "incorrect input")))))
  root)

(defn- clear-action [root]
  (listen (get-widget root :clear)
          :mouse-pressed
          (fn [e] (reset-frame! (to-root e))))
  root)

(defn- close-action [root]
  (listen
   (get-widget root :close)
   :mouse-pressed
   (fn [e]
     (let [the-root (to-root e)]
       (reset-frame! the-root)
       (dispose! (to-root the-root)))))
  root)

(defn- scaler-action [root]
  (listen (get-widgets-class root :scale)
          :change
          (fn [e]
            (let [the-root (to-root e)
                  {:keys [x-min x-max]} (value the-root)]
              (reset! the-range [x-min x-max])
              (update-canvas the-root))))
  root)

(defn- reset-action [root]
  (listen (get-widget root :reset)
          :mouse-pressed
          (fn [e] (reset-scalers (to-root e))))
  root)

(defn- add-behaviors [root]
  (-> root view-action clear-action close-action scaler-action reset-action))

(defn- base-frame []
  (let [{:keys [width height]} default-frame-values]
    (frame :width width :height height
           :content
           (border-panel
            :north (vertical-panel
                    :items [(buttons) (input-text)(scaler)])
            :center (canvas :id :chart)))))

(defn- set-font [root n]
  (config!
   (get-widgets-class root :font)
   :font (font :size n))
  root)

(defn- run []
  (-> (base-frame)
      (set-font 30)
      add-behaviors
      show!))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run))
