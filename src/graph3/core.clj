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
  {:step 100
   :width 700
   :height 700
   :image-width 500
   :image-height 500
   :stroke 1
   :font-size 25
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

;;;;;;;;;;;;;;
;; make ui  ;;
;;;;;;;;;;;;;;

(def the-graph (atom {}))

(defn- reset-graph! [] (reset! the-graph {}))

(defn- graph-registered? [] (seq @the-graph))

(defn- get-chart-image
  "Returns chart as BufferedImage by obtaining value
  from the atom registering the functions to plot."
  [root]
  (when (graph-registered?)
    (let [m @the-graph
          {:keys [center range]} (value root)
          low (- center range)
          high (+ center range)]
      (fns->chart-image m low high))))

(defn- update-canvas [root]
  (let   [im (get-chart-image root)]
    (config!
     (get-widget root :chart)
     :paint
     (image-shower im))))

(defn- buttons []
  (horizontal-panel
   :items  (->> [:view :reset :clear :close]
                (map (fn [k]
                       (button :text (name k) :class :font :id k))))))

(def ^:private slider-constants
  {:center-init 0
   :center-min -100
   :center-max 100
   :range-init 10
   :range-max 100
   :range-min 0.1})

(defn- make-slider [s id value low high ]
  (horizontal-panel
   :items [(label :text s :class :font)
           (slider :value value :id id :min low :max high )]))

(defn- sliders []
  (let [{center :center-init
         c-min :center-min
         c-max :center-max
         ran :range-init
         r-min :range-min
         r-max :range-max} slider-constants]
    (vertical-panel
     :items [(make-slider "center" :center center c-min c-max)
             (make-slider "range" :range ran r-min r-max)])))

(defn- reset-sliders [root]
  (let [center-slider (get-widget root :center)
        range-slider (get-widget root :range)
        {:keys [center-init range-init]} slider-constants]
    (config! center-slider :value center-init)
    (config! range-slider :value range-init)
    (update-canvas root)))

(defn- reset-all [root]
  (reset-sliders root)
  (reset-graph!)
  (update-canvas root))

(defn- input-text []
  (horizontal-panel
   :items [(label :text "f(x)=" :class :font)
           (text :editable? true :id :input :class :font)]))

(defn- parse-input
  "Gets function from the string takes from input text.
  Returns nil on parse error."
  [root]
  (let [s (config (get-widget root :input) :text)
        f (string->fn s)]
    (when f [s f])))

(defn- reset-input [root]
  (config! (get-widget root :input) :text "")
  root)

(defn- buttons []
  (horizontal-panel :items
                    (->> [:view :reset :clear :close]
                         (map #(button :text (name %)
                                       :class :font
                                       :id %)))))

(defn- button-action
  "Returns fn which installs a button action."
  [id type-key f]
  (fn [root]
    (listen (get-widget root id)
            type-key
            f)
    root))

(defn- slider-action
  [id f]
  (fn [root]
    (listen (get-widget root id)
            :change
            f)
    root))

(def view-action
  (button-action
   :view
   :mouse-pressed
   (fn [e]
     (let [r (to-root e)]
       (if-let [[s f] (parse-input r)]
         (do
           (swap! the-graph assoc s f)
           (update-canvas r))
         (alert "incorrect input"))))))

(def close-action
  (button-action
   :close
   :mouse-pressed
   (fn [e]
     (let [r (to-root e)]
       (reset-all r)
       (dispose! r)))))

(def reset-action
  (button-action
   :reset
   :mouse-pressed
   (fn [e] (reset-sliders (to-root e)))))

(def clear-action
  (button-action
   :clear
   :mouse-pressed
   (fn [e] (let [r (to-root e)]
             (reset-input r)
             (reset-all r)))))

(def center-slider-action
  (slider-action
   :center
   (fn [e] (update-canvas (to-root e)))))

(def range-slider-action
  (slider-action
   :range
   (fn [e] (update-canvas (to-root e)))))

(defn- add-behaviors [root]
  (-> root
      close-action
      reset-action
      view-action
      clear-action
      center-slider-action
      range-slider-action))

(defn- set-font [root n]
  (config! (get-widgets-class root :font) :font (font :size n))
  root)

(defn- main-frame []
  (let [{:keys [width height font-size]} default-frame-values]
    (-> (frame :width width
               :height
               height
               :content (border-panel
                         :north
                         (vertical-panel
                          :items [(buttons) (input-text) (sliders)])
                         :center (canvas :id :chart)))
        (set-font font-size))))

(defn- run []
  (-> (main-frame) add-behaviors show!))

(defn -main [& args] (run))



