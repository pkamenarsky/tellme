(ns tellme.core
  (:require [goog.dom :as dom]
            [goog.dom.ViewportSizeMonitor :as viewport]
            [goog.userAgent :as useragent]
            [goog.events.KeyHandler :as keyhandler]
            [goog.events.KeyCodes :as keycodes]
            [goog.events.EventType :as evttype]
            [goog.events :as events]

            [domina :as dm]
            [domina.css :as dmc]
            [domina.events :as dme]

            [tellme.base.fsm :as fsm]
            [tellme.comet :as comet]
            [tellme.ui :as ui]
            [tellme.table :as table]
            [tellme.quote :as qt])
  (:use [tellme.base.fsm :only [fsm stateresult data state next-state ignore-msg send-message goto]])
  (:use-macros [tellme.base.fsm-macros :only [view defdep defreaction defsm set-style set-styles css]]))

; Utils --------------------------------------------------------------------

(defn- text-height [shadow text]
  (dm/set-text! shadow text)
  (ui/property shadow :offsetHeight))

(defn- show-quote-button [event]
  (dm/set-style! (.-quoteButton (.-currentTarget event)) :visibility "visible"))

(defn- hide-quote-button [event]
  (dm/set-style! (.-quoteButton (.-currentTarget event)) :visibility "hidden"))

; Constants ----------------------------------------------------------------

(def help-text "What is this?!<br><br>
               In order to start an ad-hoc conversation with some&shy;body, give them the number presented to you; they should do the same. When you both type in your partner’s unique number, your private con&shy;versation will start.<br>
               &nbsp;&nbsp;Quoting something has never been easier; just click on the quote button next to the message you want to cite, select some text and see what hap&shy;pens.<br><br>

               Find out more on the github page.")

(def help-width 280)
(def help-margin 80)
(def min-help-width (+ help-width (* help-margin 2)))

(def bottom-padding 50)

; Quotes -------------------------------------------------------------------

(defn- add-quote [{:keys [table shadow]} quotable]
  (let [quotes (qt/get-quotes quotable)
        msg-container (view :div.message)
        row (table/add-row table)]
    (loop [pair quotes
           height 0]
      (if pair
        (let [[q r] (first pair)
              qv (view :div.quote-row)
              rv (view :div.retort-row)

              qh (- (text-height shadow q) 10)
              rh (if (zero? (.-length rv)) 0 (- (text-height shadow r) 10))]

          (-> msg-container (dm/append! qv) (dm/append! rv))

          (dm/set-text! qv q)
          (dm/set-text! rv r)
          (dm/set-style! qv :height qh "px")
          (dm/set-style! rv :height rh "px")

          ; FIXME: 20
          (recur (next pair) (+ (if (zero? rh) 10 20) height qh rh)))
        (do
          (table/set-row-contents table row msg-container)

          (ui/animate [table row [height :px] :duration 0])
          (dm/set-style! msg-container :height height "px"))))))

(defn- quote-message [{:keys [input main-container quote-overlay table] :as sm}
                      {:keys [text row height self]} event]

  (let [event-key (atom nil)
        static-table (dm/clone table)
        client-height (.-clientHeight (.-body (dom/getDocument)))
        qt (dm/add-class! (qt/create-quote text (fn [qt]
                                                  (events/unlistenByKey @event-key) 

                                                  (swap! self fsm/send-message :go-to-ready)
                                                  (add-quote sm qt)

                                                  (dm/set-style! main-container :visibility "visible")
                                                  (dm/set-style! table :bottom (* client-height 0.3) "px")
                                                  (ui/resized table)

                                                  (ui/animate [table :scroll-bottom 0 :duration 0] 
                                                              [main-container :style.opacity 1
                                                               :onend #(do
                                                                         (dm/detach! qt)
                                                                         (ui/select input)
                                                                         (ui/animate [table :style.bottom [(+ 31 bottom-padding) :px]]))]
                                                              [quote-overlay :style.opacity 0
                                                               :onend #(dm/set-style! quote-overlay :visibility "hidden")])))
                          "chat-table")
        bottom (+ height (- (table/row-top table row) (table/scroll-top table)))
        scroll-top (table/scroll-top table)]

    (swap! self fsm/goto :locked)

    ; first hide button
    (dm/set-style! (.-target event) :visibility "hidden")

    ; we append a static copy here so that we don't have to worry about
    ; any currently running animations
    (dm/detach! table)
    (dm/append! main-container static-table)

    (ui/set-property! (first (dm/children static-table)) :scrollTop scroll-top)

    (dm/set-style! qt :bottom (- client-height (+ bottom 28)) "px")
    (dm/set-style! main-container :opacity 1)
    (dm/set-styles! quote-overlay {:visibility "visible"
                                   :opacity "0"}) 

    ; fade out main and show quote overlay
    (ui/animate [main-container :style.opacity 0
                 :onend #(do
                           (dm/append! main-container table)
                           (dm/detach! static-table)
                           (dm/set-style! main-container :visibility "hidden"))]
                [quote-overlay :style.opacity 1
                 :onend #(ui/animate [qt :style.bottom [(* client-height 0.3) :px]])])

    (dm/append! quote-overlay qt)
    (ui/resized qt)
    
    (reset! event-key (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event]
                                                                                 (ui/resized qt))))))

(defn set-message-at-row [{:keys [table] :as data} row
                          {:keys [text site height] :as message}]

  (let [msg-container (view :div.message)
        quote-button (view :div.quote-button)
        
        container (view :div.fill-width msg-container quote-button)]

    (dm/set-style! container :height height "px")
    (set! (.-quoteButton (dm/single-node container)) quote-button)

    (dm/set-text! msg-container text)
    (table/set-row-contents table row container)
    
    ; events
    (dme/listen! container :mouseover show-quote-button)
    (dme/listen! container :mouseout hide-quote-button)
    (dme/listen! quote-button :click (partial quote-message data message))))

; FSM ----------------------------------------------------------------------

(def base-sm
  (defsm
    ; fsm data
    {:queue []
     :table nil
     :input nil
     :main-container nil
     :quote-overlay nil
     :shadow nil}

    ; slide locked state, just queue up messages
    ([:locked message data]
     ;(dm/log-debug (str ":locked message: " (:text message)))
     (next-state :locked (update-in data [:queue] conj (assoc message :slide false))))

    ([:locked :go-to-ready {:keys [queue] :as data}]
     ;(dm/log-debug (str ":locked go-to-ready"))
     ; when going out of :locked state, send all queued messages to self
     (comp (fn [sm] (reduce (fn [a msg] (fsm/send-message a msg)) sm queue))
           (fsm/next-state :ready (assoc data :queue []))))

    ([:ready :go-to-ready _]
     (fsm/ignore-msg))

    ; ready for displaying new messages
    ([:ready {:keys [slide text self] :as message}
      {:keys [shadow table queue main-container] :as data}]

     ;(dm/log-debug (str ":ready message: " text))

     (if slide
       ; received a new local message
       (do
         (if (table/at? table :bottom)
           (let [height (text-height shadow text)
                 row (table/add-row table)
                 overlay (view :div.message-overlay)]

             (dm/set-text! overlay text) 
             (dm/append! main-container overlay) 
             (dm/set-style! overlay :bottom bottom-padding "px")

             (ui/animate [table row [height :px]]
                         [overlay :style.bottom [(+ 31 bottom-padding) :px] 
                          :onend (fn []
                                   ;(dm/log-debug (str "slide done: " text))
                                   (set-message-at-row data row (into message {:row row
                                                                               :height height}))
                                   (dm/detach! overlay)
                                   (swap! self fsm/send-message :go-to-ready))])) 

           ; else (if table/at? table bottom)
           (do 
             ;(dm/log-debug (str "starting scroll: " text))
             (ui/animate [table :scroll-bottom [0 :px]
                       ; after sliding, return to :ready and add the message
                       ; we wanted to add in the first place (but had to scroll
                       ; down before doing so)
                       :duration 0
                       :onend (fn []
                                ;(dm/log-debug (str "scroll done: " text))
                                (reset! self (-> @self
                                               (fsm/send-message :go-to-ready)
                                               (fsm/send-message message))))]))) 
         ; lock sliding
         (fsm/next-state :locked)) 

       ; else (if slide)
       (let [height (text-height shadow text)
             row (table/add-row table)]
         (ui/animate [table row [height :px]])
         (set-message-at-row data row (into message {:row row
                                                     :height height}))
         (fsm/ignore-msg)))))) 

; main ---------------------------------------------------------------------

(defn begin []
  (let [number1 (view :div.number1)
        number2 (view :input.number2)

        button-background (view :div.button-background)
        button-text (view :div.button-text)
        button-container (view :div.button-container button-background button-text)

        help (view :div.help)

        label1 (view :div.label1)
        label2 (view :div.label2)

        left-column (view :div.left-column button-container help)
        right-column (view :div.right-column label1 label2
                           (view :div.divider) number1 number2)

        main-container (view :div.main)
        quote-overlay (view :div.quote-overlay)

        _ (view (dmc/sel "body") left-column right-column main-container quote-overlay)]

    (dm/set-style! button-text :right 18 "px")
    (dm/set-style! button-container :right 22 "px")
    (dm/set-style! button-container :opacity 1)
    (dm/set-style! left-column :width 100 "px")
    (dm/set-style! main-container :top 100 "%")

    (dme/listen! button-container :click (fn [e] 
                                           (let [left (ui/property right-column :offsetLeft)
                                                 new-left (Math/max left min-help-width)
                                                 margin (/ (- new-left help-width) 2)]
                                             (dm/set-style! right-column :left left "px") 
                                             (dm/set-style! help :right 100 "px") 
                                             (ui/animate 
                                               [right-column :style.left [new-left :px]]
                                               [left-column :style.width [new-left :px]]
                                               [help :style.right [margin :px]]
                                               [button-container :style.right [(- (/ margin 2) 11) :px]]
                                               [button-background :transform.rotate [180 :deg] :duration 300]
                                               [button-container :style.opacity 0 :duration 500
                                                :onend (fn []
                                                         (ui/select number2)
                                                         (dm/set-style! button-container :visibility "hidden"))]
                                               [button-text :style.right [6 :px]]))))

    (dm/set-text! label1 "tell’em")
    (dm/set-text! label2 "tell me")

    (dm/set-html! help help-text)

    (dm/set-text! number1 "4729")
    (dm/set-attr! number2 :maxlength 4)
    
    (ui/select number2)
    (events/listen (events/KeyHandler. (dm/single-node number2)) "key"
                   (fn [event]
                     (when (= (.-keyCode event) keycodes/ENTER)
                       (ui/animate [left-column :style.top [-100 :pct]]
                                   [right-column :style.top [-100 :pct]]
                                   [main-container :style.top [0 :pct]])
                       (main3 main-container quote-overlay))))
    ))

(defn main3 [main-container quote-overlay]
  (let [table (dm/add-class! (table/create-table) "chat-table") 
        shadow (view :div.shadow)
        input (view :textarea.chat-input)

        main-container (view main-container table shadow input)

        body (view (dmc/sel "body") main-container quote-overlay)

        sm (atom (fsm/with-data base-sm {:queue []
                                         :shadow shadow
                                         :input input
                                         :table table
                                         :main-container main-container
                                         :quote-overlay quote-overlay})) 
        
        message (atom nil)
        shadow-height (defdep [message]
                              (dm/set-text! shadow (if (> (.-length message) 0) message "."))
                              (ui/property shadow :offsetHeight)) 
        input-height (atom -1)]
    
    ; dependencies
    (defreaction shadow-height (ui/animate [input-height shadow-height]))

    ;(ui/bind input-height input :style.height "px")
    ;(ui/bind input-height table :style.bottom "px")
    ;(defreaction input-height (ui/resized table))

    (defreaction input-height
                 (dm/set-style! input :height input-height "px")
                 (dm/set-style! table :bottom (+ bottom-padding input-height) "px")
                 (ui/resized table))

    ; dom
    (ui/resized table)

    ; events
    (dme/listen! input :input (fn [event]
                                (reset! message (dm/value input))))

    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event]
                                                               (ui/resized table)))

    (events/listen (events/KeyHandler. (dm/single-node input))
                   "key"
                   (fn [event]
                     (when (= (.-keyCode event) keycodes/ENTER)
                       (when (> (.-length (dm/value input)) 0)
                         (swap! sm fsm/send-message {:self sm
                                                     :slide true
                                                     :text (dm/value input)})

                         (dm/set-value! input "")
                         (reset! message "")) 
                       (.preventDefault event))))  

    ; init
    (swap! sm fsm/goto :ready)
    (reset! message "")
    (ui/select input)
    
    ; test
    ;(js/setInterval #(swap! sm fsm/send-message {:text "asdasd" :slide false :self sm}) 1000)
    ))

(defn test-comet []
  (comet/channel {:command :get-uuid}
                 (fn [response]
                   (dm/log-debug (str "RPN: " (pr-str response))))))

(defn test-comet2 []
  (comet/channel [:command :get-uuid]
                 (fn [response]
                   (let [{uuid "uuid" sid "sid"} response]
                     (comet/channel [:command :get-uuid]
                                    (fn [response]
                                      (let [{uuid2 "uuid" sid2 "sid"} response]
                                        (dm/log-debug (str uuid2 ", " sid2))
                                        
                                        (comet/channel [:command :auth
                                                        :uuid uuid
                                                        :sid sid
                                                        :osid sid2]
                                                       (fn [r]
                                                         (comet/channel [:command :auth
                                                                         :uuid uuid2
                                                                         :sid sid2
                                                                         :osid sid] 
                                                                        (fn [response]
                                                                          (comet/backchannel [:uuid uuid :sid sid] (fn [msg] (dm/log-debug msg)) nil) 
                                                                          (comet/backchannel [:uuid uuid2 :sid sid2] (fn [msg] (dm/log-debug msg)) nil))))) 
                                        )))
                     (dm/log-debug (str uuid ", " sid))))))
 
;(events/listen js/window evttype/LOAD begin)
;(events/listen js/window evttype/LOAD main3)
(events/listen js/window evttype/LOAD test-comet2)

