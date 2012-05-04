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
  (:use-macros [tellme.base.fsm-macros :only [dowith defer defer-later stop-defer remote view defdep defreaction defsm set-style set-styles css]]))

; Utils --------------------------------------------------------------------

(defn- text-height [shadow text]
  (dm/set-text! shadow (if (zero? (.-length text)) "." text))
  (ui/property shadow :offsetHeight))

(defn- linkify [text]
    (cond
      (.match text js/url-pattern)
      (.replace text js/url-pattern "<a href='$1' target='_blank'>$1</a>")

      (.match text js/pseudo-url-pattern)
      (.replace text js/pseudo-url-pattern "<a href='http://$2' target='_blank'>$1$2</a>") 

      (.match text js/email-pattern)
      (.replace text js/email-pattern "<a href='mailto:$1' target='_blank'>$1</a>")
      
      :else
      text))

; Constants ----------------------------------------------------------------

(def help-text "What is this?!<br><br>
               In order to start an ad-hoc conversation with some&shy;body, give them the number presented to you; they should do the same. When you both type in your partner’s unique number, your private con&shy;versation will start.<br>
               &nbsp;&nbsp;Quoting something has never been easier; just click on the quote button next to the message you want to cite, select some text and see what hap&shy;pens.<br><br>

               Find out more on the <a href='https://github.com/pkamenarsky/tellme' target='_blank'>github</a> page.")

(def quote-help-text "Select text to split quote.<br>
                     Press Enter to confirm quote.<br>
                     Press Escape to discard quote.")

(def help-width 280)
(def help-margin 80)
(def min-help-width (+ help-width (* help-margin 2)))

(def bottom-padding 50)

; Quotes -------------------------------------------------------------------

(defn- show-quote-button [event]
  (dm/set-style! (.-quoteButton (.-currentTarget event)) :visibility "visible"))

(defn- hide-quote-button [event]
  (dm/set-style! (.-quoteButton (.-currentTarget event)) :visibility "hidden"))

(defn- add-quote [{:keys [table shadow self] :as data} {:keys [quotes slide]}]
  (let [[lq lr] (last quotes)
        adj-quotes (if (zero? (.-length lr)) (butlast quotes) quotes)

        [height rows]
        (reduce (fn [[height rows] [q r]]
                  (let [qh (- (text-height shadow q) 10)
                        rh (- (text-height shadow r) 10)]

                    ; FIXME: 20
                    [(+ height qh rh (if (zero? rh) 20 20))
                     (-> rows
                       (conj (view :div.quote-row nil {:html (linkify q) :style.height [qh :px]}))
                       (conj (view :div.retort-row nil {:html (linkify r) :style.height [rh :px]})))]))
                [0 []] adj-quotes)]

    (if-not (empty? rows)
      (let [row (table/add-row table)]

        (table/set-row-contents
          table row
          (view :div.message rows {:style.height [height :px]}))

        (if slide
          (do (ui/animate [table row [height :px] :duration 200
                           :onend #(swap! self fsm/send-message :go-to-dispatch)])
            :locked)
          (do (ui/animate [table row [height :px] :duration 0])
            :dispatch)))
      :dispatch)))

(defn- quote-message [{:keys [input main-container quote-overlay table self uuid sid] :as sm}
                      {:keys [text row height]} event]

  (when (not= (fsm/state @self) :end)
    (let [event-key (atom nil)
          static-table (dm/clone table)
          client-height (.-clientHeight (.-body (dom/getDocument)))

          button-background (view :div.button-background-up)
          button-text (view :div.button-text-up)
          button-container (view :div.button-container-up [button-background button-text])

          help (view :div.quote-help nil {:html quote-help-text})
          help-container (view :div.quote-help-container [help button-container])

          qt (dm/add-class!
               (qt/create-quote text (fn [qt]
                                       (events/unlistenByKey @event-key) 
                                       (swap! self fsm/send-message :go-to-dispatch)

                                       (when-let [quotes (qt/get-quotes qt)]
                                         ; if we actually got some quotes, send message to fsm &
                                         ; set up chat table to match up with quotes (for cool slide-down
                                         ; animation)
                                         (swap! self fsm/send-message {:site :local
                                                                       :slide false
                                                                       :quotes quotes}) 

                                         (if (zero? (.-length (second (last quotes))))
                                           ; if last and only retor is empty, do nothing
                                           (when (> (count quotes) 1)
                                             (dm/set-style! table :bottom (+ 56 (* client-height 0.3)) "px"))
                                           (dm/set-style! table :bottom (* client-height 0.3) "px")) 
                                         (ui/resized table))

                                       (dm/set-style! main-container :visibility "visible") 
                                       (ui/animate [table :scroll-bottom 0 :duration 0] 
                                                   [main-container :style.opacity 1
                                                    :onend #(do
                                                              (dm/detach! qt)
                                                              (dm/detach! help-container)
                                                              (ui/select input)
                                                              (ui/animate [table :style.bottom [(+ 28 bottom-padding) :px]]))]
                                                   [quote-overlay :style.opacity 0
                                                    :onend #(dm/set-style! quote-overlay :visibility "hidden")])))
               "quote-table")

          bottom (+ height (- (table/row-top table row) (table/scroll-top table)))
          scroll-top (table/scroll-top table)]

      (swap! self fsm/goto :locked)

      ; first hide button
      (dm/set-style! (.-target event) :visibility "hidden")

      ; we append a static copy here so that we don't have to worry about
      ; any currently running animations
      (dm/detach! table)
      (dm/append! main-container static-table)
      (dm/append! quote-overlay help-container)

      (ui/set-property! (first (dm/children static-table)) :scrollTop scroll-top)

      (dm/set-style! qt :bottom (- client-height (+ bottom 28)) "px")
      (dm/set-style! quote-overlay :visibility "visible") 

      ; fade out main and show quote overlay
      (ui/animate [main-container :style.opacity 0
                   :onend #(do
                             (dm/append! main-container table)
                             (dm/detach! static-table)
                             (dm/set-style! main-container :visibility "hidden"))]
                  [quote-overlay :style.opacity 1
                   :onend #(ui/animate [qt :style.bottom [(* client-height 0.3) :px]])])

      ; help button
      (dme/listen! button-container :click
                   (fn [event]
                     (ui/animate [button-container :style.opacity 0 :duration 300]
                                 [button-background :transform.rotate [-180 :deg] :duration 300]
                                 [help-container :style.bottom [-350 :px]])))

      (dm/append! quote-overlay qt)
      (ui/resized qt)

      (reset! event-key (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event]
                                                                                   (ui/resized qt)))))))

(defn set-message-at-row [{:keys [table] :as data} row
                          {:keys [text site height] :as message}]

  (let [msg-container (view :div.message nil {:html (linkify text)})
        quote-button (view :div.quote-button)
        
        container (view :div.fill-width [msg-container quote-button] {:style.height [height :px]})]

    (if (= site :local)
      (dm/add-class! msg-container "local-message")
      (dm/add-class! msg-container "remote-message"))

    (set! (.-quoteButton (dm/single-node container)) quote-button)
    (table/set-row-contents table row container)
    
    ; events
    (dowith dme/listen!
            (container :mouseover show-quote-button) 
            (container :mouseout hide-quote-button) 
            (quote-button :click (partial quote-message data message)))))

; UI -----------------------------------------------------------------------

(defn- start [{:keys [self] :as data}]
  (let [number1 (view :div.number1)
        number2 (view :input.number2 nil {:attr.maxlength 4})

        button-background (view :div.button-background)
        button-text (view :div.button-text)
        button-container (view :div.button-container [button-background button-text])

        help (view :div.help nil {:html help-text})

        label1 (view :div.label1 nil {:text "tell’em"})
        label2 (view :div.label2 nil {:text "tell me"})

        left-column (view :div.left-column [button-container help])
        right-column (view :div.right-column [label1 label2 (view :div.divider) number1 number2])

        main-container (view :div.main)
        quote-overlay (view :div.quote-overlay)

        _ (view (dmc/sel "body") [left-column right-column main-container quote-overlay])
        
        input-defer (atom nil)]

    (dme/listen! button-container :click
                 ; we want to distribute the margins between page border,
                 ; help column and content evenly here
                 (fn [e] 
                   (let [left (ui/property right-column :offsetLeft)
                         new-left (Math/max left min-help-width)
                         margin (/ (- new-left help-width) 2)]

                     (dm/set-style! right-column :left left "px") 

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

    (ui/select number2)

    ; request sid / uuid
    (remote [:command :get-uuid]
            {:keys [uuid sid] :as message}

            (dm/set-text! number1 sid)

            ; store credentials and setup backchannel; all server messages are going to be
            ; forwarded to this fsm
            (swap! self fsm/merge-data
                   {:sid sid
                    :uuid uuid
                    :backchannel (comet/backchannel
                                   [:uuid uuid :sid sid]
                                   (fn [{:keys [ack] :as message}]
                                     (when ack
                                       (swap! self fsm/send-message (assoc message :site :remote)))))}) 

            ; handle auth
            (events/listen (events/KeyHandler. (dm/single-node number2)) "key"
                           (fn [event]
                             (let [code (.-keyCode event)]
                               (if (or (and (>= code 48) (<= code 57)) (= code keycodes/BACKSPACE))
                                 ; throttling should be done with defdep/defreaction in a FRP style,
                                 ; but since those macros are pretty limited anyway (glitches), we do
                                 ; it the old fashioned way here
                                 (do
                                   (stop-defer @input-defer)
                                   (reset! input-defer
                                           (defer-later 500
                                                        (when-not (js/isNaN (js/parseInt (dm/value number2)))
                                                          (remote [:command :auth :uuid uuid :sid sid :osid
                                                                   (js/parseInt (dm/value number2))] _))))) 
                                 (.preventDefault event))))))

    ; update our internal state
    (-> data
      (assoc :left-column left-column)
      (assoc :right-column right-column)
      (assoc :osid-input number2)
      (assoc :main-container main-container)
      (assoc :quote-overlay quote-overlay))))

(defn- show-chat [{:keys [main-container quote-overlay self] :as data}]
  (let [table (dm/add-class! (table/create-table) "chat-table") 
        shadow (view :div.shadow)
        input (view :textarea.chat-input)

        main-container (view main-container [table shadow input])

        body (view (dmc/sel "body") [main-container quote-overlay])

        message (atom nil)
        shadow-height (defdep [message]
                              (dm/set-text! shadow (if (> (.-length message) 0) message "."))
                              (ui/property shadow :offsetHeight)) 
        input-height (atom -1)]

    ; dependencies
    (defreaction shadow-height (ui/animate [input-height shadow-height]))
    (defreaction input-height
                 (dm/set-style! input :height input-height "px")
                 (dm/set-style! table :bottom (+ bottom-padding input-height) "px")
                 (ui/resized table))

    ; dom
    (ui/resized table)

    ; events
    (dme/listen! input :input (fn [event] (reset! message (dm/value input))))
    (events/listen (dom/ViewportSizeMonitor.) evttype/RESIZE (fn [event] (ui/resized table)))
    (events/listen (events/KeyHandler. (dm/single-node input)) "key"
                   (fn [event]
                     (when (= (.-keyCode event) keycodes/ENTER)
                       (when (> (.-length (dm/value input)) 0)
                         (swap! self fsm/send-message {:site :local
                                                       :slide true
                                                       :text (dm/value input)})
                         (dm/set-value! input "")
                         (reset! message "")) 
                       (.preventDefault event))))  

    (ui/select input)
    (reset! message "")

    (-> data
      (assoc :shadow shadow)
      (assoc :input input)
      (assoc :table table)
      (assoc :main-container main-container))))

(defn- add-message [{:keys [slide text site] :as message}
                    {:keys [shadow table queue main-container self] :as data}]
  (if slide
    ; received a new local message
    (if (table/at? table :bottom)
      (let [height (text-height shadow text)
            row (table/add-row table)
            overlay (view :div.message-overlay nil {:text text})]

        (if (= site :local)
          (dm/add-class! overlay "local-message")
          (dm/add-class! overlay "remote-message"))

        (dm/append! main-container overlay) 
        (ui/animate [table row [height :px]]
                    [overlay :style.bottom [(+ 31 bottom-padding) :px] 
                     :onend (fn []
                              (set-message-at-row data row (into message {:row row
                                                                          :height height}))
                              (dm/detach! overlay)
                              (swap! self fsm/send-message :go-to-dispatch))])
        ; lock sliding
        :locked) 

      ; else (if table/at? table bottom)
      (let [height (text-height shadow text)
            row (table/add-row table)]
        (ui/animate [table :scroll-bottom [0 :px]]
                    [table row [height :px]])
        (set-message-at-row data row (into message {:row row
                                                    :height height}))
        :dispatch)) 

    ; else (if slide)
    (let [height (text-height shadow text)
          row (table/add-row table)]
      (ui/animate [table row [height :px]])
      (set-message-at-row data row (into message {:row row
                                                  :height height}))
      :dispatch)))

; FSM ----------------------------------------------------------------------

(def base-sm
  (defsm
    ; fsm data
    {:queue []}

    ; we start here
    ([:start :in data]
     (fsm/next-state :auth (start data)))

    ([:auth message data]
     (if (= message {:ack :auth :site :remote})
       (fsm/next-state :show-chat)
       (fsm/ignore-msg)))

    ; show chat & go directly to dispatch state
    ([:show-chat :in {:keys [left-column right-column main-container] :as data}]
     (ui/animate [left-column :style.top [-100 :pct]]
                 [right-column :style.top [-100 :pct]]
                 [main-container :style.top [0 :pct]])

     (fsm/next-state :dispatch (show-chat data)))

    ; dispatch local and remote messages
    ([:dispatch {:keys [site ack] :as message} {:keys [sid uuid] :as data}]
     (cond
       (= ack :close) (fsm/next-state :end)

       (and (= site :remote)
            (= ack :message)) (fsm/next-state :ready data {:site :remote
                                                           :slide false
                                                           :text (:message message)})
       (and (= site :remote)
            (= ack :quote)) (fsm/next-state (add-quote data (if (= false (:slide message))
                                                              ; if false has been set in :locked,
                                                              ; don't set to true... this is a hack
                                                              message
                                                              (assoc message :slide true))))

       (and (= site :local)
            (:quotes message)) (do
                                 (remote [:command :quote :uuid uuid :sid sid :quotes (:quotes message)] _)
                                 (fsm/next-state (add-quote data message)))

       (= site :local) (do
                         ; send message to remote site
                         (remote [:command :message :uuid uuid :sid sid :message (:text message)] _)
                         (fsm/next-state :ready data message))

       :else (fsm/ignore-msg)))

    ; slide locked state, just queue up messages
    ([:locked message data]
     (fsm/next-state :locked (update-in data [:queue] conj (assoc message :slide false))))

    ([:locked :go-to-dispatch {:keys [queue] :as data}]
     ; when going out of :locked state, send all queued messages to self
     (comp (fn [sm] (reduce (fn [a msg] (fsm/send-message a msg)) sm queue))
           (fsm/next-state :dispatch (assoc data :queue []))))

    ; ready for displaying new messages
    ([:ready :go-to-dispatch _]
     (fsm/ignore-msg))

    ([:ready message data]
     (fsm/next-state (add-message message data)))
    
    ([:end :in {:keys [input backchannel] :as data}]
     (comet/stop backchannel)
     (add-message {:slide false
                   :local true
                   :text "Disconnected from chat."} data)
     (dm/set-attr! input :disabled "disabled")
     (.blur (dm/single-node input)))
    
    ([:end _ _]
     (ignore-msg)))) 

; Initialization -----------------------------------------------------------

(def sm (atom base-sm)) 

(comet/set-comet-error-callback! (fn [] (swap! sm fsm/goto :end)))

(events/listen js/window evttype/LOAD #(defer
                                         (reset! sm (-> @sm
                                                      (fsm/merge-data {:self sm})
                                                      (fsm/goto :start))) 
                                         0))

