"
Sets a default URL handler for external links and mailto addresses

Displays the chooser UI to select which application to open it in.
This way if using a separate work or personal browser profile can
choose which browser profile to open a link with

For example opening a url from slack and choosing to open it in
Default or Work browser profile

https://www.hammerspoon.org/docs/hs.urlevent.html#setDefaultHandler
"
(require-macros :lib.macros)
(require-macros :lib.advice.macros)

(local {: contains?
        : filter
        : first
        : join
        : map
        : merge
        : seq} (require :lib.functional))

(fn cmd-str
  [args]
  (->> args
       (join " ")))

(fn quo
  [unwrapped-str]
  (.. "\"" unwrapped-str "\""))

(local applications
       {:brave-browser-work
        {:title "Work"
         :app "Brave Browser"
         :subtext "Brave Browser"
         :schemes ["https" "mailto"]
         :cmd (fn [url]
                ["open" "-a" (quo "Brave Browser.app") "-n"
                 "--args"
                 (.. "--profile-directory=" (quo "Profile 1"))
                 (quo url)])}

        :brave-browser-personal
        {:title "Personal"
         :app "Brave Browser"
         :subtext "Brave Browser"
         :schemes ["https" "mailto"]
         :cmd (fn [url]
                ["open" "-a" (quo "Brave Browser.app") "-n"
                 "--args"
                 (.. "--profile-directory=" (quo "Default"))
                 (quo url)])}})

(defn open-url
      [full-url selected]
      "
      Opens the given url in the selected application
      "
      (when selected
       (let [{:uuid application-id} selected
             application (. applications application-id)]
         ; (print "\n\nSelected application")
         ; (pprint selected)
         (if application
             (let [cmd (cmd-str (application.cmd full-url))]
               ;(print "\nOpening url " full-url " with " cmd)
               (hs.execute cmd))
             (error (.. "Could not open application " application-id))))))

(fn get-app-icon
  [app-name]
  (-?> app-name
       (hs.application.find)
       (: :bundleID)
       (hs.image.imageFromAppBundle)
       ))

(defn url-handler
      [scheme host params full-url sender-pid]
      "
      Handle url events
      "
      (let [chooser (hs.chooser.new #(open-url full-url $1))]
        (chooser:choices (->> applications
                              (map (fn [application id]
                                     (merge application {:id id})))
                              (filter #(contains? scheme $1.schemes))
                              (map (fn [application]
                                     {:uuid application.id
                                      :text (string.format "%s\n%s" application.title application.subtext)
                                      :image (get-app-icon application.app)
                                      }))))
        (chooser:placeholderText full-url)
        (chooser:show)))

;; When hammerspoon is closed reset url handlers to brave

(hs.urlevent.setDefaultHandler "https" "com.brave.Browser")
(hs.urlevent.setDefaultHandler "mailto" "com.brave.Browser")

;; Set the hs.urlevent httpCallback
;;
;; NOTE: This should be the way to do it soon once
;;       https://github.com/Hammerspoon/hammerspoon/pull/3127
;;       is released
;;
;; (tset hs.urlevent :httpCallback url-handler)

;; Tell hammerspoon to set hammerspoon app as the default handler
;; for both https:// http:// and mailto: urls
(hs.urlevent.setDefaultHandler "https")
(hs.urlevent.setDefaultHandler "mailto")

;; Override the default root callback once PR#3127 lands, this
;; will not likely be necessary

(hs.urlevent.setCallback
 (fn [scheme event params full-url senderPID]
  (let [(ok err) (xpcall (fn []
                           (url-handler scheme
                                        event
                                        params
                                        full-url
                                        senderPID))

                         debug.traceback)]
    (when (not ok)
      (hs.showError err)))))

{:applications applications}
