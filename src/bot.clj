(import '(java.io InputStreamReader PrintWriter BufferedReader))
(import '(java.net URL Socket))


(defstruct ircbot :nick :server :input :output :channels)
(defstruct private-command :nick :command)
(defstruct public-command :nick :command :channel)

(def urls (ref ()))
(def bot (atom {}))
(defn end-of-motd? [line] (re-matches #".* 376 .*" line))
(defn ping? [line] (re-matches #"PING .*" line))
(def command-char \,)

(def example-private-message ":ircdude!username@dynamic-ip.isp.com PRIVMSG iStutter :np ircdude")
(def example-channel-message ":ircdude!username@dynamic-ip.isp.com PRIVMSG #mychannel :,(join (channel #myotherchannel))")

(defn match-private-message [line]
  (let [[_ sender-nick command] (re-find (re-pattern (str ":([^!]+).+? PRIVMSG " (:nick @bot) " :(.*)")) line)]
    (if (some nil? [sender-nick, command])
      false
      (struct private-command sender-nick command))))

(defn match-channel-message [line]
  (let [[_ sender-nick channel command] (re-find (re-pattern (str ":([^!]+).+? PRIVMSG (#[^ ]+) :" command-char "(.*)")) line)]
    (if (some nil? [sender-nick, channel, command])
      false
      (struct public-command sender-nick command channel))))


(defn match-part-request [message] (not (nil? (re-find #".*\(leave\).*" message))))
(defn match-join-request [message]
  (let [[_ channel] (re-find #".*\(join \(channel (#[^ ]+)\)\).*" message)]
    (if (nil? channel)
      false
      channel)))


(comment
  (defn match-spotify-uri [message] (re-find #".*spotify:(track|album|artist|user):([\w:]+).*" message))
  (defn match-spotify-url [message] (re-find #".*http://open.spotify.com/(track|album|artist|user)/([\w/]+).*" message))
  )


(defn write-to-server [line]
  (let [output (:output @bot)]
    (if (nil? output)
      (println (str "output is nil! " @bot))
      (do
        (println "-> " line)
        (try
          (.println output line)
          (.flush output)
          (catch NullPointerException e (println "Caught nullpointer"))
          )))))

(defn send-nick []
  (write-to-server (str "NICK " (:nick @bot))))

(defn send-user []
  (write-to-server (str "USER stutter stutter " (:server @bot) " :Stutter Bot")))

(defn join-channel [channel] (write-to-server (str "JOIN " channel)))
(defn part-channel [channel] (write-to-server (str "PART " channel)))



(defn respond-to [target message]
  (println "respond-to " target " ; " message)
  (write-to-server (str "PRIVMSG " target " :" message)))


(defn react-to-privately [command]
  (let [channel-to-be-joined (match-join-request (:command command))]
    (cond
      (not (false? channel-to-be-joined)) (join-channel channel-to-be-joined)
      )))

(defn react-to-publicly [command]
  (let [
    channel-to-be-joined (match-join-request (:command command))
    part-requested (match-part-request (:command command))
    ]
    (cond
      (not (false? channel-to-be-joined)) (join-channel channel-to-be-joined)
      (true? part-requested) (part-channel (:channel command))
      )))

(defn join-startup-channels "force traversing channel list and mapping" [] (dorun (map join-channel (:channels @bot))))

(defn react-to-message [line]
  (let [
    privmsg (match-private-message line)
    chanmsg (match-channel-message line)
    ]
    (do (println "<- " line)
      (cond
        (ping? line) (write-to-server (str "PONG " (:server @bot)))
        (end-of-motd? line) (join-startup-channels)
        (not (false? privmsg)) (react-to-privately privmsg)
        (not (false? chanmsg)) (react-to-publicly chanmsg)
        ))))


(defn react-to-messages []
  (let [line (.readLine (:input @bot))]
    (if (nil? line)
      (println "react-to-messages line is nil")
      (do
        (react-to-message line)
        ))
    (recur)))

(defn start-bot [botnick, #^String server, #^Integer port, channels]
  (println (str "Trying to create a bot with nick=" botnick "; server=" server "; port=" port "; channels=" channels))
  (let [socket (Socket. server port)
        input (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        output (PrintWriter. (.getOutputStream socket))
        chan-seq (re-seq #"#[^ ]+" channels)]
    (if (nil? socket)
      (println "Socket is not up?")
      (do
        (swap! bot assoc :nick botnick :server server :input input :output output :channels chan-seq)
        (send-nick)
        (send-user)

        (try
          (react-to-messages)
          (catch NullPointerException e ((println "Caught nullpointer: " (.getMessage e))))
          (finally (.close socket))
          )))))

(start-bot "iStutter" "irc.inet.fi" 6667 "#paskamaja #paskamaja2")


 