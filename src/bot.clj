(import '(java.io InputStreamReader PrintWriter BufferedReader))
(import '(java.net URL Socket))


(defstruct ircbot :nick :server :input :output :channels)
(defstruct message :text :output)

(def urls (ref ()))
(def bot (ref ircbot))
(defn end-of-motd? [line] (re-matches #".* 376 .*" line))
(defn ping? [line] (re-matches #"PING .*" line))
(defn match-private-message [line] (re-find (re-pattern (str ":([^!]+).+? PRIVMSG " (:nick @bot) " :(.*)")) line))
(defn match-channel-message [line] (re-find #".+? PRIVMSG (#[^ ]+) :(.*)" line))
(defn match-spotify-uri [message] (re-find #".*spotify:(track|album|artist|user):([\w:]+).*" message))
(defn match-spotify-url [message] (re-find #".*http://open.spotify.com/(track|album|artist|user)/([\w/]+).*" message))
(defn match-part-request [message] (re-find #".*GTFO NIGGA.*" message))
(defn match-join-request [message] (re-find #".*YO JOIN (#[^ ]+) KTHXBAI.*" message))


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
(defn join-channel [channel]
  (println "join-channel [" channel "]")
  (write-to-server (str "JOIN " channel)))
(defn part [channel]
  (write-to-server (str "PART " channel)))



(defn respond-to [target message]
  (println "respond-to " target " ; " message)
  (write-to-server (str "PRIVMSG " target " :" message)))
(defn resolve-spotify-uri-info [spotify-uri origin]
  (println "resolve-spotify-uri-info")
  (respond-to origin "Spotify support is incomplete"))
(defn resolve-spotify-url-info [spotify-url origin]
  (println "resolve-spotify-url-info")
  (respond-to origin "Spotify support is incomplete"))


(defn react-to [matches]
  (let [[_ origin message] matches
        spotify-uri (match-spotify-uri message)
        spotify-url (match-spotify-url message)
        channel-to-be-joined (match-join-request message)
        channel-to-be-parted (match-part-request message)]
    (cond
      (not (nil? spotify-uri)) (resolve-spotify-uri-info spotify-uri origin)
      (not (nil? spotify-url)) (resolve-spotify-url-info spotify-url origin)
      (not (nil? channel-to-be-joined)) (join-channel (first (rest channel-to-be-joined)))
      (not (nil? channel-to-be-parted)) (part origin))
    ))

(defn join-startup-channels []
  (println "joining" (:channels @bot))
  (map #(join-channel %) (:channels @bot))
  )
(defn react-to-message [line]
  (let [
        privmsg (match-private-message line)
        chanmsg (match-channel-message line)
        ]
    (do (println "<- " line)
      (cond
        (ping? line) (write-to-server (str "PONG " (:server @bot)))
        (end-of-motd? line) (join-startup-channels)
        (not (nil? privmsg)) (react-to privmsg)
        (not (nil? chanmsg)) (react-to chanmsg)
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
        newbot (struct ircbot botnick server input output (re-seq #"#[^ ]+" channels))]
    (if (nil? socket)
      (println "Socket is not up?")
      (do
        (dosync
          (ref-set bot newbot))
        (send-nick)
        (send-user)

        (try
          (react-to-messages)
          (catch NullPointerException e ((println  "Caught nullpointer: " (.getMessage e))))
          (finally (.close socket))
          )
        ))))
(start-bot "iStutter" "irc.inet.fi" 6667 "#paskamaja #paskamaja2")

 