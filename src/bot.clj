(import '(java.io InputStreamReader PrintWriter BufferedReader))
(import '(java.net URL Socket))


(defstruct ircbot :nick :input :output :channels)
(defstruct message :text :output)

(defn end-of-motd? [line] (re-matches #".*376.*" line))
(defn ping? [line] (re-matches #".*PING.*" line))

(defn write-to-server [message]
  (let [line (:text message)
        output (:output message)]
  (do (.println output line)
      (.flush output))))

(defn react-to-message [line output]
  (do (println "<- " line)
      (if (ping? line) (write-to-server (struct message "PONG" output)))))



(defn react-to-messages [bot]
  (let [line (.readLine (:input bot))
        output (:output bot)]
    (react-to-message line output))
    (recur bot))

(defn start-bot [nick, server, port, channels]
  (println (str "Trying to create a bot with nick=" nick "; server=" server "; port=" port "; channels=" channels))
  (let [socket (Socket. server port)
        input (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        output (PrintWriter. (.getOutputStream socket))
        bot (struct ircbot nick input output channels)]
    (react-to-messages bot)))


