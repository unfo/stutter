(import '(java.io InputStreamReader PrintWriter BufferedReader))
(import '(java.net URL Socket))


(defstruct ircbot :nick :server :input :output :channels)
(defstruct message :text :output)

(defn end-of-motd? [line] (re-matches #".* 376 .*" line))
(defn ping? [line] (re-matches #"PING .*" line))

(defn write-to-server [message]
  (let [line (:text message)
        output (:output message)]
    (do
      (println "-> " line)
      (.println output line)
      (.flush output))))

(defn react-to-message [line server output]
  (do (println "<- " line)
      (cond
          (ping? line) (write-to-server (struct message (str "PONG " server) output))
        )))

(defn react-to-messages [bot]
  (let [line (.readLine (:input bot))
        output (:output bot)
        server (:server bot)]
    (react-to-message line server output))
    (recur bot))

(defn send-nick [nick output]
  (write-to-server (struct message (str "NICK " nick) output)))
(defn send-user [server output]
    (write-to-server (struct message (str "USER stutter stutter " server " :Stutter Bot") output)))

(defn start-bot [nick, #^String server, #^Integer port, channels]
  (println (str "Trying to create a bot with nick=" nick "; server=" server "; port=" port "; channels=" channels))
  (let [socket (Socket. server port)
        input (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        output (PrintWriter. (.getOutputStream socket))
        bot (struct ircbot nick server input output channels)]
    (do
    (send-nick nick output)
    (send-user server output)
    (react-to-messages bot)
      )))


 