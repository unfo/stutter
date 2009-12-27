(def visitors (ref #{}))
(defn hello
  "This is the documentation for hello"
  [username]
  (dosync
    (let [past-visitor (@visitors username)]
      (if past-visitor
        (str "welcome back, " username)
        (do
          (alter visitors conj username)
          (str "nice to meet you, " username))))))