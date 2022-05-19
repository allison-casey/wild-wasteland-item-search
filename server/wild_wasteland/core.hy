(import flask [Flask]
        flask-cors [CORS])

(require hyrule [doto])


(defn create-app []
  (let [app (doto (Flask __name__)
                  (.config.from-object "wild_wasteland.default_settings")
                  (CORS))]
    (import wild-wasteland.view [query])
    (app.register-blueprint query)
    app))
