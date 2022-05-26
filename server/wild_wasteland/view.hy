(import
  lark.exceptions [LarkError]
  flask [request Blueprint]

  wild-wasteland.query [search read-equipment])

(setv query (Blueprint "query" __name__)
      equipment (read-equipment "ranged.csv" "melee.csv"))

(with-decorator
  (query.route "/" :methods ["POST"])
  (defn query-items []
    (let [query (:query request.json)]
      (try
        (dict :items (if query
                         (search query equipment)
                         equipment))
        (except [e LarkError]
          #((dict :error (str e)) 400))))))
