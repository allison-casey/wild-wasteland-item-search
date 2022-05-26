;; * Pathlib
;; ** Imports
(import
  json
  csv
  pathlib [Path]

  lark
  toolz [assoc comp juxt update-in curry concatv itemmap])

(require hyrule [-> as-> of])

;; ** methods
(defn normalize-numeric [s suffix]
  (-> s (.removesuffix suffix) .strip int))

(setv lmap (comp list map))

(defn rename [coll key-map]
  (dfor [k v] (coll.items)
        [(key-map.get k k) v]))

(setv EMPTY-CELL "——")

(defn read-ranged [path]
  (with [f (open path)]
    (let [reader (csv.DictReader f)]
      (lfor row reader
        (as-> row $
             (itemmap (curry map str.lower) $)
             (rename $ {"weapon" "name"  "type" "subtype"})
             (assoc $ "type" "ranged")
             (update-in $ ["ap"] (curry normalize-numeric :suffix "ap"))
             (update-in $ ["range"] (curry normalize-numeric :suffix "feet"))
             (update-in $ ["damage"] (fn [s] (if (= s EMPTY-CELL) None s)))
             (update-in $ ["traits"] (fn [s]
                                       (if (= s EMPTY-CELL)
                                           None
                                           (lmap str.strip (s.split ","))))))))))

(defn read-melee [path]
  (with [f (open path)]
    (let [reader (csv.DictReader f)]
      (lfor row reader
        (as-> row $
             (itemmap (curry map str.lower) $)
             (rename $ {"weapon" "name"  "type" "subtype"})
             (assoc $ "type" "melee")
             (update-in $ ["ap"] (curry normalize-numeric :suffix "ap"))
             (update-in $ ["reach"] (curry normalize-numeric :suffix "feet"))
             (update-in $ ["traits"] (fn [s]
                                       (if (= s EMPTY-CELL)
                                           None
                                           (lmap str.strip (s.split ","))))))))))

(defn read-equipment [ranged-path melee-path]
  (list (concatv (read-ranged ranged-path) (read-melee melee-path))))

;; ** lexer
(setv grammar #[[
start: query+

query: IDENTIFIER                                 -> name
     | ("ap:" ((CONDITIONAL NUMBER) | NUMBER))    -> ap
     | ("reach:" ((CONDITIONAL NUMBER) | NUMBER)) -> reach
     | ("range:" ((CONDITIONAL NUMBER) | NUMBER)) -> range
     | ("trait:" IDENTIFIER)                      -> trait
     | ("type:" IDENTIFIER)                       -> type
     | ("subtype:" IDENTIFIER)                    -> subtype
     | ("ammo:" IDENTIFIER)                       -> ammo
     | ("dmg-type:" IDENTIFIER)                   -> dmg_type
     | ("(" query ("or" query)+ ")")              -> or
     | ("(" query+ ")")                           -> and

CONDITIONAL: ">" | ">=" | "<" | "<="
IDENTIFIER: (LETTER | "-")+

%import common.LETTER
%import common.INT -> NUMBER
%import common.WS
%ignore WS
]])

(defn parse-query [node]
  (let [numeric-filter
        (fn [key] (if (= (len node.children) 2)
                     (let [[condition val] node.children
                           dispatch {">" hy.pyops.>
                                     ">=" hy.pyops.>=
                                     "<" hy.pyops.<
                                     "<=" hy.pyops.<=}
                           func (get dispatch condition)]
                       (fn [o]
                         (and (in key o) (func (get o key) (int val)))))
                     (let [val (int (get node.children 0))]
                       (fn [o]
                         (= val (and (in key o) (get o key)))))))
        equality-filter
        (fn [key]
          (fn [o] (= (. node children [0] (lower))
                    (get o key))))]

    (match node.data
           "name"
           (fn [o] (in (. node children [0] (lower)) (:name o)))

           "ammo"
           (fn [o] (when (= (:type o) "ranged")
                    (in (. node children [0] (lower)) (:ammunition o))))

           "or"
           (comp any (juxt (map parse-query node.children)))

           "and"
           (comp all (juxt (map parse-query node.children)))

           "reach"
           (numeric-filter "reach")

           "ap"
           (numeric-filter "ap")

           "range"
           (numeric-filter "range")

           "trait"
           (let [trait (. node children [0] (lower))]
             (fn [o] (in trait (or (:traits o) []))))

           "dmg_type"
           (fn [o] (when (:damage o)
                    (= (. node.children [0] (lower))
                       (. o ["damage"] (split) [-1]))))

           "type"
           (equality-filter "type")

           "subtype"
           (equality-filter "subtype")
           )))

(defn #^(of list dict) search
  [#^str query #^(of list dict) items]
  (let [parser (lark.Lark grammar)
        tree (parser.parse query)
        predicates (lmap parse-query tree.children)]
    (as-> predicates $
         (juxt $)
         (comp all $)
         (filter $ items)
         (list $))))
