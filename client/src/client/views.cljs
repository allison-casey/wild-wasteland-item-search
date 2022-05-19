(ns client.views
  (:require
   [reagent.core :as r]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com :refer [at]]
   [client.subs :as subs]
   [client.events :as events]
   [cuerdas.core :as cuerdas]))

(def popover-labels
  {;; weapon traits
   "automatic" "This weapon can be used to take the Suppression action or any other action that requires a weapon with the automatic trait."
   "agile" "This weapon uses your Agility modifier in place of your Strength modifier for attack rolls and damage."
   "energy" "You may add half of your Intelligence modifier (rounded down) to this weapon’s base damage."
   "heavy" "You lose 5 feet of speed while using this weapon."
   "reload" "This weapon must be reloaded after the number of shots in parenthesis. Reloading costs 1 AP."
   "special" "This weapon has a special property or behaves in an abnormal way. The description of its special rules is located in the Special Weapons section."
   "spread" "This weapon can be used to take actions that require a weapon with the spread trait."
   "throwable" "You can throw this weapon without having disadvantage on the attack roll."
   "two-handed" "This weapon requires two hands to be used effectively."
   "cqb" "You do not roll with disadvantage on attack rolls when targeting a creature within 5 feet with this weapon."
   "reload (1)" "This weapon must be reloaded after the number of shots in parenthesis. Reloading costs 1 AP."

   ;; melee weapon types
   "blunt" "weapons such as clubs or maces are defined by a large flat surface capable of bludgeoning or beating."
   "edged" "weapons such as a swords or knives are equipped with sharp blades that are used to slice or cut-down enemies."
   "lashing" "weapons such as whips or lashing rods are made of pliant, flexible materials and are used to whip opponents and inflict damage."
   "piercing" "weapons such as daggers or spears are defined by their precise, sharp point."
   "unarmed" "weapons such as brass knuckles or claw gauntlets are close quarters tools used in brawls and fistfights."

   ;; ranged weapon types
   "ar" "Assault rifles such as laser assault rifles or standard assault rifles are automatic firearms that typically use an intermediate cartridge and used in mid- range combat."
   "br" "Battle rifles such as service rifles and hunting rifles are firearms that use low to mid-caliber ammunition and are typically used in mid-range engagements."
   "exotic" "Exotic ranged weapons such as a net gun are weapons that shoot unconventional ammunition and do not neatly fit in any of the standard ranged weapon types."
   "explosive" "Explosive weapons such as grenade launchers or rocket launchers encompass any weapon fires an explosive projectile. Placed or thrown explosive devices that deal damage also count as explosive weapons."
   "lmg" "Light machineguns (LMG) such as a standard light machinegun or heavy machinegun are large, fully automatic, long-barreled firearms that are capable of prolonged, sustained fire."
   "pistol" "Pistols such as revolvers or laser pistols are small firearms that can typically be used in one hand."
   "primitive" "Primitive ranged weapons such as bows or slingshots are weapons that use simple munitions and basic methods of propelling projectiles at targets."
   "shotgun" "Shotguns are firearms that discharge a blast of scattered ammunition in a spreading pattern and are typically only used in very close quarters."
   "sniper" "Sniper rifles such as anti-materiel rifles or gauss rifles are long-barreled long-range firearms that fire high- caliber ammunition."
   "smg" "Submachine guns (SMG) such as laser submachine guns or standard submachine guns are small, low-caliber automatic firearms typically used in close range combat."
   "surge" "Surge weapons such as flamethrowers or arc-casters are ranged weapons that deal damage a through sustained fire of some exotic form of munition."})

(def special-label-casing-lookup {"cqb" "CQB"
                                  "br" "BR"
                                  "ar" "AR"
                                  "smg" "SMG"
                                  "lmg" "LMG"})

(defmulti render-item :type)

(defn tooltip [& {:keys [key tag value class]
                  :or {class ""}}]
  (let [is-showing (r/atom false)]
    (fn []
      [re-com/popover-tooltip
       :label (get popover-labels (cuerdas/lower key))
       :showing? is-showing
       :width "20em"
       :class class
       :anchor [tag {:on-mouse-over #(reset! is-showing true)
                     :on-mouse-out #(reset! is-showing false)} value]])))

(defn card [title type subtype ap traits definitions]
  [:div.card.m-3 {:style {:width "21em"}}
   [re-com/v-box
    :class "card-body"
    :children [[re-com/h-box
                :justify :between
                :children [[:h4.card-title (cuerdas/title title)]
                           [:h6.card-subtitle.text-muted.mt-1 (cuerdas/format "%s AP" ap)]]]
               [re-com/h-box
                :justify :between
                :children [[:h6.card-subtitle.mb-2 (cuerdas/title type)]
                           [tooltip
                            :key subtype
                            :tag :h6.card-subtitle.text-muted
                            :value (get special-label-casing-lookup subtype (cuerdas/title subtype))]]]
               [re-com/line]
               [re-com/v-box
                :margin "1rem 0"
                :children
                (for [[key value] (seq definitions)]
                  ^{:key key}
                  [re-com/h-box
                   :justify :between
                   :children [[:dt key] [:dd value]]])]
               (when (seq traits)
                 [:<>
                  [:h5.card-title.my-2 "Traits"]
                  [:ul.list-group.list-group-flush
                   (for [trait traits]
                     ^{:key trait}
                     [tooltip
                      :class "list-group-item"
                      :key trait
                      :tag :div
                      :value (get special-label-casing-lookup trait (cuerdas/title trait))])]])]]])

(defn add-suffix [s suffix]
  (cuerdas/format "%s %s" s suffix))

(defmethod render-item "melee"
  [item]
  [card
   (:name item)
   (:type item)
   (:subtype item)
   (:ap item)
   (:traits item)
   (-> item
       (select-keys [:damage :reach])
       (update :reach add-suffix "feet"))])

(defmethod render-item "ranged"
  [item]
  [card
   (:name item)
   (:type item)
   (:subtype item)
   (:ap item)
   (:traits item)
   (-> item
       (select-keys [:damage :range :ammunition])
       (update :range add-suffix "feet"))])

(defn item-list []
  (let [items (re-frame/subscribe [::subs/items])]
    [re-com/h-box
     :style {:flex-flow "row wrap"
             :row-gap "1em"}
     :justify :between
     :gap "50px"
     :children [(for [item @items]
                  ^{:key (:name item)}
                  [render-item item])]]))

(defn text->hiccup
  "Convert newlines to [:br]'s."
  [text]
  (->> (clojure.string/split text "\n")
       (interpose [:br])
       (map #(if (string? %)
               %
               (with-meta % {:key (gensym "br-")})))))

(defn item-pane []
  (let [query (re-frame/subscribe [::subs/query])
        valid-query? (re-frame/subscribe [::subs/valid-query?])
        query-syntax-error (re-frame/subscribe [::subs/query-syntax-error])]
    [re-com/v-box
     :src (at)
     :margin "4em auto"
     :width "80%"
     :children [[:div.mx-3
                 [re-com/input-text
                  :model query
                  :change-on-blur? false
                  :width "100%"
                  :status-icon? (not @valid-query?)
                  :status (when-not @valid-query? :error)
                  :on-change #(re-frame/dispatch [::events/update-query-string %])]]
                (when (not @valid-query?)
                  [re-com/h-box
                   :class "m-3 alert alert-danger"
                   :style {:white-space "pre"
                           :font-family "'Courier New', monospace"}
                   :children [(text->hiccup @query-syntax-error)]])
                [item-list]]]))

(defn main-panel []
  [re-com/v-box
   :src      (at)
   :height   "100%"
   :children [[item-pane]]])
