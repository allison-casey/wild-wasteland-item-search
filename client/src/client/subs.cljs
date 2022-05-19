(ns client.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::query
 (fn [db]
   (:query db)))

(re-frame/reg-sub
 ::items
 (fn [db]
   (:items db)))

(re-frame/reg-sub
 ::valid-query?
 (fn [db]
   (:is-query-valid db)))

(re-frame/reg-sub
 ::query-syntax-error
 (fn [db]
   (:error-message db)))
