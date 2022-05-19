(ns client.events
  (:require
   [ajax.core :as ajax]
   [re-frame.core :as re-frame]
   [client.db :as db]))

(defonce timeouts (atom {}))

(re-frame/reg-fx
 :dispatch-debounce
 (fn [[id event-vec n]]
   (js/clearTimeout (@timeouts id))
   (swap! timeouts assoc id
          (js/setTimeout (fn []
                           (re-frame/dispatch event-vec)
                           (swap! timeouts dissoc id))
                         n))))

(re-frame/reg-fx
 :stop-debounce
 (fn [id]
   (js/clearTimeout (@timeouts id))
   (swap! timeouts dissoc id)))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-fx
 ::update-query-string
 (fn [{db :db} [_ query-string]]
   {:db (assoc db :query query-string)
    :dispatch-debounce [::backend-item-query [::query-db query-string] 600]}))

(re-frame/reg-event-fx
 ::query-db
 (fn [{db :db} [_ query-string]]
   {:http-xhrio {:method :post
                 :uri "http://localhost:5000/"
                 :params {:query query-string}
                 :format (ajax/json-request-format)
                 :response-format (ajax/json-response-format {:keywords? true})
                 :on-success [::update-items]
                 :on-failure [::bad-response]}
    :db (assoc db :is-loading true)}))

(re-frame/reg-event-db
 ::update-items
 (fn [db [_ result]]
   (assoc db
          :items (:items result)
          :is-query-valid true)))

(re-frame/reg-event-db
 ::bad-response
 (fn [db [_ result]]
   (assoc db
          :is-query-valid false
          :error-message (get-in result [:response :error] ""))))
