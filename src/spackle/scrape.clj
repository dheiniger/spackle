(ns spackle.scrape
  (:require [clj-http.client :as client]
            [net.cgrand.enlive-html :as enlive]
            [clojure.string :as str]))

(def base-url "http://redeemernorwalk.org")

(defn get-content
  ([]
    (get-content nil))
  ([subpage]
    (enlive/html-snippet
     (:body(client/get (str base-url subpage) {:insecure true})))))

(defn get-content-of-url
  [url]
  (enlive/html-snippet
    (:body (client/get url {:insecure true}))))

(defn get-nodes
  [html-snippet nodes]
  (enlive/select html-snippet nodes))

(defn get-page-content
  [page]
  (try(get-content (str "/resources/page/" page))
  (catch Exception e "404 File Not Found")))

(defn get-all-page-content
  "Returns a collection of page content as a vector of maps"
  []
  (loop [i 1
       pages []]
    (let [page-content (get-page-content i)]
      (if (not (= "404 File Not Found" page-content))
        (recur (inc i)
        (conj pages {:page i :html-snippet page-content}))pages))))

(defn get-all-page-snippets
  []
  (map :html-snippet (get-all-page-content)))

(defn extract-post-from-snippet
  [html-snippet]
  (get-nodes html-snippet [:article.post]))

(defn get-all-posts
  []
  (loop [html-snippets (get-all-page-snippets)
         posts []]
    (if (not (empty? html-snippets))
      (recur (rest html-snippets)
             (conj posts (extract-post-from-snippet (first html-snippets))))
      (flatten posts))))

(defn extract-title-from-post
  [post]
  (get-nodes post [:h2.entry-title :a enlive/text]))

(defn extract-url-from-post
  [post]
  (first(enlive/attr-values (first(get-nodes post [:h2.entry-title :a])) :href)))

(defn extract-author-from-post
  [post]
  (get-nodes post [:div.entry-meta :span.author :a enlive/text]))

(defn extract-published-date-from-post
  [post]
  (first
    (enlive/attr-values
      (first(get-nodes post [:div.entry-meta :span.posted-on :time.entry-date.published])) 
      :datetime)))

(defn extract-updated-date-from-post
  [post]
  (first
    (enlive/attr-values
      (first(get-nodes post [:div.entry-meta :span.posted-on :time.updated]))
      :datetime)))

(defn extract-tags-from-post
  [post]
  (get-nodes post [:footer.entry-footer :span.tags-links :a enlive/text]))

(defn extract-category-from-post
  [post]
  (set(get-nodes post [:footer.entry-footer :span.cat-links :a enlive/text])))

(defn retrieve-post-content
  [post category]
  (let [post-content (get-content-of-url (extract-url-from-post post))]
  (if (contains? category "Blogs")
    (enlive/emit* (get-nodes post-content [:main :div.entry-content]))
    (extract-sermon-from-post-content post-content))))

(defn extract-preview-text-from-post-content
  [post-content]
  (get-nodes post-content [:p]))

(defn extract-sermon-from-post-content
 [post-content]
 (first
   (enlive/attr-values 
     (first(:content 
             (first
               (get-nodes post-content [:div.entry-content :audio.wp-audio-shortcode])))) 
     :src)))

(defn build-clean-post
  [post]
  (let [category (extract-category-from-post post)]
  {:title        (extract-title-from-post post)
   :author       (extract-author-from-post post)
   :publish-date (extract-published-date-from-post post)
   :update-date  (extract-updated-date-from-post post)
   :tags         (extract-tags-from-post post)
   :category     category
   :content      (retrieve-post-content post category)}))
   
(defn build-all-clean-posts
  []
  (map build-clean-post (get-all-posts)))

