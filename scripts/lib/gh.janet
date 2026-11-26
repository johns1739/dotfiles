(use spork/sh-dsl)
(use ./helpers)
(import spork/json)

(var cache @{})

(defn into-cache
  "Saves and returns ENTRIES into a global cache scoped under CONTEXT using entry id"
  [context entries]
  (update cache context |(or $ @{}))
  (def context-cache (cache context))
  (each entry entries
    (put context-cache (entry :id) entry))
  entries)

(defn get-in-cache
  [context id keys]
  (get-in cache [context id ;keys]))

(def pr-list-fields
  [:id
   :baseRefName
   :headRefName
   :title
   :createdAt
   :additions
   :changedFiles
   :reviewDecision])

# JSON FIELDS
#   additions, assignees, author, autoMergeRequest, baseRefName, baseRefOid, body,
#   changedFiles, closed, closedAt, closingIssuesReferences, comments, commits,
#   createdAt, deletions, files, fullDatabaseId, headRefName, headRefOid,
#   headRepository, headRepositoryOwner, id, isCrossRepository, isDraft, labels,
#   latestReviews, maintainerCanModify, mergeCommit, mergeStateStatus, mergeable,
#   mergedAt, mergedBy, milestone, number, potentialMergeCommit, projectCards,
#   projectItems, reactionGroups, reviewDecision, reviewRequests, reviews, state,
#   statusCheckRollup, title, updatedAt, url

(defn pr-list
  "Print pull-request metadata for my open prs"
  [owner-repos]
  (mapcat
    (fn [owner-repo]
      (-> ($< gh pr list
              --author "@me"
              -R ,owner-repo
              --json ,(string/join pr-list-fields ",")
              --state open
              --limit 20)
          (json/decode :use-keywords :use-nils)))
    owner-repos))

# JSON FIELDS
#   assignees, author, authorAssociation, body, closedAt, commentsCount, createdAt,
#   id, isDraft, isLocked, isPullRequest, labels, number, repository, state, title,
#   updatedAt, url

(def search-fields
  [:id :commentsCount :number :repository :title])

(defn search-prs
  "Print pull-request metadata for my open prs"
  []
  (def entries (-> ($< gh search prs
          --author "@me"
          --state open
          --limit 100
          --sort created
          --json ,(string/join search-fields ","))
                   (json/decode :use-keywords :use-nils)))
  (into-cache :search entries))


(defn extract-owner-repos
  [search-prs]
  (distinct (map (fn [pr] (get-in pr [:repository :nameWithOwner])) search-prs)))
