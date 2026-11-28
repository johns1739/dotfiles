(use spork/sh-dsl)
(use ./helpers)
(import spork/json)

(var cache @{})

(defn into-cache
  "Saves and returns ENTRIES into a global cache scoped under CONTEXT using entry id"
  [entries context]
  (update cache context |(or $ @{}))
  (def context-cache (cache context))
  (each entry entries
    (put context-cache (entry :id) entry))
  entries)

(def cache-context-search-prs :search-prs)
(def cache-context-search-issues :search-issues)
(def cache-context-issue-list :issue-list)
(def cache-context-pr-list :pr-list)

(defn get-in-cache
  [pr context keys dflt]
  (get-in cache [context (pr :id) ;keys] dflt))

(defn get-in-search-prs
  [pr keys dflt]
  (get-in-cache pr cache-context-search-prs keys dflt))

(defn get-in-search-issues
  [pr keys dflt]
  (get-in-cache pr cache-context-search-issues keys dflt))

(defn get-in-issue-list
  [pr keys dflt]
  (get-in-cache pr cache-context-issue-list keys dflt))

(defn get-in-pr-list
  [pr keys dflt]
  (get-in-cache pr cache-context-pr-list keys dflt))

(def pr-list-fields
  # JSON FIELDS
  #   additions, assignees, author, autoMergeRequest, baseRefName, baseRefOid, body,
  #   changedFiles, closed, closedAt, closingIssuesReferences, comments, commits,
  #   createdAt, deletions, files, fullDatabaseId, headRefName, headRefOid,
  #   headRepository, headRepositoryOwner, id, isCrossRepository, isDraft, labels,
  #   latestReviews, maintainerCanModify, mergeCommit, mergeStateStatus, mergeable,
  #   mergedAt, mergedBy, milestone, number, potentialMergeCommit, projectCards,
  #   projectItems, reactionGroups, reviewDecision, reviewRequests, reviews, state,
  #   statusCheckRollup, title, updatedAt, url
  [:id
   :author
   :baseRefName
   :headRefName
   :title
   :createdAt
   :additions
   :changedFiles
   :reviewDecision])

(var pr-search-query @["author:@me" "OR" "review-requested:@me review:none"])

(defn pr-list
  "Print pull-request metadata for my open prs"
  [owner-repos]
  (mapcat
    (fn [owner-repo]
      (-> ($< gh pr list
              -R ,owner-repo
              --state open
              --limit 20
              --search ,(string/join pr-search-query " ")
              --json ,(string/join pr-list-fields ","))
          (json/decode :use-keywords :use-nils)
          (dbg)
          (into-cache cache-context-pr-list)))
    owner-repos))

(def search-pr-fields
  # JSON FIELDS
  #   assignees, author, authorAssociation, body, closedAt, commentsCount, createdAt,
  #   id, isDraft, isLocked, isPullRequest, labels, number, repository, state, title,
  #   updatedAt, url
  #
  #   {
  #   "repository": {
  #     "name": "api",
  #     "nameWithOwner": "purpleplatform/api"
  #   }
  # },
  [:id :commentsCount :number :repository :title])

(defn search-prs
  "Search my open prs"
  []
  (-> ($< gh search prs ,;pr-search-query
          --state open
          --limit 100
          --sort created
          --json ,(string/join search-pr-fields ","))
      (json/decode :use-keywords :use-nils)
      (into-cache cache-context-search-prs)))

(def search-issue-fields
  # JSON FIELDS
  # assignees, author, authorAssociation, body, closedAt, commentsCount, createdAt,
  # id, isLocked, isPullRequest, labels, number, repository, state, title,
  # updatedAt, url
  [:id :repository])

(defn search-issues
  "Search my open issues"
  []
  (-> ($< gh search issues
          --author "@me"
          --state open
          --limit 100
          --json ,(string/join search-issue-fields ","))
      (json/decode :use-keywords :use-nils)
      (into-cache cache-context-search-issues)))

(def issue-list-fields
  # JSON FIELDS
  # assignees, author, blockedBy, blocking, body, closed, closedAt,
  # closedByPullRequestsReferences, comments, createdAt, id, isPinned, issueType,
  # labels, milestone, number, parent, projectCards, projectItems, reactionGroups,
  # state, stateReason, subIssues, subIssuesSummary, title, updatedAt, url
    # "blockedBy|blocking|subIssues": {
    #   "nodes": [
    #     {
    #       "id": "I_kwDOITXGZ88AAAABVU15SQ",
    #       "number": 8864,
    #       "state": "OPEN",
    #       "title": "Remove shadow-case filtering and the referenceBank suppression flag",
    #       "url": "https://github.com/purpleplatform/frontend/issues/8864"
    #     }
    #   ],
    #   "totalCount": 1
  [:id :title :blockedBy :blocking :subIssues :createdAt :updatedAt])

(defn issue-list
  "Print pull-request metadata for my open prs"
  [owner-repos]
  (mapcat
    (fn [owner-repo]
      (-> ($< gh issue list
              --author "@me"
              -R ,owner-repo
              --json ,(string/join issue-list-fields ",")
              --state open
              --limit 20)
          (json/decode :use-keywords :use-nils)
          (into-cache cache-context-issue-list)))
    owner-repos))

(defn extract-owner-repos
  [search-prs]
  (distinct (map (fn [pr] (get-in pr [:repository :nameWithOwner])) search-prs)))
