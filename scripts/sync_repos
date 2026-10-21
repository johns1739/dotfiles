#!/usr/bin/env janet

(import ./utils/git)

(defn sync-repo
  "Fetch and fast-forward a repo, unless on a feature branch."
  [repo-path]

  (unless (git/is-git-dir? repo-path)
    (print "SKIP (not-git-repo): " repo-path)
    (break))

  (when (git/on-feature-branch? repo-path)
    (printf "SKIP (on-feat-branch:%s): %s" (git/current-branch repo-path) repo-path)
    (break))

  (git/fetch repo-path)

  (if (git/fast-forward-with-stash repo-path)
    (print "DONE: " repo-path)
    (print "FAILED: " repo-path)))

(defn sync-repos
  "Sync each repo."
  [repo-paths]
  (each repo-path repo-paths
    (sync-repo repo-path)))

(defn main
  "Sync the repos given as arguments."
  [_script & repo-paths]
  (sync-repos repo-paths))
