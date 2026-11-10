#!/usr/bin/env janet

(import ./utils/git)
(use ./utils/helpers)

(defn clean-branches
  "Remove local feature branches merged into the default branch."
  [repo-path]
  (each branch (git/merged-branches repo-path)
    (when (git/is-string-feature-branch? branch)
      (git/remove-branch repo-path branch))))

(defn clean-worktrees
  "Stash leftover changes and remove worktrees without unpushed work."
  [repo-path]
  (git/prune-worktrees repo-path)
  (each worktree (git/removable-worktrees repo-path)
    (def stash-message (string "Leftover changes from " worktree " @ " (current-date-timestamp)))
    (git/stash-with-message worktree stash-message)
    (git/remove-worktree repo-path worktree "-f")))

(defn clean-repo
  "Fetch, then clean worktrees and branches of a repo."
  [repo-path]
  (print "\nCleaning " repo-path " ... ")

  (unless (git/is-git-dir? repo-path)
    (print "SKIP not a git directory: " repo-path)
    (break))

  (git/fetch repo-path)
  (clean-worktrees repo-path)
  (clean-branches repo-path)
  (print "DONE"))

(defn clean-repos
  "Clean each repo."
  [repo-paths]
  (each repo-path repo-paths
    (clean-repo repo-path)))

(defn main
  "Clean the repos given as arguments."
  [_script & repo-paths]
  (clean-repos repo-paths))
