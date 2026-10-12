(use sh)

(def stash-sync-message (string (os/time) " sync save"))

(def non-feature-branches @["development" "develop" "dev" "master" "main"])

(defn is-string-non-feature-branch? [branch]
  (truthy? (peg/match ~(* (+ ,;non-feature-branches) -1)
                      (-> branch (string/trim) (string/ascii-lower)))))

(defn is-string-feature-branch? [branch]
  (not (is-string-non-feature-branch? branch)))

(defn string-normalize-branch [branch]
  (first (peg/match '(* (+ "+" "*" "") (any " ") (<- (some 1)) -1)
                    (string/trim branch))))

(defn fetch [repo-path]
  (print "git FETCH " repo-path)
  ($? git -C ,repo-path fetch -pPq))

(defn has-staged-changes? [repo-path]
  (any? ($<_ git -C ,repo-path diff --cached --quiet)))

(defn branches [repo-path & flags]
  (let [output ($<_ git -C ,repo-path branch ,;flags)
        lines (string/split "\n" output)
        branches (map string-normalize-branch lines)]
    branches))

(defn remove-branch [repo-path branch]
  (print "git REMOVE branch " branch)
  (branches repo-path "-d" branch))

(defn merged-branches [repo-path]
  (branches repo-path "--merged" "origin/main"))

(defn remove-worktree [repo-path worktree & flags]
  (let [success? ($? git -C ,repo-path worktree remove ,;flags -- ,worktree > @"" > [stderr stdout])]
    (if success?
      (print "git REMOVE worktree" worktree)
      (print "FAILED git REMOVE worktree" worktree))
    success?))

(defn has-upstream-set? [repo-path]
  ($? git -C ,repo-path rev-parse --abbrev-ref "@{u}" > @"" > [stderr stdout]))

(defn has-committed-changes? [repo-path]
  (and (has-upstream-set? repo-path)
       (any? ($<_ git -C ,repo-path rev-list -1 "@{u}..HEAD"))))

(defn status [repo-path & flags]
  # ($<_ git -C ,repo-path status --porcelain --untracked-files=no))
  ($<_ git -C ,repo-path status --porcelain ,;flags))

(defn stash [repo-path & flags]
  (if-let [output ($<_ git -C ,repo-path stash push ,;flags)
           stashed? (not= output "No local changes to save")]
    (print "git STASHED")))

(defn stash-with-message [repo-path message]
  (stash repo-path "--message" message))

(defn pop-stash [repo-path & flags]
  (when ($? git -C ,repo-path stash pop -q ,;flags)
    (print "git STASH-POP")))

(defn pop-stash-with-message [repo-path message]
  (pop-stash repo-path (string/format "stash^{/%s}" message)))

(defn current-branch [repo-path]
  (string/trim ($< git -C ,repo-path branch --show-current)))

(defn fast-forward [repo-path]
  (let [branch (current-branch repo-path)]
    (print "git FAST-FORWARD " repo-path)
    ($? git -C ,repo-path pull -q --ff-only origin ,branch)))

(defn is-git-dir? [repo-path]
  ($? git -C ,repo-path rev-parse --git-dir > ,@"" > [stderr stdout]))

(defn on-feature-branch? [repo-path]
  (is-string-feature-branch? (current-branch repo-path)))

(defn fast-forward-with-stash
  "Stash,  fast-forward repo, and then pop stash."
  [repo-path]
  (let [stashed? (stash-with-message repo-path stash-sync-message)]
    (let [success? (fast-forward repo-path)]
      (when stashed?
        (pop-stash-with-message repo-path stash-sync-message))
      success?)))

(defn worktrees
  "List non-repository worktrees."
  [repo-path]
  (let [output ($< git -C ,repo-path worktree list --porcelain)]
    (->> output
         (string/split "\n")
         (map |(peg/match '(* "worktree " (<- (some 1)) -1) $))
         (keep |(and (truthy? $) (not (is-git-dir? (first $))) (first $))))))

(defn dirty-worktrees
  "List of worktree, except worktrees with staged changes or unpushed-comitted changes."
  [repo-path]
  (->> (worktrees repo-path)
       (filter |(not (has-committed-changes? $)))
       (filter |(not (has-staged-changes? $)))))
