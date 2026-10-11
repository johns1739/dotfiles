(use sh)

(def stash-sync-message (string (os/time) " sync save"))
(def non-feature-branches @["development" "develop" "dev" "master" "main"])

(defn string-non-feature-branch? [branch]
  (truthy? (peg/match ~(* (+ ,;non-feature-branches) -1)
                      (-> branch (string/trim) (string/ascii-lower)))))

(defn string-feature-branch? [branch]
  (not (string-non-feature-branch? branch)))

(defn fetch [repo-path]
  (print "FETCH " repo-path)
  ($? git -C ,repo-path fetch -pPq))

(defn has-staged-changes? [repo-path]
  (any? ($<_ git -C ,repo-path diff --cached --quiet)))

(defn merged-branches [repo-path]
  (let [output ($<_ git -C ,repo-path branch --merged "origin/main")
        lines (string/split "\n" output)
        feature-branches (keep string-feature-branch? lines)]
    feature-branches))

(defn remove-worktree [repo-path worktree & flags]
  (print "REMOVE " worktree)
  ($? git -C ,repo-path worktree remove ,;flags -- ,worktree))

(defn has-upstream-set? [repo-path]
  ($? git -C ,repo-path rev-parse --abbrev-ref "@{u}" > [stderr (file/open "/dev/null")]))

(defn has-committed-changes? [repo-path]
  (and (has-upstream-set? repo-path)
       (any? ($<_ git -C ,repo-path rev-list -1 "@{u}..HEAD"))))

(defn status [repo-path & flags]
  # ($<_ git -C ,repo-path status --porcelain --untracked-files=no))
  ($<_ git -C ,repo-path status --porcelain ,;flags))

(defn stash [repo-path & flags]
  (if-let [output ($<_ git -C ,repo-path stash push ,;flags)
           stashed? (not= output "No local changes to save")]
    (print "STASHED")))

(defn stash-with-message [repo-path message]
  (stash repo-path "--message" message))

(defn pop-stash [repo-path & flags]
  (when ($? git -C ,repo-path stash pop -q ,;flags)
    (print "STASH-POP")))

(defn pop-stash-with-message [repo-path message]
  (pop-stash repo-path (string/format "stash^{/%s}" message)))

(defn current-branch [repo-path]
  (string/trim ($< git -C ,repo-path branch --show-current)))

(defn fast-forward [repo-path]
  (let [branch (current-branch repo-path)]
    (print "FAST-FORWARD " repo-path)
    ($? git -C ,repo-path pull -q --ff-only origin ,branch)))

(defn is-git-dir? [repo-path]
  ($? git -C ,repo-path rev-parse --git-dir :error-to-out >,@""))

(defn on-feature-branch? [repo-path]
  (string-feature-branch? (current-branch repo-path)))

(defn fast-forward-with-stash [repo-path]
  (let [stashed? (stash-with-message repo-path stash-sync-message)]
    (when (fast-forward repo-path)
      (when stashed?
        (pop-stash-with-message repo-path stash-sync-message)))))

(defn worktrees [repo-path]
  (let [output ($< git -C ,repo-path worktree list --porcelain)]
    (->> output
         (string/split "\n")
         (map |(peg/match '(* "worktree " (<- (some 1)) -1) $))
         (keep truthy?)
         (map first)
         (filter |(not (= $ repo-path))))))
