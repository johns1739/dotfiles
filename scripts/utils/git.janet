(use sh)

(def non-feature-branches @["development" "develop" "dev" "master" "main"])

(defn is-string-non-feature-branch?
  "True if branch name is a non-feature branch, e.g. main."
  [branch]
  (truthy? (peg/match ~(* (+ ,;non-feature-branches) -1)
                      (-> branch (string/trim) (string/ascii-lower)))))

(defn is-string-feature-branch?
  "True if branch name is a feature branch."
  [branch]
  (not (is-string-non-feature-branch? branch)))

(defn fetch
  "Fetch and prune from remotes."
  [repo-path]
  (print "git FETCH " repo-path)
  ($? git -C ,repo-path fetch -pPq))

(defn has-staged-changes?
  "True if repo has staged changes."
  [repo-path]
  (not ($? git -C ,repo-path diff --cached --quiet)))

(defn remove-branch
  "Force-delete a local branch. Returns success."
  [repo-path branch]
  (let [success? ($? git -C ,repo-path branch -D ,branch > @"" > [stderr stdout])]
    (if success?
      (print "git REMOVE branch " branch)
      (print "FAILED git REMOVE branch " branch))
    success?))

(defn default-remote-branch
  "Default branch of the repo. E.g. 'origin/main'"
  [repo-path]
  (let [output @""]
    (when ($? git -C ,repo-path symbolic-ref -q --short refs/remotes/origin/HEAD > ,output)
      (string/trim output))))

(defn merged-branches
  "Local branches merged into the default remote branch."
  [repo-path]
  (if-let [default-branch (default-remote-branch repo-path)]
    (->> ($<_ git -C ,repo-path for-each-ref "--format=%(refname:short)" --merged ,default-branch refs/heads)
         (string/split "\n")
         (filter |(not (empty? $))))
    (do
      (print "SKIP no default branch for origin: " repo-path)
      @[])))

(defn remove-worktree
  "Remove a worktree. Returns success."
  [repo-path worktree & flags]
  (let [success? ($? git -C ,repo-path worktree remove ,;flags -- ,worktree > @"" > [stderr stdout])]
    (if success?
      (print "git REMOVE worktree " worktree)
      (print "FAILED git REMOVE worktree " worktree))
    success?))

(defn has-upstream-set?
  "True if current branch has an upstream."
  [repo-path]
  ($? git -C ,repo-path rev-parse --abbrev-ref "@{u}" > @"" > [stderr stdout]))

(defn has-committed-changes?
  "True if repo has commits not pushed to upstream."
  [repo-path]
  (and (has-upstream-set? repo-path)
       (not (empty? ($<_ git -C ,repo-path rev-list -1 "@{u}..HEAD")))))

(defn stash
  "Stash changes, including untracked. Returns true if stashed."
  [repo-path & flags]
  (let [output ($<_ git -C ,repo-path stash push "--include-untracked" ,;flags)
        stashed? (not= output "No local changes to save")]
    (when stashed? (print "git STASHED"))
    stashed?))

(defn stash-with-message
  "Stash changes with a message."
  [repo-path message]
  (stash repo-path "--message" message))

(defn pop-stash
  "Pop latest stash. Returns success."
  [repo-path & flags]
  (let [success? ($? git -C ,repo-path stash pop -q ,;flags)]
    (if success?
      (print "git STASH-POP")
      (print "FAILED git STASH-POP, changes kept in stash"))
    success?))

(defn current-branch
  "Name of the checked out branch."
  [repo-path]
  (string/trim ($< git -C ,repo-path branch --show-current)))

(defn fast-forward
  "Fast-forward current branch from origin."
  [repo-path]
  (let [branch (current-branch repo-path)]
    (print "git FAST-FORWARD " repo-path)
    ($? git -C ,repo-path pull -q --ff-only origin ,branch)))

(defn is-git-dir?
  "True if path is inside a git repo."
  [repo-path]
  ($? git -C ,repo-path rev-parse --git-dir > ,@"" > [stderr stdout]))

(defn on-feature-branch?
  "True if repo is on a feature branch."
  [repo-path]
  (is-string-feature-branch? (current-branch repo-path)))

(defn fast-forward-with-stash
  "Stash, fast-forward repo, and then pop stash."
  [repo-path]
  (let [stashed? (stash-with-message repo-path (string (os/time) " sync save"))]
    (let [success? (fast-forward repo-path)]
      (if stashed?
        (and (pop-stash repo-path) success?)
        success?))))

(defn prune-worktrees
  "Prune stale worktree entries."
  [repo-path]
  ($? git -C ,repo-path worktree prune))

(defn worktrees
  "List linked worktrees."
  [repo-path]
  (->> ($< git -C ,repo-path worktree list --porcelain)
       (string/split "\n")
       (keep |(first (peg/match '(* "worktree " (<- (some 1)) -1) $)))
       (drop 1)))

(defn removable-worktrees
  "List linked worktrees, except worktrees with staged changes or unpushed committed changes."
  [repo-path]
  (->> (worktrees repo-path)
       (filter |(not (has-committed-changes? $)))
       (filter |(not (has-staged-changes? $)))))
