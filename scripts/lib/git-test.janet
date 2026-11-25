(use judge)
(use sh)
(import ./git)
(use ./fixtures)

(deftest "branch names"
  (test (git/is-string-non-feature-branch? "main") true)
  (test (git/is-string-non-feature-branch? " Develop\n") true)
  (test (git/is-string-non-feature-branch? "mainline") false)
  (test (git/is-string-feature-branch? "123-feature") true)
  (test (git/is-string-feature-branch? "master") false))

(deftest "repo state"
  (def repo (git-repo "state"))
  (test (git/is-git-dir? repo) true)
  (test (git/is-git-dir? root) false)
  (test (git/current-branch repo) "main")
  (test (git/on-feature-branch? repo) false)
  (test (git/default-remote-branch repo) "origin/main")
  (test (git/has-upstream-set? repo) true)
  (test (git/has-committed-changes? repo) false)
  (commit repo "unpushed")
  (test (git/has-committed-changes? repo) true)
  (git* repo "switch" "-q" "-c" "1-feature")
  (test (git/on-feature-branch? repo) true)
  (test (git/has-upstream-set? repo) false)
  (test (git/has-committed-changes? repo) false))

(deftest "debug git commands"
  (def repo (git-repo "debug"))
  (os/setenv "DEBUG" "true")
  (test-stdout (git/current-branch repo) `
    DEBUG: git-command => ("git" "-C" "/private/tmp/judge-scripts/debug/clone" "branch" "--show-current")
    DEBUG: output => "main"
  ` "main")
  (test-stdout (git/has-staged-changes? repo) `
    DEBUG: git-command => ("git" "-C" "/private/tmp/judge-scripts/debug/clone" "diff" "--cached" "--quiet")
    DEBUG: success? => true
  `)
  (os/setenv "DEBUG" nil))

(deftest "fetch"
  (def repo (git-repo "fetch"))
  (test-stdout (git/fetch repo) `
    git FETCH /private/tmp/judge-scripts/fetch/clone
  ` true))

(deftest "staged changes and stash"
  (def repo (git-repo "stash"))
  (test (git/has-staged-changes? repo) false)
  (spit (string repo "/file.txt") "changes")
  (git* repo "add" "file.txt")
  (test (git/has-staged-changes? repo) true)
  (test-stdout (git/stash-with-message repo "saved") `
    git STASHED
  ` true)
  (test (git/has-staged-changes? repo) false)
  (test (string/has-suffix? "saved" ($<_ git -C ,repo stash list)) true)
  (test (git/stash repo) false)
  (test-stdout (git/pop-stash repo) `
    git STASH-POP
  ` true)
  (test (slurp (string repo "/file.txt")) @"changes")
  (git* repo "stash" "clear")
  (test-stdout (git/pop-stash repo) `
    FAILED git STASH-POP, changes kept in stash
  `))

(deftest "branches"
  (def repo (git-repo "branches"))
  (git* repo "branch" "1-merged")
  (git* repo "switch" "-q" "-c" "2-unmerged")
  (commit repo "unmerged")
  (git* repo "switch" "-q" "main")
  (test (git/merged-branches repo) @["1-merged" "main"])
  (test-stdout (git/remove-branch repo "1-merged") `
    git REMOVE branch 1-merged
  ` true)
  (test-stdout (git/remove-branch repo "1-merged") `
    FAILED git REMOVE branch 1-merged
  `)
  (git* repo "remote" "set-head" "origin" "--delete")
  (test-stdout (git/merged-branches repo) `
    SKIP no default branch for origin: /private/tmp/judge-scripts/branches/clone
  ` @[]))

(deftest "worktrees"
  (def repo (git-repo "worktrees"))
  (def clean-worktree (string repo "-clean"))
  (def staged-worktree (string repo "-staged"))
  (def unpushed-worktree (string repo "-unpushed"))
  (git* repo "worktree" "add" "-q" "-b" "1-clean" clean-worktree)
  (git* repo "worktree" "add" "-q" "-b" "2-staged" staged-worktree)
  (git* repo "worktree" "add" "-q" "--track" "-b" "3-unpushed" unpushed-worktree "origin/main")
  (spit (string staged-worktree "/file.txt") "changes")
  (git* staged-worktree "add" "file.txt")
  (commit unpushed-worktree "unpushed")
  (test (git/worktrees repo)
    ["/private/tmp/judge-scripts/worktrees/clone-clean"
     "/private/tmp/judge-scripts/worktrees/clone-staged"
     "/private/tmp/judge-scripts/worktrees/clone-unpushed"])
  (test (git/removable-worktrees repo)
    @["/private/tmp/judge-scripts/worktrees/clone-clean"])
  (test-stdout (git/remove-worktree repo clean-worktree) `
    git REMOVE worktree /private/tmp/judge-scripts/worktrees/clone-clean
  ` true)
  (test-stdout (git/remove-worktree repo staged-worktree) `
    FAILED git REMOVE worktree /private/tmp/judge-scripts/worktrees/clone-staged
  `)
  ($ rm -rf ,unpushed-worktree)
  (git/prune-worktrees repo)
  (test (git/worktrees repo)
    ["/private/tmp/judge-scripts/worktrees/clone-staged"]))

(deftest "fast-forward"
  (def repo (git-repo "fast-forward"))
  (def other (string repo "-other"))
  ($ git clone -q ,(string root "/fast-forward/origin.git") ,other)
  (commit other "upstream")
  (git* other "push" "-q")
  (spit (string repo "/file.txt") "local")
  (test-stdout (git/fast-forward-with-stash repo) `
    git STASHED
    git FAST-FORWARD /private/tmp/judge-scripts/fast-forward/clone
    git STASH-POP
  ` true)
  (test (slurp (string repo "/file.txt")) @"local")
  (test ($<_ git -C ,repo log -1 --format=%s) "upstream"))
