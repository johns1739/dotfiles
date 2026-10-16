(use judge)
(use sh)
(array/push module/paths [":cur:/:all:" :source |(string/has-prefix? "." $)])
(import ./clean_repos)
(use ./utils/fixtures)

(deftest "clean-branches"
  (def repo (git-repo "clean-branches"))
  (git* repo "branch" "1-merged")
  (git* repo "switch" "-q" "-c" "2-unmerged")
  (commit repo "unmerged")
  (git* repo "switch" "-q" "main")
  (test-stdout (clean_repos/clean-branches repo) `
    git REMOVE branch 1-merged
  `)
  (test ($<_ git -C ,repo branch "--format=%(refname:short)") "2-unmerged\nmain"))

(deftest "clean-worktrees"
  (def repo (git-repo "clean-worktrees"))
  (def worktree (string repo "-dirty"))
  (git* repo "worktree" "add" "-q" "-b" "1-dirty" worktree)
  (spit (string worktree "/file.txt") "leftover")
  (test-stdout (clean_repos/clean-worktrees repo) `
    git STASHED
    git REMOVE worktree /private/tmp/judge-scripts/clean-worktrees/clone-dirty
  `)
  (test (os/stat worktree) nil)
  (test (string/has-prefix? "stash@{0}: On 1-dirty: Leftover changes from /private/tmp/judge-scripts/clean-worktrees/clone-dirty @ "
                            ($<_ git -C ,repo stash list))
    true))

(deftest "clean-repo"
  (def repo (git-repo "clean-repo"))
  (git* repo "branch" "1-merged")
  (test-stdout (clean_repos/clean-repo repo) `
    
    Cleaning /private/tmp/judge-scripts/clean-repo/clone ... 
    git FETCH /private/tmp/judge-scripts/clean-repo/clone
    git REMOVE branch 1-merged
    DONE
  `)
  (test-stdout (clean_repos/clean-repo root) `
    
    Cleaning /private/tmp/judge-scripts ... 
    SKIP not a git directory: /private/tmp/judge-scripts
  `))

(deftest "main"
  (test-stdout (clean_repos/main "clean_repos" root root) `
    
    Cleaning /private/tmp/judge-scripts ... 
    SKIP not a git directory: /private/tmp/judge-scripts
    
    Cleaning /private/tmp/judge-scripts ... 
    SKIP not a git directory: /private/tmp/judge-scripts
  `))
