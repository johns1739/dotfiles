(use judge)
(use sh)
(array/push module/paths [":cur:/:all:" :source |(string/has-prefix? "." $)])
(import ./sync_repos)
(use ./utils/fixtures)

(defn push-upstream-commit
  "Push a commit to origin of repo from another clone."
  [repo message]
  (def other (string repo "-other"))
  ($ git clone -q ,(string repo "/../origin.git") ,other)
  (commit other message)
  (git* other "push" "-q"))

(deftest "sync-repo"
  (def repo (git-repo "sync-repo"))
  (push-upstream-commit repo "upstream")
  (test-stdout (sync_repos/sync-repo repo) `
    
    Syncing /private/tmp/judge-scripts/sync-repo/clone ... 
    git FETCH /private/tmp/judge-scripts/sync-repo/clone
    git FAST-FORWARD /private/tmp/judge-scripts/sync-repo/clone
    DONE
  `)
  (test ($<_ git -C ,repo log -1 --format=%s) "upstream"))

(deftest "sync-repo diverged"
  (def repo (git-repo "sync-repo-diverged"))
  (push-upstream-commit repo "upstream")
  (commit repo "local")
  (test-stdout (sync_repos/sync-repo repo) `
    
    Syncing /private/tmp/judge-scripts/sync-repo-diverged/clone ... 
    git FETCH /private/tmp/judge-scripts/sync-repo-diverged/clone
    git FAST-FORWARD /private/tmp/judge-scripts/sync-repo-diverged/clone
    FAILED
  `))

(deftest "sync-repo skips"
  (def repo (git-repo "sync-repo-skips"))
  (git* repo "switch" "-q" "-c" "1-feature")
  (test-stdout (sync_repos/sync-repo repo) `
    
    Syncing /private/tmp/judge-scripts/sync-repo-skips/clone ... 
    SKIP on feature branch: 1-feature
  `)
  (test-stdout (sync_repos/sync-repo root) `
    
    Syncing /private/tmp/judge-scripts ... 
    SKIP not a git directory: /private/tmp/judge-scripts
  `))

(deftest "main"
  (test-stdout (sync_repos/main "sync_repos" root root) `
    
    Syncing /private/tmp/judge-scripts ... 
    SKIP not a git directory: /private/tmp/judge-scripts
    
    Syncing /private/tmp/judge-scripts ... 
    SKIP not a git directory: /private/tmp/judge-scripts
  `))
