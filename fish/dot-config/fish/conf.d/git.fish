set -g git_non_feature_branches development develop dev master main

function git_run --argument-names repo_path --description "Run git command in repo path"
    set -l git_command git -C $repo_path $argv[2..]
    if test "$DEBUG" = true
        print_info "$git_command" >&2
    end
    $git_command
end

function git_is_non_feature_branch --argument-names branch --description "True if branch name is a non-feature branch, e.g. main"
    contains -- (string lower (string trim -- $branch)) $git_non_feature_branches
end

function git_is_feature_branch --argument-names branch --description "True if branch name is a feature branch"
    not git_is_non_feature_branch $branch
end

function git_is_git_dir --argument-names repo_path --description "True if path is inside a git repo"
    git_run $repo_path rev-parse --git-dir >/dev/null 2>&1
end

function git_current_branch --argument-names repo_path --description "Name of the checked out branch"
    git_run $repo_path branch --show-current | string trim
end

function git_on_feature_branch --argument-names repo_path --description "True if repo is on a feature branch"
    git_is_feature_branch (git_current_branch $repo_path)
end

function git_fetch --argument-names repo_path --description "Fetch and prune from remotes"
    git_run $repo_path fetch -pPq
end

function git_stash --argument-names repo_path --description "Stash changes, including untracked. True if stashed"
    set -l output (git_run $repo_path stash push --include-untracked $argv[2..])
    test "$output" != "No local changes to save"
end

function git_stash_with_message --argument-names repo_path message --description "Stash changes with a message"
    git_stash $repo_path --message $message
end

function git_pop_stash --argument-names repo_path --description "Pop latest stash"
    git_run $repo_path stash pop -q $argv[2..]
    set -l pop_status $status
    if test $pop_status -ne 0
        print_error "FAILED git STASH-POP, changes kept in stash"
    end
    return $pop_status
end

function git_fast_forward --argument-names repo_path --description "Fast-forward current branch from origin"
    git_run $repo_path pull -q --ff-only origin (git_current_branch $repo_path)
end

function git_fast_forward_with_stash --argument-names repo_path --description "Stash, fast-forward repo, and then pop stash"
    git_stash_with_message $repo_path (date +%s)" sync save"
    set -l stash_status $status
    git_fast_forward $repo_path
    set -l fast_forward_status $status
    if test $stash_status -eq 0
        git_pop_stash $repo_path; or return 1
    end
    return $fast_forward_status
end
