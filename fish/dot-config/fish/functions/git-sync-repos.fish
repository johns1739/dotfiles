function __git_sync_repo --argument-names repo_path --description "Fetch and fast-forward a repo, unless on a feature branch"
    if not git_is_git_dir $repo_path
        print_warning "SKIP (not-git-repo): $repo_path"
        return
    end

    if git_on_feature_branch $repo_path
        print_warning "SKIP (on-feat-branch:"(git_current_branch $repo_path)"): $repo_path"
        return
    end

    git_fetch $repo_path

    if git_fast_forward_with_stash $repo_path
        print_success "SYNC: $repo_path"
    else
        print_error "FAILED: $repo_path"
    end
end

function git-sync-repos --description "Sync the repos given as arguments"
    for repo_path in $argv
        __git_sync_repo $repo_path
    end
end
