function wta;
    git worktree add ../$argv[1];
end

function wtd;
    git worktree remove $argv[1];
end

alias wtl 'git worktree list'
