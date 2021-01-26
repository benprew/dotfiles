# Tips and Tricks

## Git

### Create a worktree from an existing branch

    git worktree add -b <branch> ../<dir> origin/</branch>

Similarly, you can also create a new branch from the tip of an existing branch.  Here I wanted to create a new branch from Chris' branch so I could get my changes reviewed before merging them back into his branch.

    git worktree add -b personal/bprew/deeproot ../deeproot personal/cgoughnour/deeproot-automation
