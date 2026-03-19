#!/usr/bin/env bash

# Create a new git worktree for nntp and copy personal config files into it.
#
# Usage: worktree-add.sh <worktree-name> [branch]
#
# Examples:
#   worktree-add.sh fix_auth              # creates worktree on new branch fix_auth
#   worktree-add.sh fix_auth origin/main  # creates worktree from origin/main

set -euo pipefail

DOTFILES_DIR="$HOME/Workspace/Public/dotfiles/Misc/work/nntp"
WORKTREE_BASE="$HOME/Workspace/Work/Omicron/Projects/nntp"

if [[ $# -lt 1 ]]; then
    echo "Usage: $(basename "$0") <worktree-name> [branch]"
    exit 1
fi

WORKTREE_NAME="$1"
shift
WORKTREE_PATH="$WORKTREE_BASE/$WORKTREE_NAME"

# Create the worktree (pass any remaining args like branch name)
git -C "$WORKTREE_BASE/master" worktree add "$WORKTREE_PATH" "$@"

# Symlink personal config files
ln -s "$DOTFILES_DIR/.clang-tidy" "$WORKTREE_PATH/.clang-tidy"
ln -s "$DOTFILES_DIR/.dir-locals.el" "$WORKTREE_PATH/.dir-locals.el"
ln -s "$DOTFILES_DIR/compile_flags.fedora.txt" "$WORKTREE_PATH/compile_flags.txt"
ln -s "$DOTFILES_DIR/.scripts" "$WORKTREE_PATH/.scripts"

echo ""
echo "Worktree created at: $WORKTREE_PATH"
echo "Personal config files symlinked."
