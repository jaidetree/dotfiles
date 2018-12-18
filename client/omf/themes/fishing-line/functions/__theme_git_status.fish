# Private functions should be prefixed with __ since they are globally
# accessible.
# Returns a list of colored attributes describing the git state
function __theme_git_status
  set status_list (git status --porcelain | cut -c 1-2 | string trim | uniq)

  # "??" means we have untracked files in the repo
  if contains "??" $status_list
    echo (set_color ff5faf)"new"(set_color normal)
  end
  
  # "A" means we have staged files not yet committed
  if contains "A" $status_list
    echo (set_color brgreen)"staged"(set_color normal)
  end

  # "." means a file was added but is unstaged
  # "d" means a file was deleted but is unstaged
  # "M" means a file was modified but is unstaged
  # "R" means a file was renamed but is unstaged
  # However we just return "changed" so as not to make this line too long
  if contains "." $status_list
    or contains "D" $status_list
    or contains "M" $status_list
    or contains "R" $status_list
    echo (set_color yellow)"changed"(set_color normal)
  end

  # "U" means there were unmerged conflicts in the repo
  if contains "U" $status_list
    echo (set_color red)"conflict"(set_color normal)
  end
end
