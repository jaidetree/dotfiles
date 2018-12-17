function fish_prompt
  set -g _last_status $status
  set -l branch (__fish_git_prompt "%s")
  set -l _is_git_dirty (git status -s --ignore-submodules=dirty ^/dev/null)

  # Private functions should be prefixed with __ since they are globally
  # accessible.
  # Returns a list of colored attributes describing the git state
  function __git_status
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
  
  # Going forward we first start out by appending to a mutable prompt string

  set -l pink ff00ff
  set -l prompt ""

  # Add current user to prompt
  set -l user (whoami)
  set -l prompt (set_color yellow)"$prompt$user"(set_color normal)
  
  # Add hostname to prompt
  set -l host (prompt_hostname)
  set -l prompt "$prompt at "(set_color magenta)$host(set_color normal)

  # Add shortened pwd to prompt
  set -l pwd (prompt_pwd)
  set -l prompt "$prompt in "(set_color brgreen)$pwd(set_color normal)
  
  # Display the first part of the prompt with the general info
  echo -n $prompt
  
  # Test to make sure the length of prompt chars is less than total columns
  # minus the VI mode indicator which is 4 chars long. Uses the strip_escapes
  # so that the color escape sequences are not counted against the length.
  # If it passes it puts the git status on a separate line.
  if test (string length (strip_escapes $prompt)) -gt (math $COLUMNS - 4)
    echo 
    printf '   '
  end

  # If a git branch was found in cwd then check its status
  if test -n "$branch"
    printf ' on '
    
    # Print the branch name
    set_color cyan
    printf '%s' $branch
    set_color normal
    
    # Turn the list of statuses like "new" "changed" 
    if test -n "$_is_git_dirty"
      # Create a local $git_status var to cache the results
      set -l git_status (__git_status)
      printf ' with '
      echo -n  (string join ", " $git_status[1..-2] | string trim -r -c ", ")
      
      # If there are more than 2 statuses display it like
      # new, changed, & staged files
      if test (count $git_status) -gt 2
        printf ','
      end
      
      # If there are more than 1 statuses put a & the second-to-last and last
      # like new & changed
      if test (count $git_status) -gt 1
        printf ' & '
        echo -n $git_status[-1]
      end
      
      printf ' files'
    end
  end

  # Line 2
  # Use the fish_right_block fn to display the prompt char in one of those
  # trendy triangle blocky things.
  # 
  # Be sure to set your iTerm ANSI char font to a Powerline font
  # https://github.com/powerline/fonts
  echo
  fish_right_block black $pink normal " ➜ "
  printf " "
end
