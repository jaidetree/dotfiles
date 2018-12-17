function fish_prompt
  set -g _last_status $status
  set -l branch (__fish_git_prompt "%s")
  set -l _is_git_dirty (git status -s --ignore-submodules=dirty ^/dev/null)

  function __git_status
    set status_list (git status --porcelain | cut -c 1-2 | string trim | uniq)

    if contains "??" $status_list
      echo (set_color ff5faf)"new"(set_color normal)
    end

    if contains "." $status_list
      or contains "D" $status_list
      or contains "M" $status_list
      or contains "R" $status_list
      echo (set_color yellow)"changed"(set_color normal)
    end

    if contains "U" $status_list
      echo (set_color red)"conflict"(set_color normal)
    end
  end

  set -l pink ff00ff
  set -l prompt ""

  set -l user (whoami)
  set -l prompt (set_color yellow)"$prompt$user"(set_color normal)
  
  set -l host (prompt_hostname)
  set -l prompt "$prompt at "(set_color magenta)$host(set_color normal)

  set -l pwd (prompt_pwd)
  set -l prompt "$prompt in "(set_color brgreen)$pwd(set_color normal)
  
  echo -n $prompt
  if test (string length $prompt) -gt $COLUMNS
    echo 
    printf '   '
  end

  if test -n "$branch"
    printf ' on '

    set_color cyan
    printf '%s' $branch
    set_color normal
    if test -n "$_is_git_dirty"
      set -l git_status (__git_status)
      printf ' with '
      echo -n  (string join ", " $git_status[1..-2] | string trim -r -c ", ")
      if test (count $git_status) -gt 2
        printf ','
      end
      if test (count $git_status) -gt 1
        printf ' & '
        echo -n $git_status[-1]
      end
      printf ' files'
    end
  end

  # Line 2
  echo
  fish_right_block black $pink normal " ➜ "
  printf " "
end
