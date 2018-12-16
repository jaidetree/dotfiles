function fish_prompt
  set -g _last_status $status
  set -l branch (__fish_git_prompt "%s")
  set -l _is_git_dirty (git status -s --ignore-submodules=dirty ^/dev/null)

  function __git_status
    set status_list (git status --porcelain | cut -c 1-2 | string trim | uniq)

    if contains "." $status_list
      or contains "D" $status_list
      or contains "M" $status_list
      or contains "R" $status_list
      echo (set_color yellow)"changed"(set_color normal)
    end

    if contains "??" $status_list
      echo (set_color ff5faf)"new"(set_color normal)
    end

    if contains "U" $status_list
      echo (set_color red)"conflict"(set_color normal)
    end
  end

  set -l pink ff00ff

  set_color yellow
  printf '%s' (whoami)
  set_color normal
  printf ' at '

  set_color magenta
  echo -n (prompt_hostname)
  set_color normal
  printf ' in '

  set_color brgreen
  printf '%s' (prompt_pwd)
  set_color normal

  if test -n $branch
    printf ' on '

    set_color cyan
    printf '%s' $branch
    set_color normal
    if test -n "$_is_git_dirty"
      printf ' with '
      echo -n (__git_status | string join ", " | string trim -r -c ", ")
      printf ' files'
    end
  end

  # Line 2
  echo
  fish_right_block black $pink normal " ➜ "
  printf " "
end
