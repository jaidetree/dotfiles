function fish_prompt
  set -g _last_status $status

  function _git_branch_name
    __fish_git_prompt "%s"
  end

  function _is_git_dirty
    echo (git status -s --ignore-submodules=dirty ^/dev/null)
  end

  function _git_status
    if [ (_is_git_dirty) ]
      for i in (git status --porcelain | cut -c 1-2 | uniq)
        switch $i
        case "*[ahead *"
          echo "ahead"
        case "*behind *"
          echo "behind"
        case "."
          echo (set_color green)"adds"(set_color normal)
        case " D"
          echo (set_color red)"deletes"(set_color normal)
        case "*M*"
          echo (set_color yellow)"changes"(set_color normal)
        case "*R*"
          echo (set_color 800080)"renames"(set_color normal)
        case "*U*"
          echo "conflicts"
        case "??"
          echo (set_color ff875f)"new"(set_color normal)
        end
      end
    else
      echo "good"
    end
  end

  function _contains
    contains $argv[1] $argv[2..-1]
    if test $status -eq 0
      true
    else
      false
    end
  end

  function _git_status_2
    set status_list (git status --porcelain | cut -c 1-2 | string trim | uniq)

    if _contains "??" $status_list
      echo (set_color ff5faf)"adds"(set_color normal)
    #   echo -n (set_color ff875f)"+"(set_color normal)
    # else
    #   echo
    end

    if _contains "." $status_list
      or _contains "D" $status_list
      or _contains "M" $status_list
      or _contains "R" $status_list
      echo (set_color yellow)"changes"(set_color normal)
    end

    if _contains "U" $status_list
      echo (set_color red)"conflicts"(set_color normal)
    end
  end

  set pink ff00ff

  set_color yellow
  printf '%s' (whoami)
  set_color normal
  printf ' at '

  set_color magenta
  echo -n (prompt_hostname)
  set_color normal
  printf ' in '

  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  set branch (__fish_git_prompt "%s")
  if test -n "$branch"
    printf ' on '

    set_color cyan
    printf '%s' $branch
    set_color normal
    printf ' with '
    echo -n (_git_status_2 | string join ", " | string trim -r -c ", ")
  end

  # Line 2
  echo
  fish_right_block black $pink normal " ➜ "
  printf " "
end
