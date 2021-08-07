# Helpers
##############################################################################

function __theme_git_prompt -S
  printf 'on '

  # Print the branch name
  set_color cyan
  printf '%s' $branch
  set_color normal

  # Turn the list of statuses like "new" "changed"
  if test -n "$_is_git_dirty"
    # Create a local $git_status var to cache the results
    set -l git_status (__theme_git_status)
    set -l status_count (count $git_status)

    printf ' with '
    if test $status_count -gt 1
      echo -n (string join ", " $git_status[1..-2] | string trim -r -c ", ")
    else
      echo -n $git_status[1]
    end

    # If there are more than 2 statuses display it like
    # new, changed, & staged files
    if test $status_count -gt 2
      printf ','
    end

    # If there are more than 1 statuses put a & the second-to-last and last
    # like new & changed
    if test $status_count -gt 1
      printf ' & '
      echo -n $git_status[-1]
    end

    printf ' files'
  end
end

# Apply Theme
##############################################################################

function fish_prompt
  set -g _last_status $status
  set -l branch (__fish_git_prompt "%s")
  set -l _is_git_dirty (git status -s --ignore-submodules=dirty 2>/dev/null)

  __theme_glyphs

  # Going forward we first start out by appending to a mutable prompt string

  set -l pink ff00ff

  # Add current user to prompt
  set -l user (whoami)
  set -l prompt (set_color yellow)"$user"(set_color normal)

  # Add hostname to prompt
  set -l host (prompt_hostname)
  set -l prompt "$prompt at "(set_color magenta)$host(set_color normal)

  # Add shortened pwd to prompt
  set -l pwd (prompt_pwd)
  set -l prompt "$prompt in "(set_color brgreen)"$pwd"(set_color normal)


  # Calculate the length of the plain text chars of the prompt
  set -l prompt_length (string length (__theme_strip_escapes $prompt))
  # Set some mutable vars we can conditionally update if within a git repo
  set -l git_prompt_length 0
  set -l git_status_prompt ""

  # Update the mutable vars with git status info
  if test -n "$branch"
    set git_status_prompt (__theme_git_prompt)
    set git_prompt_length (string length (__theme_strip_escapes $git_status_prompt))
  end

  # Display the first part of the prompt with the general info
  echo -n $prompt

  # Test to make sure the length of prompt chars is less than total columns
  # Uses the __theme_strip_escapes so that the color escape sequences are not
  # counted against the length.
  # If it passes it puts the git status on a separate line.
  if test -n "$branch" -a (math $prompt_length + $git_prompt_length + 1) -gt $COLUMNS
    echo
    printf ' %s%s%s' (set_color a8a8a8) $sub_glyph (set_color normal)
  else if test -n "$branch"
    printf ' '
  end

  # Display the git prompt
  if test -n "$branch"
    echo -n $git_status_prompt
  end

  # Line 2
  # Use the fish_right_block fn to display the prompt char in one of those
  # trendy triangle blocky things.
  #
  # Be sure to set your iTerm ANSI char font to a Powerline font
  # https://github.com/powerline/fonts
  echo
  __theme_mode_prompt
  printf (set_color $pink)" >"(set_color normal)
  printf " "
end
