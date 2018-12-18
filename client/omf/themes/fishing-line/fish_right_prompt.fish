function fish_right_prompt -d "Write out the right prompt"
  # Color the last status as either green if it's 0 meaning the last program
  # exited correctly or red meaning the last program exited with a non-zero
  # status code
  if test $_last_status -eq 0
    set_color 6c6c6c
    printf '[ '
    set_color brgreen
    echo -n $_last_status
    set_color 6c6c6c
    printf ' ]'
  else
    set_color -b red black
    printf ' %s ' $_last_status
  end
  set_color normal

  # Print the current date
  set_color 6c6c6c
  printf ' ['
  set_color normal
  date '+%m.%d.%y '

  # Print the time in a pink color
  set_color ff5faf
  date '+%-I:%M%p'
  set_color 6c6c6c
  printf ']'
  set_color normal
end
