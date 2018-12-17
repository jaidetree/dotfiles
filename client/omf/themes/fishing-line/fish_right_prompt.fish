function fish_right_prompt -d "Write out the right prompt"
  if test $_last_status -eq 0
    set_color green
  else
    set_color red
  end
  echo -n $_last_status
  set_color normal

  date '+ %m.%d.%y '

  set_color ff5faf
  date '+%-I:%M%p'
  set_color normal
end
