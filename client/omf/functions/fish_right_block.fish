function fish_right_block
  set fg $argv[1]
  set bg $argv[2]
  set next $argv[3]
  set content $argv[4..-1]

  set_color normal
  set_color -b $bg $fg
  echo -n $content
  set_color -b $next $bg
  printf ""
  set_color normal
end
