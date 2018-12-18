function __theme_right_segment -S -a fg bg next_bg
  set content $argv[4..-1]

  set_color normal
  set_color -b $bg $fg
  echo -n $content
  set_color -b $next_bg $bg
  echo -n $right_solid_arrow_glyph
end
