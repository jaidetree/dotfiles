function __theme_left_segment -S -a fg bg next_bg
  set content $argv[4..-1]

  set_color -b $next_bg $bg
  echo -n $left_solid_arrow_glyph
  set_color normal
  set_color -b $bg $fg
  echo -n $content
  set_color normal
end
