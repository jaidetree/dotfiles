function __theme_strip_escapes 
  string replace -a -r '\x1b(\[[^@-~]*[@-~]|\(B)' '' $argv
end 