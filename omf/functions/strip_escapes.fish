function strip_escapes 
  string replace -r '\x1b\[[^@-~]*[@-~]' '' $argv[1]
end 