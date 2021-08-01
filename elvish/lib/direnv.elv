## hook for direnv
@edit:before-readline = $@edit:before-readline {
	try {
		m = [("/usr/local/bin/direnv" export elvish | from-json)]
		if (> (count $m) 0) {
			m = (all $m)
			keys $m | each [k]{
				if $m[$k] {
					set-env $k $m[$k]
				} else {
					unset-env $k
				}
			}
		}
	} except e {
		echo $e
	}
}
