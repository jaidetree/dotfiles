function __theme_mode_prompt -S --description 'Display the default mode for the prompt'
    if test "$fish_key_bindings" = "fish_vi_key_bindings"
        or test "$fish_key_bindings" = "fish_hybrid_key_bindings"
        switch $fish_bind_mode
            case default
                # set_color --bold --background red white
                __theme_right_segment black cyan $pink ' N '
            case insert
                # set_color --bold --background green white
                # echo '[I]'
                __theme_right_segment cyan black $pink ' I '
            case replace_one
                # set_color --bold --background green white
                # echo '[R]'
                __theme_right_segment cyan black $pink ' R '
            case visual
                # set_color --bold --background magenta white
                # echo '[V]'
                __theme_right_segment black yellow $pink ' V '
        end
        set_color normal
    end
end
