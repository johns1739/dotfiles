function print_error --description "Print error message"
    set_color ee3333
    echo "[ERROR] $argv[1]"
    set_color --reset
end

function print_warning --description "Print warning message"
    set_color f1fa8c
    echo "[WARN] $argv[1]"
    set_color --reset
end

function print_success --description "Print success message"
    set_color 50ea7b
    echo "[OK] $argv[1]"
    set_color --reset
end

function print_info --description "Print info message"
    set_color 6272a4
    echo "[INFO] $argv[1]"
    set_color --reset
end
