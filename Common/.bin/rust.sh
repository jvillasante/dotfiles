#!/bin/bash

. $(dirname "$0")/common.sh

update_rust() {
    if hash rustup 2>/dev/null; then
        rustup self update
        check $?

        rustup update
        check $?
    fi
}

update_rust_analyzer() {
    if [ ! -d ${HOME}/.bin/bin/ ]; then
        mkdir ${HOME}/.bin/bin/
    fi

    local CURRENT_OS=$(find_os)
    if [ $CURRENT_OS = "OSX" ]; then
        curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-mac -o ${HOME}/.bin/bin/rust-analyzer
    elif [ $CURRENT_OS = "LINUX" ]; then
        curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ${HOME}/.bin/bin/rust-analyzer
    fi

    if hash ${HOME}/.bin/bin/rust-analyzer 2>/dev/null; then
        chmod +x ${HOME}/.bin/bin/rust-analyzer
    fi
}

update_rust_and_analyzer( ) {
    update_rust
    update_rust_analyzer
}

show_global_packages() {
    if hash cargo 2>/dev/null; then
        echo "Global Packages Installed:"
        cargo install-update -al
        check $?

        echo "Run the following to update:
        $ cargo install-update -a                # Update all installed packages.
        $ cargo install-update crate1 crate2 ... # check for newer versions and update selected packages, will not install new packages."
    fi
}

update_global_packages() {
    if hash cargo 2>/dev/null; then
        cargo install-update -a
        check $?
    fi
}

create_project() {
    read -p "Enter project name: " PROJECT_NAME
    local DOTFILES_DIR=$(find_dotfiles)
    local PROJECT_PATH="$HOME/Workspace/Code/rust/$PROJECT_NAME"

    if [ ! -d $PROJECT_PATH ]; then
        echo ">>> $PROJECT_PATH does not exists, creating it..."
        mkdir -p $PROJECT_PATH
        check $?

        echo ">>> Copying project template to $PROJECT_PATH..."
        cp -a ${HOME}/.bin/templates/rust_project_template/. $PROJECT_PATH/
        check $?

        echo ">>> Copying .editorconfig to $PROJECT_PATH..."
        cp ${HOME}/.editorconfig $PROJECT_PATH/
        check $?

        echo ">>> Copying .rustfmt.toml to $PROJECT_PATH..."
        cp ${HOME}/.rustfmt.toml $PROJECT_PATH/
        check $?
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Update Rust" "Update Rust Analyzer" "Update Rust and Rust Analyzer"
             "Show Global Packages Installed" "Update Global Packages Installed"
             "Create Rust Project" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1) update_rust; break ;;
            2) update_rust_analyzer; break ;;
            3) update_rust_and_analyzer; break ;;
            4) show_global_packages; break ;;
            5) update_global_packages; break ;;
            6) create_project; break ;;
            7) break 2 ;;
            *) echo "Invalid option!" >&2
        esac
    done

    echo ""

    if ask "Are we done?"; then
        break
    else
        echo ""
    fi
done

