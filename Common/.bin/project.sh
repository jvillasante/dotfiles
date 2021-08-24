#!/bin/bash

. "$(dirname "$0")/common.sh"
CURRENT_ENV=$(find_env)

create_cpp_project() {
    read -r -p "Enter project name: " PROJECT_NAME
    local DOTFILES_DIR="$(find_dotfiles)"
    local PROJECT_PATH="$HOME/Workspace/Code/cpp/$PROJECT_NAME"

    if [ ! -d "$PROJECT_PATH" ]; then
        echo ">>> $PROJECT_PATH does not exists, creating it..."
        mkdir -p "$PROJECT_PATH"
        check $?

        echo ">>> Copying project template to $PROJECT_PATH..."
        cp -a "${DOTFILES_DIR}/Misc/templates/cpp_project_template/." "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .editorconfig to $PROJECT_PATH..."
        cp "${HOME}/.editorconfig" "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .clang-format to $PROJECT_PATH..."
        if [ "$CURRENT_ENV" = "PERSONAL" ]; then
            cp "${DOTFILES_DIR}/Personal/.clang-format" "$PROJECT_PATH/"
        elif [ "$CURRENT_ENV" = "WORK" ]; then
            cp "${DOTFILES_DIR}/Work/.clang-format" "$PROJECT_PATH/"
        fi

        echo ">>> Copying compile_flags.txt to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/compile_flags.txt" "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .clang-tidy to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/.clang-tidy" "$PROJECT_PATH/"
        check $?
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

create_rust_project() {
    read -r -p "Enter project name: " PROJECT_NAME
    local DOTFILES_DIR="$(find_dotfiles)"
    local PROJECT_PATH="$HOME/Workspace/Code/rust/$PROJECT_NAME"

    if [ ! -d "$PROJECT_PATH" ]; then
        echo ">>> $PROJECT_PATH does not exists, creating it..."
        mkdir -p "$PROJECT_PATH"
        check $?

        echo ">>> Copying project template to $PROJECT_PATH..."
        cp -a "${DOTFILES_DIR}/Misc/templates/rust_project_template/." "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .editorconfig to $PROJECT_PATH..."
        cp "${HOME}/.editorconfig" "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .rustfmt.toml to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/.rustfmt.toml" "$PROJECT_PATH/"
        check $?
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

create_sfml_project() {
    read -r -p "Enter project name: " PROJECT_NAME
    local DOTFILES_DIR="$(find_dotfiles)"
    local PROJECT_PATH="$HOME/Workspace/Code/games/sfml/$PROJECT_NAME"

    if [ ! -d "$PROJECT_PATH" ]; then
        echo ">>> $PROJECT_PATH does not exists, creating it..."
        mkdir -p "$PROJECT_PATH"
        check $?

        echo ">>> Copying project template to $PROJECT_PATH..."
        cp -a "${DOTFILES_DIR}/Misc/templates/sfml_project_template/." "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .editorconfig to $PROJECT_PATH..."
        cp "${HOME}/.editorconfig" "$PROJECT_PATH/"
        check $?

        echo ">>> Copying .clang-format to $PROJECT_PATH..."
        if [ "$CURRENT_ENV" = "PERSONAL" ]; then
            cp "${DOTFILES_DIR}/Personal/.clang-format" "$PROJECT_PATH/"
        elif [ "$CURRENT_ENV" = "WORK" ]; then
            cp "${DOTFILES_DIR}/Work/.clang-format" "$PROJECT_PATH/"
        fi
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Create C++ Project" "Create Rust Project" "Create SFML Project" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                create_cpp_project
                break
                ;;
            2)
                create_rust_project
                break
                ;;
            3)
                create_sfml_project
                break
                ;;
            4) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done

    echo ""

    if ask "Are we done?"; then
        break
    else
        echo ""
    fi
done
