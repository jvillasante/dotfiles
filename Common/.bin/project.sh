#!/bin/bash

. "$(dirname "$0")/common.sh"
CURRENT_HOST=$(find_host)

create_cpp_project_clang() {
    read -r -p "Enter project name: " PROJECT_NAME
    read -r -p "Enter project path (default to ~/Workspace/Personal/cpp/): " PROJECT_PATH
    if [ -z "$PROJECT_PATH" ]; then
        PROJECT_PATH="$HOME/Workspace/Personal/cpp/$PROJECT_NAME"
    else
        if [ ! -d "$PROJECT_PATH" ]; then
            echo ">>> $PROJECT_PATH does not exists, exiting..."
            exit 1
        fi
        PROJECT_PATH="$PROJECT_PATH/$PROJECT_NAME"
    fi

    local DOTFILES_DIR="$(find_dotfiles)"
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
        if [ -d "$(DOTFILES_DIR)/Hosts/$CURRENT_HOST" ]; then
            cp "${DOTFILES_DIR}/Hosts/$CURRENT_HOST/.clang-format" "$PROJECT_PATH/"
        fi

        echo ">>> Copying CMakeLists.txt to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/CMakeLists.clang.txt" "$PROJECT_PATH/CMakeLists.txt"
        check $?

        echo ">>> Copying compile_flags.txt to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/compile_flags.clang.txt" "$PROJECT_PATH/compile_flags.txt"
        check $?

        echo ">>> Copying .clang-tidy to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/.clang-tidy" "$PROJECT_PATH/"
        check $?
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

create_cpp_project_gcc() {
    read -r -p "Enter project name: " PROJECT_NAME
    read -r -p "Enter project path (default to ~/Workspace/Personal/cpp/): " PROJECT_PATH
    if [ -z "$PROJECT_PATH" ]; then
        PROJECT_PATH="$HOME/Workspace/Personal/cpp/$PROJECT_NAME"
    else
        if [ ! -d "$PROJECT_PATH" ]; then
            echo ">>> $PROJECT_PATH does not exists, exiting..."
            exit 1
        fi
        PROJECT_PATH="$PROJECT_PATH/$PROJECT_NAME"
    fi

    local DOTFILES_DIR="$(find_dotfiles)"
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
        if [ -d "$(DOTFILES_DIR)/Hosts/$CURRENT_HOST" ]; then
            cp "${DOTFILES_DIR}/Hosts/$CURRENT_HOST/.clang-format" "$PROJECT_PATH/"
        fi

        echo ">>> Copying CMakeLists.txt to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/CMakeLists.gcc.txt" "$PROJECT_PATH/CMakeLists.txt"
        check $?

        echo ">>> Copying compile_flags.txt to $PROJECT_PATH..."
        cp "${DOTFILES_DIR}/Misc/compile_flags.gcc.txt" "$PROJECT_PATH/compile_flags.txt"
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
    read -r -p "Enter project path (default to ~/Workspace/Personal/rust/): " PROJECT_PATH
    if [ -z "$PROJECT_PATH" ]; then
        PROJECT_PATH="$HOME/Workspace/Personal/rust/$PROJECT_NAME"
    else
        if [ ! -d "$PROJECT_PATH" ]; then
            echo ">>> $PROJECT_PATH does not exists, exiting..."
            exit 1
        fi
        PROJECT_PATH="$PROJECT_PATH/$PROJECT_NAME"
    fi

    local DOTFILES_DIR="$(find_dotfiles)"
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
    read -r -p "Enter project path (default to ~/Workspace/Personal/games/sfml/): " PROJECT_PATH
    if [ -z "$PROJECT_PATH" ]; then
        PROJECT_PATH="$HOME/Workspace/Personal/games/sfml/$PROJECT_NAME"
    else
        if [ ! -d "$PROJECT_PATH" ]; then
            echo ">>> $PROJECT_PATH does not exists, exiting..."
            exit 1
        fi
        PROJECT_PATH="$PROJECT_PATH/$PROJECT_NAME"
    fi

    local DOTFILES_DIR="$(find_dotfiles)"
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
        if [ -d "$(DOTFILES_DIR)/Hosts/$CURRENT_HOST" ]; then
            cp "${DOTFILES_DIR}/Hosts/$CURRENT_HOST/.clang-format" "$PROJECT_PATH/"
        fi
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Create C++ Project (clang)" "Create C++ Project (gcc)" "Create Rust Project" "Create SFML Project" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                create_cpp_project_clang
                break
                ;;
            2)
                create_cpp_project_gcc
                break
                ;;
            3)
                create_rust_project
                break
                ;;
            4)
                create_sfml_project
                break
                ;;
            5) break 2 ;;
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
