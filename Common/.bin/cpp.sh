#!/bin/bash

. $(dirname "$0")/common.sh

create_project() {
    read -p "Enter project name: " PROJECT_NAME
    local DOTFILES_DIR=$(find_dotfiles)
    local PROJECT_PATH="$HOME/Workspace/Code/cpp/$PROJECT_NAME"

    if [ ! -d $PROJECT_PATH ]; then
        echo ">>> $PROJECT_PATH does not exists, creating it..."
        mkdir -p $PROJECT_PATH
        check $?

        echo ">>> Copying project template to $PROJECT_PATH..."
        if [ "$1" = make ]; then
            cp -a ${HOME}/.bin/templates/cpp_project_template_make/. $PROJECT_PATH/
        else
            cp -a ${HOME}/.bin/templates/cpp_project_template_cmake/. $PROJECT_PATH/
        fi
        check $?

        echo ">>> Copying .editorconfig to $PROJECT_PATH..."
        cp ${HOME}/.editorconfig $PROJECT_PATH/
        check $?

        echo ">>> Copying .clang-format to $PROJECT_PATH..."
        cp ${HOME}/.clang-format $PROJECT_PATH/
        check $?

        echo ">>> Copying compile_flags.txt to $PROJECT_PATH..."
        cp ${DOTFILES_DIR}/Common/compile_flags.txt $PROJECT_PATH/
        check $?

        echo ">>> Copying .clang-tidy to $PROJECT_PATH..."
        cp ${HOME}/.clang-tidy $PROJECT_PATH/
        check $?
    else
        echo ">>> $PROJECT_PATH already exists."
    fi
}

create_cmake_project() {
    create_project "cmake"
}

create_make_project() {
    create_project "make"
}

while true; do
    PS3="Choose an option: "
    options=("Create Cmake Project" "Create Make Project" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1) create_cmake_project; break ;;
            2) create_make_project; break ;;
            3) break 2 ;;
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
