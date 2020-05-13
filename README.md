# Themis
TUI todo list

![Screenshot](screenshot.jpeg)
## Install
```shell
git clone https://github.com/einzigartigername/themis.git
cd themis
cabal install
```

## Usage
Themis will first look in the current folder for a `todo.md` file, if not found, it will go look in your home directory and crate one, if non found.

### Arguments:
`todo [ARGUMENT] [OPTION]`
* `-c`: use current directory
* `-d`: use home directory
* `-f FILE`: use specified file
* `-h`: display help dialog

If file does not exist, themis will create one.

### Keybinds
* `Up` or `k` - Up
* `Down` or `j` - Down
* Movement Keys + `Shift` - move selected item up/down in list
* `i` - insert above selected item
* `a` - insert below selected item
* Insert Keys + `Shift` - insert item at Top/Bottom of list
* `e` - edit selected item
* `c` - toggle check
* `d` - delete selected item
* `q` or `Esc` - quit application

In Item Editor: 
* `Esc` - quit edit mode (changes not saved)
* `Enter` - save changes / insert item