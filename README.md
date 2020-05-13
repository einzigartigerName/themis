# Themis - a todo list for your terminal

![Screenshot](screenshot.jpeg)

## Install
```shell
git clone https://github.com/einzigartigername/themis.git
cd themis
cabal install
```

## Usage
Themis will look in the current folder for a `todo.md` file. If not found, it will use/create one in your home directory.

### Arguments:
`todo [ARGUMENT] [OPTION]`
* `-c`: use current directory
* `-d`: use home directory
* `-f FILE`: use FILE
* `-h`: display help dialog

If file does not exist, `todo` will create one.

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