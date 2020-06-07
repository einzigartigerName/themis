# Configuration
Configure your experience with an [INI](https://en.wikipedia.org/wiki/INI_file) formatted config file.

## Default values
There is a template with all default options at [doc/config](config).

## Colors
Customize your todo-list's theme in the `COLOR` section of the config file. Available Colors:

### Options
Add to all options ether `FG` for foreground or `BG` for background option.
| Option            | Description                   |
| ----------------- | ----------------------------- |
| `item`            | non selected item             |
| `selection`       | selected item                 |
| `file`            | bottom info row               |
| `editor`          | text in editor                |
| `editorBorder`    | border around editor          |
| `editorLable`     | info lable at top of editor   |


### ANSI Colors
This uses your terminals colors
| Color         | Bright Color      |
| ------------- |:----------------- |
| black         | brightBlack       |
| red           | brightRed         |
| green         | brightGreen       |
| yellow        | brightYellow      |
| blue          | brightBlue        |
| magenta       | brightMagenta     |
| cyan          | brightCyan        |
| white         | brightWhite       |

### Pseudo RGB
Instead of the standard ansi colors, support for 240 colors is supported via lossy RGB. Format the color with `#rrggbb`.


## Keybindings
All keybindings can be customized. Default list movement is ether with arrow-keys or vim-keys. These can always be used, when no other binfing is applied via config file.

### Options
| Option            | Description                           |
| ----------------- | ------------------------------------- |
| `insert`          | insert item ABOVE selection           |
| `append`          | insert item BELOW selection           |
| `insertTop`       | insert item at TOP of list            |
| `insertBottom`    | insert item at BOTTOM of list         |
| `edit`            | EDIT selected item                    |
| `delete`          | DELETE seelcted item                  |
| `toggleCheck`     | toggle CHECK status of selected item  |
| `up`              | UP list movement                      |
| `down`            | DOWN list movement                    |
| `moveUp`          | SWAP selection with item above        |
| `moveDown`        | SWAP selection with item below        |
| `help`            | open HELP panel with list of keybinds |
| `quit`            | QUIT application                      |
