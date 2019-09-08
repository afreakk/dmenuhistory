#### dmenu history caching, shows most frequent selected fist
### Works with `dmenu`, `fzf`, `rofi -dmenu` etc.
## WIP

# Example usages

Show all git projects using locate, select using fzf, cache history to ~/cache/gitprojects, cd to directory and open in vim
```
#!/bin/bash
project=$(locate -r '\.git$'| sed 's/....$//' | ./dmenuhist ~/.cache/gitprojects fzf)
cd $project
vim .
```

open some file in ~/.config with vim, and save history to ~/.cache/configfilecache so frequently opened will be on the top.
```
find ~/.config | dmenuhist ~/.cache/configfilecachex rofi -dmenu | xargs -o vim
```
