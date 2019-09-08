# dmenu history caching, shows most frequently selected first
## Works with `dmenu`, `fzf`, `rofi -dmenu` etc.
### WIP

#### Insallation
compile and put dmenuhist in path

#### Example usages

##### Show all git projects using locate, select using fzf, cache history to ~/cache/gitprojects, cd to directory and open in vim
```
#!/bin/bash
project=$(locate -r '\.git$'| sed 's/....$//' | dmenuhist ~/.cache/gitprojects fzf)
cd $project
vim .
```

##### Open some file in ~/.config with vim, and save history to ~/.cache/configfilecache so frequently opened will be on the top.
```
find ~/.config -type f | dmenuhist ~/.cache/configfilecachex rofi -dmenu | xargs -o vim
```
