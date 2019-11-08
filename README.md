# dmenu history caching, shows most frequently selected first
## Works with `dmenu`, `fzf`, `rofi -dmenu` etc.
### WIP

#### Installation
Compile `ghc -o dmenuhist dmenuhist.hs` for example, then put `dmenuhist` in path (No special dependencies other than haskell)

#### Example usages

##### Show all Git projects using locate, select using fzf, cache history to ~/.cache/gitprojects, cd to directory and open its file listing in Vim
```
#!/bin/bash
project=$(locate -r '\.git$'| sed 's/....$//' | dmenuhist ~/.cache/gitprojects fzf)
if test -z "$project" 
then
    echo "Nothing selected"
else
    cd $project
    vim .
fi
```

##### Open some file in ~/.config with Vim, save history to ~/.cache/configfilecache and open in Vim.
```
find ~/.config -type f | dmenuhist ~/.cache/configfilecachex rofi -dmenu | xargs -o vim
```
