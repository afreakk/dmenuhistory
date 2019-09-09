#!/bin/bash
project=$(locate -r '\.git$'| sed 's/....$//' | dmenuhist ~/.cache/gitprojects fzf)
if test -z "$project" 
then
    echo "Nothing selected"
else
    cd $project
    vim .
fi
