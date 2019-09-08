#!/bin/bash
project=$(locate -r '\.git$'| sed 's/....$//' | ./dmenuhist ~/.cache/gitprojects fzf)
cd $project
vim .
