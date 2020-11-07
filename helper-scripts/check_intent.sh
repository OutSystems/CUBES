#!/bin/sh
#cat analysis/intent.csv | sed "s/^[0-9]*;//g" | sed "s/;//g" | xargs -I % sh -c "vim -O analysis/data/$1/%.log +$ tests-examples/%.yaml"
cat analysis/intent.csv | sed "s/^[0-9]*;//g" | sed "s/;//g" | xargs -I % sh -c "vim -O analysis/data/$1/%_0.log +$ tests-examples/%.yaml"
