cat $1 print_regions.ml | /imandra/imandra
mv temp.json static/$(basename $1).json
