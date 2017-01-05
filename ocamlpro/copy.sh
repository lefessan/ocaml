#!/bin/sh

# Same as "cp -f $src $dst" but adds a location line on top of the file

src=$1
dst=$2

rm -f $2
echo '# 2 "'$1'"' > $2
cat $1 >> $2
chmod -w $2
