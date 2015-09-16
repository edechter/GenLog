#!/bin/bash -ex

cd ./src

CPATH="$HOME"/Dropbox/Projects/swipl-all/swipl-devel/include/
LIBRARY_PATH="$HOME"/Dropbox/Projects/swipl-all/swipl-devel/lib/x86_64-darwin13.2.0/

mkdir -p ../lib 

gcc -I"$CPATH" \
   -L"$LIBRARY_PATH" \
   -fPIC -c -v digamma.c 

gcc -v -shared -W1 -o ../lib/digamma.dylib digamma.o \
   -L/"$LIBRARY_PATH" \
   -lswipl

gcc -I"$CPATH" \
   -L"$LIBRARY_PATH" \
   -fPIC -c -v multinom.c 

gcc -v -shared -W1 -o ../lib/multinom.dylib multinom.o \
   -L/"$LIBRARY_PATH" \
   -lswipl

cd -
