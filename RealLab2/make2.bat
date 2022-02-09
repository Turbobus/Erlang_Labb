erl -make
move *.beam ebin
cd lib
erl -make
move "*.beam" "../ebin"
cd ..
cd ebin
erl