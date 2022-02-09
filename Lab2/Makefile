all: *.erl lib/*.erl
	make -C lib
	erl -compile *.erl lib/*.erl

clean:
	rm -f *.beam *.dump

run_tests: all
	
