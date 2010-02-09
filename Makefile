compile: 
	erl -make
clean:
	rm -rf ebin/*.beam
test:
	erl -pa "ebin" -noshell -eval "ok" -s init stop
