compile: 
	erl -make
clean:
	rm -rf ebin/*.beam
test:
	erl -pa "ebin" -noshell -eval "ecache:test()" -s init stop
