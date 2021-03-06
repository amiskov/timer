watch:
	ls src/*.elm | entr elm make src/Main.elm --output=static/timer.js
build:
	elm make src/Main.elm --optimize --output=static/timer.js
	node_modules/.bin/uglifyjs static/timer.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" \
	| node_modules/.bin/uglifyjs --mangle -o static/timer.js
deploy:
	sh deploy.sh