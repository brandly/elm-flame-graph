default: dist

dist: dist/app.min.js dist/index.html

dist/index.html: src/index.html
	cp src/index.html dist/index.html

dist/app.min.js: dist/app.js
	node_modules/.bin/uglifyjs dist/app.js -o dist/app.min.js

dist/app.js: src/Main.elm
	mkdir -p dist/
	node_modules/.bin/elm make src/Main.elm --output dist/app.js --optimize

clean:
	rm -rf dist/
