PATH := node_modules/.bin/:$(PATH)

.PHONY: watch
watch:
	elm live src/Main.elm

dist: dist/app.min.js dist/index.html

dist/index.html: src/index.html
	cp src/index.html dist/index.html

dist/app.min.js: dist/app.js
	uglifyjs dist/app.js -o dist/app.min.js

dist/app.js: src/Main.elm
	mkdir -p dist/
	elm-make --yes src/Main.elm --output dist/app.js

clean:
	rm -rf dist/
