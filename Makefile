all: parser ts

parser: pg.js

ts:
	npx ttsc --project tsconfig.json

pg.js:
	./node_modules/.bin/jison pg.jison pg.jisonlex --module-type es --outfile pg.js
	patch pg.js patches/00-bt.patch
