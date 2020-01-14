all: ts parser

ts:
	npx ttsc --project tsconfig.json

parser:
	node ./build.js
	cp ./src/parser/parse.js ./dist/parser/parse.js
	cp ./src/parser/gram.js ./dist/parser/gram.js