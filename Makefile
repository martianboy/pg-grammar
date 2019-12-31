all: parser ts

ts:
	npx tsc

parser:
	./node_modules/.bin/jison pg.jison pg.jisonlex