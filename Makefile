all: parser ts

ts:
	npx tsc

parser:
	npx jison pg.jison pg.jisonlex