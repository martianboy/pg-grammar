import pg from './pg.js';
import fs from 'fs';
// import parser from 'pg-query-parser';
import parseFn from './parse.js';
import config from './config.js';
import { keywordlist } from './src/parser/kwlist.js';

pg.parser.parse = parseFn;

pg.parser.yy = {
  config,
  lval: {
    str: null,
    ival: 0,
    keyword: null
  },
  extra: {
    standard_conforming_strings: true,
    xcdepth: 0,
    keywordlist
  }
};

const argv = process.argv.slice(1);
const input = fs.readFileSync(argv[1], { encoding: 'utf-8' });
fs.writeFileSync('./output/sqljs.json', JSON.stringify(pg.parse(input), null, 2))

// const tree = parser.parse(input)
// fs.writeFileSync('./output/native.json', JSON.stringify(tree, null, 2))