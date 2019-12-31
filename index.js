const pg = require("./pg");
const fs = require('fs');

pg.parser.parse = require('./parse');

pg.parser.yy = {
  nodes: require('./src'),
  config: require("./config"),
  lval: {
    str: null,
    ival: 0,
    keyword: null
  },
  extra: {
    standard_conforming_strings: true,
    xcdepth: 0,
    keywordlist: require('./src/parser/kwlist').keywordlist
  }
};

const argv = process.argv.slice(1);
const input = fs.readFileSync(argv[1], { encoding: 'utf-8' });
console.log(JSON.stringify(pg.parse(input), null, 2));
