import pg from './parser/gram';
import parseFn from './parser/parse';
import config from './config';
import { keywordlist } from './parser/kwlist';

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

export default pg;