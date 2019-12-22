const pg = require('./pg')

pg.parser.yy = {
  config: require('./config'),
  nodes: require('./nodes'),
  extra: {
    xcdepth: 0
  }
}

console.log(pg.main(process.argv.slice(1)))