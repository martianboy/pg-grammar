import JisonCli from './vendor/jison-gho/cli-es6.js'

JisonCli.main({
    lexfile: 'src/parser/scan.jisonlex',
    file: 'src/parser/gram.jison',
    outfile: 'src/parser/gram.js',
    moduleType: 'es'
})