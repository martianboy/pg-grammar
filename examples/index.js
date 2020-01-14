import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import parser from 'pg-query-parser';
import pg from '../dist/index.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const argv = process.argv.slice(1);
const input = fs.readFileSync(argv[1], { encoding: 'utf-8' });
fs.writeFileSync(path.resolve(__dirname, './output/sqljs.json'), JSON.stringify(pg.parse(input), null, 2));

const tree = parser.parse(input);
fs.writeFileSync(path.resolve(__dirname, './output/native.json'), JSON.stringify(tree, null, 2));