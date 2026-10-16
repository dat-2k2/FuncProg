// Assembles the static site into dist/: the web/ folder plus the Haskell
// sources the code panes display. No dependencies, no bundling.
//   node scripts/build.mjs
import { cpSync, mkdirSync, rmSync, readdirSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import path from 'node:path';

const root = path.dirname(path.dirname(fileURLToPath(import.meta.url)));
const dist = path.join(root, 'dist');

rmSync(dist, { recursive: true, force: true });
cpSync(path.join(root, 'web'), dist, { recursive: true });

for (const dir of ['Assignment1', 'Assignment2', 'Assignment3']) {
  for (const file of readdirSync(path.join(root, dir)).filter((f) => f.endsWith('.hs'))) {
    mkdirSync(path.join(dist, 'src', dir), { recursive: true });
    cpSync(path.join(root, dir, file), path.join(dist, 'src', dir, file));
  }
}
console.log('built dist/');
