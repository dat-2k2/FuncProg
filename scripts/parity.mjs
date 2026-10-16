// Checks the JavaScript ports in web/assets/haskell against the real
// Haskell assignments, evaluated by GHC (`ghc -e`). Needs `ghc` on PATH.
//   node scripts/parity.mjs
import { execFileSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
import assert from 'node:assert/strict';
import { hilbert } from '../web/assets/haskell/hilbert.js';
import { solutions } from '../web/assets/haskell/rooks.js';
import { choosePlayer, runGame, humanStep, newGame } from '../web/assets/haskell/trust.js';

const root = path.dirname(path.dirname(fileURLToPath(import.meta.url)));

// Evaluate each expression in the context of a Haskell file; one output line per expression.
function ghcEval(file, exprs) {
  const args = exprs.flatMap((e) => ['-e', e]).concat(path.join(root, file));
  const out = execFileSync('ghc', args, { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'], maxBuffer: 1 << 28 });
  const lines = out.trim().split('\n');
  assert.equal(lines.length, exprs.length, `expected ${exprs.length} result lines from ${file}`);
  // Haskell tuples (a,b) become JSON arrays [a,b].
  return lines.map((l) => JSON.parse(l.replaceAll('(', '[').replaceAll(')', ']')));
}

let checks = 0;
function same(actual, expected, label) {
  assert.deepEqual(actual, expected, label);
  checks++;
}

// Assignment 1: hilbert n
const orders = [1, 2, 3, 4, 5, 6, 7];
ghcEval('Assignment1/Assign1.hs', orders.map((n) => `hilbert ${n}`))
  .forEach((hs, i) => same(hilbert(orders[i]), hs, `hilbert ${orders[i]}`));

// Assignment 2: rooks n and diffrooks n, after main's length filter
const sizes = [1, 2, 3, 4, 5, 6];
const rookExpr = (tree, n) => `filter (\\x -> length x == ${n}) (allPaths (${tree} ${n}))`;
ghcEval('Assignment2/Assign2.hs', sizes.flatMap((n) => [rookExpr('rooks', n), rookExpr('diffrooks', n)]))
  .forEach((hs, i) => {
    const n = sizes[i >> 1], diag = i % 2 === 1;
    same(solutions(n, diag), hs, `${diag ? 'diffrooks' : 'rooks'} ${n}`);
  });

// Assignment 3: every pairing of strategies, plus a human-vs-bot sequence
const pairs = [];
for (const a of [0, 1, 2]) for (const b of [0, 1, 2]) for (const r of [1, 7, 20]) pairs.push([a, b, r]);
const humanMoves = [1, 0, 1, 1, 0, 0, 1];
const humanExpr = (bot) =>
  `let bot = choosePlayer '${bot}'; step g u = addMoves g (u, makeMove bot (snd $ mode g) ` +
  `(if not (null (logGame g)) then snd $ last $ logGame g else coopMove bot 1)); ` +
  `g = foldl step (Game ${humanMoves.length} (True,True) [] (0,0)) [${humanMoves}] in (logGame g, points g)`;
ghcEval('Assignment3/assignment3.hs', [
  ...pairs.map(([a, b, r]) =>
    `let g = runGame (Game ${r} (True,True) [] (0,0)) (choosePlayer '${a}') (choosePlayer '${b}') in (logGame g, points g)`),
  ...[0, 1, 2].map(humanExpr),
]).forEach((hs, i) => {
  if (i < pairs.length) {
    const [a, b, r] = pairs[i];
    const g = runGame(r, choosePlayer(a), choosePlayer(b));
    same([g.logGame, g.points], hs, `runGame ${r} ${a} vs ${b}`);
  } else {
    const bot = choosePlayer(i - pairs.length);
    const g = humanMoves.reduce((acc, u) => humanStep(acc, bot, u), newGame(humanMoves.length));
    same([g.logGame, g.points], hs, `human vs ${bot.name}`);
  }
});

console.log(`parity: ${checks} checks match GHC`);
