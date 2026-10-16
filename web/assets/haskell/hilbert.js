// Line-for-line port of the Hilbert functions in Assignment1/Assign1.hs.
// Points are [x, y] pairs, exactly like the Haskell [[Int]].
// scripts/parity.mjs checks these against GHC's output.

// rotr n a = [last (head a), n - 1 - head (head a)] : rotr n (tail a)
export const rotr = (n, a) => a.map(([x, y]) => [y, n - 1 - x]);

// power2 n = 2 * power2 (n-1)
export const power2 = (n) => 2 ** n;

// reflexy n a = [n - 1 - head (last a), last (last a)] : reflexy n (init a)
// (walks the list from the end, so the result is reversed and mirrored)
export const reflexy = (n, a) => [...a].reverse().map(([x, y]) => [n - 1 - x, y]);

// rev a = last a : rev (init a)
export const rev = (a) => [...a].reverse();

// shiftup a b = [head (head b), a + last (head b)] : shiftup a (tail b)
export const shiftup = (k, b) => b.map(([x, y]) => [x, y + k]);

const memo = new Map();

// hilbert n = rev (rotr h prev) ++ shiftup h prev
//          ++ reflexy (power2 n) (rev (rotr h prev) ++ shiftup h prev)
//   where h = power2 (n-1), prev = hilbert (n-1)
export function hilbert(n) {
  if (n <= 0) return [];
  if (n === 1) return [[0, 0], [0, 1], [1, 1], [1, 0]];
  if (memo.has(n)) return memo.get(n);
  const parts = hilbertParts(n);
  const out = [...parts.rotated, ...parts.shifted, ...parts.mirrored];
  memo.set(n, out);
  return out;
}

// The three pieces of one recursive step, kept apart for the explainer.
export function hilbertParts(n) {
  const prev = hilbert(n - 1);
  const h = power2(n - 1);
  const rotated = rev(rotr(h, prev));
  const shifted = shiftup(h, prev);
  const mirrored = reflexy(power2(n), [...rotated, ...shifted]);
  return { prev, h, rotated, shifted, mirrored };
}

// The defining line mentions `hilbert (n-1)` four times. Without sharing
// (e.g. in GHCi) each one is its own call: calls(n) = 1 + 4 * calls(n-1).
export const hilbertCalls = (n) => (n <= 1 ? 1 : 1 + 4 * hilbertCalls(n - 1));
