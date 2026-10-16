// Line-for-line port of the rook-placement tree in Assignment2/Assign2.hs.
// A CustomTree is { value, children }, and Nil is null. Nodes also keep the
// call's arguments (n, rows) and a `leaf` flag so the page can draw the tree.
// scripts/parity.mjs checks these against GHC's output.

// rangeCustom n = n : rangeCustom (n-1)
export const rangeCustom = (n) => Array.from({ length: n }, (_, i) => n - i);

// permutation a b [] = CustomTree b [Nil]
// permutation 1 pos availrows = CustomTree pos [Nil]
// permutation n pos availrows = CustomTree pos (map (\p -> permutation (n-1) p
//     (filter (\x -> x/= p && x/= pos) availrows)) (filter (/=pos) availrows))
export function permutation(n, pos, availrows) {
  if (availrows.length === 0 || n === 1) return { value: pos, n, rows: availrows, children: [null], leaf: true };
  return {
    value: pos,
    n,
    rows: availrows,
    children: availrows
      .filter((p) => p !== pos)
      .map((p) => permutation(n - 1, p, availrows.filter((x) => x !== p && x !== pos))),
  };
}

// pwithfilter: the same tree, but a node becomes Nil when f [pos, level] is False.
export function pwithfilter(n, pos, rows, f) {
  if (rows.length === 0) return f([pos, n]) ? { value: pos, n, rows, children: [null], leaf: true } : null;
  if (n === 1) return f([pos, 1]) ? { value: pos, n, rows, children: [null], leaf: true } : null;
  if (!f([pos, n])) return null;
  return {
    value: pos,
    n,
    rows,
    children: rows
      .filter((p) => p !== pos)
      .map((p) => pwithfilter(n - 1, p, rows.filter((x) => x !== p && x !== pos), f)),
  };
}

// rooks n = permutation (n+1) 0 (rangeCustom n)
export const rooks = (n) => permutation(n + 1, 0, rangeCustom(n));

// diffrooks n = pwithfilter (n+1) 0 (rangeCustom n) (\a -> (a!!0 /= a!!1))
export const diffrooks = (n) => pwithfilter(n + 1, 0, rangeCustom(n), (a) => a[0] !== a[1]);

// allPaths Nil = [[]]
// allPaths x = map (++ y) (mergeCustom (map allPaths (children x)))
//   where y = if value x == 0 then [] else [value x]
// Paths come out leaf-first because each parent appends itself to the end.
export function allPaths(node) {
  if (node === null) return [[]];
  const y = node.value === 0 ? [] : [node.value];
  return node.children.flatMap(allPaths).map((p) => [...p, ...y]);
}

// main: filter (\x -> length x == n) (allPaths (rooks n))
export const solutions = (n, blockDiagonal = false) =>
  allPaths(blockDiagonal ? diffrooks(n) : rooks(n)).filter((p) => p.length === n);
