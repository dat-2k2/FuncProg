import { rooks, diffrooks, allPaths, solutions } from '../haskell/rooks.js';
import { Stepper, showList } from './stepper.js';

const PAGE = 24;
const cache = new Map();
const solved = (n, diag) => {
  const key = `${n}:${diag}`;
  if (!cache.has(key)) cache.set(key, solutions(n, diag));
  return cache.get(key);
};

// A path is leaf-first: path[c] is the row (1 = top) of the rook in column c.
function boardHTML(path, n, { diag = false, big = false, label = '' } = {}) {
  let cells = '';
  for (let r = 1; r <= n; r++) {
    for (let c = 0; c < n; c++) {
      const dark = (r + c) % 2 === 0;
      const blocked = diag && r === c + 1;
      const rook = path[c] === r;
      cells += `<i class="${dark ? 'd' : ''}${blocked ? ' x' : ''}">${rook ? '♜' : ''}</i>`;
    }
  }
  return `<div class="board${big ? ' big' : ''}" style="--n:${n}" role="img" aria-label="${label || `rooks in rows ${path.join(', ')}`}">${cells}</div>`;
}

// Lay the tree out left to right: x by depth, y by leaf order.
function layoutTree(root) {
  const nodes = [];
  const edges = [];
  let row = 0;
  const visit = (node, depth, parent, value, path) => {
    const me = { node, depth, value, path, pruned: node === null };
    if (node && !node.leaf) {
      const kids = node.rows.filter((p) => p !== node.value);
      const ys = node.children.map((child, i) => visit(child, depth + 1, me, kids[i], [...path, kids[i]]).y);
      me.y = (ys[0] + ys[ys.length - 1]) / 2;
    } else {
      me.y = row++;
    }
    nodes.push(me);
    if (parent) edges.push([parent, me]);
    return me;
  };
  visit(root, 0, null, 0, []);
  return { nodes, edges, rows: row };
}

function treeSVG(tree, n, { selected = [], showRows = false } = {}) {
  const { nodes, edges, rows } = layoutTree(tree);
  const colW = 64, rowH = n >= 4 ? 18 : 28, padX = 22, padY = 28;
  const W = padX * 2 + colW * (n + 0.6);
  const H = padY * 2 + rowH * (rows - 1);
  const X = (d) => padX + d * colW;
  const Y = (y) => padY + y * rowH;
  const onPath = (m) => m.depth > 0 && selected.length >= m.depth &&
    m.path.every((v, i) => v === selected[selected.length - 1 - i]);
  const isOn = (m) => m.depth === 0 || onPath(m);
  let s = `<svg viewBox="0 0 ${W} ${H}" class="tree" role="img" aria-label="Recursion tree built by permutation">`;
  for (let d = 1; d <= n; d++) {
    s += `<text class="col-label" x="${X(d)}" y="11">col ${n + 1 - d}</text>`;
  }
  for (const [a, b] of edges) {
    s += `<line class="${isOn(a) && isOn(b) ? 'on' : ''}${b.pruned ? ' pruned' : ''}" x1="${X(a.depth)}" y1="${Y(a.y)}" x2="${X(b.depth)}" y2="${Y(b.y)}"/>`;
  }
  for (const m of nodes) {
    const x = X(m.depth), y = Y(m.y);
    const cls = m.pruned ? 'pruned' : isOn(m) ? 'on' : m.node.leaf ? 'leaf' : '';
    const data = m.pruned ? '' : ` data-path="${m.path.join(',')}"`;
    if (m.pruned) {
      s += `<g class="node pruned"><text x="${x}" y="${y + 4}">×${m.value}</text></g>`;
    } else {
      s += `<g class="node ${cls}"${data} tabindex="${m.node.leaf ? 0 : -1}"><circle cx="${x}" cy="${y}" r="${rowH >= 26 ? 9 : 7.5}"/><text x="${x}" y="${y + 3.5}">${m.depth === 0 ? '·' : m.value}</text></g>`;
      if (showRows && isOn(m) && !m.node.leaf) {
        s += `<text class="rows" x="${x}" y="${y - 12}">[${m.node.rows.join(',')}]</text>`;
      }
    }
  }
  return s + '</svg>';
}

export function mountRooks(card, code) {
  const play = card.querySelector('.play');
  const explain = card.querySelector('.explain');
  play.innerHTML = `
    <div class="controls">
      <div class="control">
        <span class="label" id="r-n">n =</span>
        <div class="seg" role="radiogroup" aria-labelledby="r-n">
          ${[1, 2, 3, 4, 5, 6, 7, 8].map((n) => `<button role="radio" data-n="${n}">${n}</button>`).join('')}
        </div>
      </div>
      <label class="switch"><input type="checkbox" data-act="diag"><span></span>Block the diagonal</label>
    </div>
    <div class="boards"></div>
    <div class="controls">
      <div class="control">
        <div class="stat"><b data-s="count"></b><span data-s="what"></span></div>
      </div>
      <div class="control pager">
        <button class="btn secondary" data-act="prev" aria-label="Previous page">←</button>
        <span data-s="page" class="mono"></span>
        <button class="btn secondary" data-act="next" aria-label="Next page">→</button>
      </div>
    </div>`;
  let n = 4, diag = false, page = 0;
  const fact = (k) => (k <= 1 ? 1 : k * fact(k - 1));
  const render = () => {
    const sols = solved(n, diag);
    const pages = Math.max(1, Math.ceil(sols.length / PAGE));
    page = Math.min(page, pages - 1);
    play.querySelectorAll('.seg button').forEach((b) => b.setAttribute('aria-checked', String(+b.dataset.n === n)));
    play.querySelector('.boards').innerHTML = sols.length
      ? sols.slice(page * PAGE, page * PAGE + PAGE).map((p) => boardHTML(p, n, { diag })).join('')
      : `<p class="empty">No way to do it: with the diagonal blocked, a single rook on a 1×1 board has nowhere to go.</p>`;
    play.querySelector('[data-s=count]').textContent = sols.length.toLocaleString();
    play.querySelector('[data-s=what]').textContent = diag ? 'solutions (derangements)' : `solutions = ${n}! = ${fact(n).toLocaleString()}`;
    play.querySelector('[data-s=page]').textContent = sols.length
      ? `${page * PAGE + 1}–${Math.min(sols.length, (page + 1) * PAGE)} of ${sols.length.toLocaleString()}` : '0 of 0';
    play.querySelector('[data-act=prev]').disabled = page === 0;
    play.querySelector('[data-act=next]').disabled = page >= pages - 1;
    code.focus({ defs: diag ? ['diffrooks'] : ['rooks'], show: ['permutation'] });
  };
  play.querySelectorAll('.seg button').forEach((b) => b.addEventListener('click', () => { n = +b.dataset.n; page = 0; render(); }));
  play.querySelector('[data-act=diag]').addEventListener('change', (e) => { diag = e.target.checked; page = 0; render(); });
  play.querySelector('[data-act=prev]').addEventListener('click', () => { page--; render(); });
  play.querySelector('[data-act=next]').addEventListener('click', () => { page++; render(); });

  // ----- How it works -----
  let en = 3, ediag = false, selected = null;
  const stepper = new Stepper(explain, { onStep: (s) => { s.draw(); code.focus(s.code); } });
  stepper.figure.innerHTML = `
    <div class="figure-side row">
      <label class="label">n
        <select data-act="en">${[2, 3, 4].map((k) => `<option ${k === 3 ? 'selected' : ''}>${k}</option>`).join('')}</select>
      </label>
      <label class="switch"><input type="checkbox" data-act="ediag"><span></span>Block the diagonal</label>
      <span class="hint">Click a leaf to pick a path.</span>
    </div>
    <div class="figure-row">
      <div class="tree-wrap"></div>
      <div class="side-board"></div>
    </div>`;
  const treeWrap = stepper.figure.querySelector('.tree-wrap');
  const sideBoard = stepper.figure.querySelector('.side-board');

  const buildSteps = () => {
    const tree = ediag ? diffrooks(en) : rooks(en);
    const all = allPaths(tree);
    const full = all.filter((p) => p.length === en);
    if (!selected || selected.length !== en || !full.some((p) => p.join() === selected.join())) selected = full[0] || [];
    const treeName = ediag ? 'diffrooks' : 'rooks';
    const builder = ediag ? 'pwithfilter' : 'permutation';
    const fig = ({ showRows = false, board = true } = {}) => () => {
      treeWrap.innerHTML = treeSVG(tree, en, { selected, showRows });
      sideBoard.innerHTML = board && selected.length
        ? `${boardHTML(selected, en, { diag: ediag, big: true })}<code>[${selected.join(',')}]</code>` : '';
      treeWrap.querySelectorAll('.node[data-path]').forEach((g) => {
        const pick = () => {
          // a path in the tree is root-first; the Haskell list is leaf-first
          const rootFirst = g.dataset.path.split(',').filter(Boolean).map(Number);
          const match = full.find((p) => rootFirst.every((v, i) => p[p.length - 1 - i] === v));
          if (match) { selected = match; buildSteps(); }
        };
        g.addEventListener('click', pick);
        g.addEventListener('keydown', (e) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); pick(); } });
      });
    };
    // the calls made along the selected path, root first
    const calls = [];
    {
      let node = tree;
      const rootFirst = [...selected].reverse();
      calls.push(`${builder} ${node.n} ${node.value} [${node.rows}]`);
      for (const v of rootFirst) {
        const kids = node.rows.filter((p) => p !== node.value);
        node = node.children[kids.indexOf(v)];
        calls.push(`${builder} ${node.n} ${node.value} [${node.rows}]${node.leaf ? '   -- base case' : ''}`);
      }
    }
    stepper.setSteps([
      {
        title: 'Recursion builds a tree of choices',
        text: `<code>${treeName} ${en}</code> calls <code>${builder} ${en + 1} 0 [${en}..1]</code>.
               The root <code>0</code> means "nothing placed yet". Each level places one rook, starting from the last column,
               and the number on a node is the row it uses. ${ediag
                 ? 'A red × is a branch cut off because it would put a rook on the blocked diagonal.'
                 : `Every row gets tried at every level, so there are ${full.length} leaves.`}`,
        trace: `${treeName} ${en}\n= ${builder} ${en + 1} 0 [${[...Array(en)].map((_, i) => en - i)}]`,
        code: { defs: [builder, treeName], marks: [] },
        draw: fig({ board: false }),
      },
      {
        title: 'Each call removes the row it just used',
        text: `Nothing is updated in place. Each recursive call gets a <em>new</em> list of free rows, made with
               <code>filter</code>. The grey lists show the rows still free at each step of the highlighted path.`,
        trace: calls.join('\n'),
        code: { defs: [builder], marks: [['filter (\\x -> x/= p && x/= pos)', 0, builder], ['filter (/=pos)', 0, builder]] },
        draw: fig({ showRows: true }),
      },
      {
        title: 'allPaths reads every root-to-leaf path',
        text: `<code>allPaths</code> turns the tree back into a list of lists. Each parent adds its value to the
               <em>end</em> (<code>map (++ y)</code>), so paths read leaf-first, and the root's <code>0</code> is dropped.`,
        trace: `allPaths (${treeName} ${en})\n= ${showList(all, 4)}`,
        code: { defs: ['allPaths', 'mergeCustom'], marks: [] },
        draw: fig(),
      },
      {
        title: 'Keep only the complete paths',
        text: ediag
          ? `A cut branch becomes <code>Nil</code>, and <code>allPaths Nil = [[]]</code> still returns a path, just a short one.
             <code>main</code> keeps only paths of length ${en}: ${full.length} of ${all.length} survive.`
          : `<code>main</code> keeps paths of length ${en}. Without the diagonal rule every path is already complete,
             so all ${all.length} survive. Try "Block the diagonal" to see what the filter is for.`,
        trace: `length (allPaths (${treeName} ${en})) = ${all.length}\nlength (filter (\\x -> length x == ${en}) …) = ${full.length}`,
        code: { defs: [], marks: [['filter (\\x -> length x == n)', 0, 'main']], show: ['main'] },
        draw: fig(),
      },
      {
        title: 'Each path is a board',
        text: `Read <code>[${selected.join(',')}]</code> as "column 1 → row ${selected[0]}, column 2 → row ${selected[1]}…".
               Each column has one rook and no row repeats, so no two rooks attack. The original program wrote these lists
               as JSON with <code>toStringCustom2</code> for a JavaScript page to draw.`,
        trace: `{"path": [${selected.join(',')}]}`,
        code: { defs: ['toStringCustom2'], marks: [] },
        draw: fig(),
      },
    ], true);
  };
  stepper.figure.querySelector('[data-act=en]').addEventListener('change', (e) => { en = +e.target.value; selected = null; buildSteps(); });
  stepper.figure.querySelector('[data-act=ediag]').addEventListener('change', (e) => { ediag = e.target.checked; selected = null; buildSteps(); });
  return {
    showPlay: render,
    showExplain: () => (stepper.steps.length ? stepper.go(stepper.i) : buildSteps()),
  };
}
