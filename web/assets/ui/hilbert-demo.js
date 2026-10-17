import { hilbert, hilbertParts, hilbertCalls, rotr } from '../haskell/hilbert.js';
import { Stepper, showList } from './stepper.js';

const COLORS = { rotated: '#3B6FB6', shifted: '#2F8F6B', mirrored: '#D08A2B', prev: '#8D877B' };

// Draw layers of points on a square canvas; y grows upwards like the Haskell coordinates.
function draw(canvas, size, layers, { grid = size <= 32, joins = [] } = {}) {
  const css = canvas.clientWidth || 360;
  const dpr = window.devicePixelRatio || 1;
  canvas.width = css * dpr;
  canvas.height = css * dpr;
  const ctx = canvas.getContext('2d');
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  ctx.clearRect(0, 0, css, css);
  const cell = css / size;
  const X = (x) => cell / 2 + x * cell;
  const Y = (y) => css - cell / 2 - y * cell;
  const style = getComputedStyle(canvas);
  if (grid) {
    ctx.strokeStyle = style.getPropertyValue('--line').trim() || '#E3DFD6';
    ctx.lineWidth = 1;
    for (let i = 0; i <= size; i++) {
      const p = Math.round(i * cell) + 0.5;
      ctx.beginPath(); ctx.moveTo(p, 0); ctx.lineTo(p, css); ctx.stroke();
      ctx.beginPath(); ctx.moveTo(0, p); ctx.lineTo(css, p); ctx.stroke();
    }
  }
  const lw = Math.max(1.25, Math.min(5, cell * 0.28));
  ctx.lineJoin = 'round';
  ctx.lineCap = 'round';
  for (const { pts, color, upto = pts.length, dash, alpha = 1, gradient } of layers) {
    ctx.globalAlpha = alpha;
    ctx.setLineDash(dash ? [lw * 1.4, lw * 1.8] : []);
    ctx.lineWidth = lw;
    if (gradient) {
      // colour along the path, so the visiting order is visible
      const step = Math.max(1, Math.floor(pts.length / 96));
      for (let i = 1; i < upto; i += step) {
        const j = Math.min(i + step, upto);
        ctx.strokeStyle = `hsl(${32 - (47 * i) / pts.length} 78% 46%)`;
        ctx.beginPath(); ctx.moveTo(X(pts[i - 1][0]), Y(pts[i - 1][1]));
        for (let k = i; k < j; k++) ctx.lineTo(X(pts[k][0]), Y(pts[k][1]));
        ctx.stroke();
      }
    } else {
      ctx.strokeStyle = color;
      ctx.beginPath();
      pts.slice(0, upto).forEach(([x, y], i) => (i ? ctx.lineTo(X(x), Y(y)) : ctx.moveTo(X(x), Y(y))));
      ctx.stroke();
    }
  }
  ctx.globalAlpha = 1;
  ctx.setLineDash([lw, lw * 1.2]);
  ctx.lineWidth = Math.max(1, lw * 0.6);
  ctx.strokeStyle = style.getPropertyValue('--ink').trim() || '#1B1A17';
  for (const [a, b] of joins) {
    ctx.beginPath(); ctx.moveTo(X(a[0]), Y(a[1])); ctx.lineTo(X(b[0]), Y(b[1])); ctx.stroke();
  }
  ctx.setLineDash([]);
}

export function mountHilbert(card, code) {
  const play = card.querySelector('.play');
  const explain = card.querySelector('.explain');
  play.innerHTML = `
    <div class="canvas-wrap"><canvas class="curve" role="img"></canvas></div>
    <div class="controls">
      <div class="control">
        <span class="label" id="h-order">Order</span>
        <div class="seg" role="radiogroup" aria-labelledby="h-order">
          ${[1, 2, 3, 4, 5, 6, 7].map((n) => `<button role="radio" data-n="${n}">${n}</button>`).join('')}
        </div>
      </div>
      <div class="control">
        <div class="stat"><b data-s="points"></b><span>points</span></div>
        <div class="stat"><b data-s="grid"></b><span>grid</span></div>
        <div class="stat" title="Calls to hilbert when the four mentions of hilbert (n-1) are not shared"><b data-s="calls"></b><span>calls</span></div>
        <button class="btn primary" data-act="animate">▶ Animate</button>
      </div>
    </div>`;
  const canvas = play.querySelector('canvas');
  let order = 4;
  let anim = 0;

  const render = (upto) => {
    const pts = hilbert(order);
    draw(canvas, 2 ** order, [{ pts, upto, gradient: true }]);
    canvas.setAttribute('aria-label', `Hilbert curve of order ${order}, ${pts.length} points`);
  };
  const setOrder = (n) => {
    order = n;
    cancelAnimationFrame(anim);
    play.querySelectorAll('.seg button').forEach((b) => b.setAttribute('aria-checked', String(+b.dataset.n === n)));
    play.querySelector('[data-s=points]').textContent = (4 ** n).toLocaleString();
    play.querySelector('[data-s=grid]').textContent = `${2 ** n}×${2 ** n}`;
    play.querySelector('[data-s=calls]').textContent = hilbertCalls(n).toLocaleString();
    render();
    code.focus({ show: ['hilbert'] });
  };
  play.querySelectorAll('.seg button').forEach((b) => b.addEventListener('click', () => setOrder(+b.dataset.n)));
  play.querySelector('[data-act=animate]').addEventListener('click', () => {
    cancelAnimationFrame(anim);
    const total = 4 ** order;
    const ms = Math.min(4000, 600 + total * 3);
    const t0 = performance.now();
    const tick = (t) => {
      const k = Math.min(1, (t - t0) / ms);
      render(Math.max(2, Math.ceil(k * total)));
      if (k < 1) anim = requestAnimationFrame(tick);
    };
    anim = requestAnimationFrame(tick);
  });

  // ----- How it works -----
  let target = 3;
  const stepper = new Stepper(explain, { onStep: (s) => { s.draw(); code.focus(s.code); } });
  stepper.figure.innerHTML = `
    <div class="figure-row">
      <div class="canvas-wrap small"><canvas class="curve"></canvas></div>
      <div class="figure-side">
        <label class="label">Build order
          <select data-act="target">${[2, 3, 4, 5].map((n) => `<option ${n === 3 ? 'selected' : ''}>${n}</option>`).join('')}</select>
        </label>
        <ul class="legend">
          <li><i style="background:${COLORS.prev}"></i><code>hilbert (n-1)</code></li>
          <li><i style="background:${COLORS.rotated}"></i><code>rev (rotr h prev)</code></li>
          <li><i style="background:${COLORS.shifted}"></i><code>shiftup h prev</code></li>
          <li><i style="background:${COLORS.mirrored}"></i><code>reflexy (2h) …</code></li>
        </ul>
      </div>
    </div>`;
  const ec = stepper.figure.querySelector('canvas');

  const buildSteps = () => {
    const n = target;
    const { prev, h, rotated, shifted, mirrored } = hilbertParts(n);
    const size = 2 ** n;
    const L = (pts, color, extra) => ({ pts, color, ...extra });
    const fig = (layers, opts) => () => draw(ec, size, layers, opts);
    const join1 = [rotated.at(-1), shifted[0]];
    const join2 = [shifted.at(-1), mirrored[0]];
    stepper.setSteps([
      {
        title: 'Start from the smaller curve',
        text: `Haskell has no loops. <code>hilbert ${n}</code> is defined using <code>hilbert ${n - 1}</code>${n > 2
                 ? `, which uses ${n > 3 ? `<code>hilbert ${n - 2}</code>, and so on down to ` : ''}<code>hilbert 1</code>. That base case`
                 : `, the base case, which`} is written out by hand.
               Here is <code>hilbert ${n - 1}</code>: ${prev.length} points on a ${h}×${h} grid.`,
        trace: `hilbert ${n - 1}\n= ${showList(prev)}`,
        code: { defs: ['hilbert'], marks: [['hilbert (n-1)', -1, 'hilbert']] },
        draw: fig([L(prev, COLORS.prev)]),
      },
      {
        title: 'Turn it a quarter, then reverse it',
        text: `<code>rotr h</code> maps every point <code>[x, y]</code> to <code>[y, h-1-x]</code>, a quarter turn.
               <code>rev</code> then reverses the list, so the path still starts in the corner at <code>[0,0]</code>.
               Both return new lists. The original <code>prev</code> is untouched (faded).`,
        trace: `rotr ${h} ${showList(prev, 3)}\n= ${showList(rotr(h, prev), 3)}\nrev it\n= ${showList(rotated, 3)}`,
        code: { defs: ['rotr', 'rev'], marks: [['rev (rotr (power2(n-1)) (hilbert (n-1) ))', 0, 'hilbert']] },
        draw: fig([L(prev, COLORS.prev, { alpha: 0.25, dash: true }), L(rotated, COLORS.rotated)]),
      },
      {
        title: 'Shift a copy up',
        text: `<code>shiftup h</code> adds <code>h</code> to every y-coordinate, putting an unrotated copy in the top-left quarter.
               The dotted step joins the end of the blue piece to the start of the green one.`,
        trace: `shiftup ${h} ${showList(prev, 3)}\n= ${showList(shifted, 3)}`,
        code: { defs: ['shiftup'], marks: [['shiftup (power2 (n-1)) (hilbert (n-1) )', 0, 'hilbert']] },
        draw: fig([L(rotated, COLORS.rotated), L(shifted, COLORS.shifted)], { joins: [join1] }),
      },
      {
        title: 'Mirror the whole left half',
        text: `<code>reflexy</code> reads the list from the end (<code>last</code>, <code>init</code>) and mirrors
               <code>x</code> to <code>${size}-1-x</code>. Because it runs backwards, the mirrored copy starts exactly where the left half ends.`,
        trace: `reflexy ${size} (blue ++ green)\n= ${showList(mirrored, 3)}`,
        code: { defs: ['reflexy'], marks: [['reflexy (power2 n)', 0, 'hilbert']] },
        draw: fig([L(rotated, COLORS.rotated, { alpha: 0.35 }), L(shifted, COLORS.shifted, { alpha: 0.35 }), L(mirrored, COLORS.mirrored)]),
      },
      {
        title: 'Glue the pieces with ++',
        text: `<code>++</code> joins lists end to end. The answer is still just a list of
               ${(4 ** n).toLocaleString()} points; drawing them in order gives the curve.
               One catch: the line mentions <code>hilbert (n-1)</code> four times. Unless the compiler shares them
               (GHCi doesn't), each one is computed again, so order 7 makes ${hilbertCalls(7).toLocaleString()} calls.`,
        trace: `length (hilbert ${n})\n= ${rotated.length} + ${shifted.length} + ${mirrored.length}\n= ${4 ** n}`,
        code: { defs: ['hilbert'], marks: [['++', -1, 'hilbert']] },
        draw: fig([L(rotated, COLORS.rotated), L(shifted, COLORS.shifted), L(mirrored, COLORS.mirrored)], { joins: [join1, join2] }),
      },
    ], true);
  };
  stepper.figure.querySelector('[data-act=target]').addEventListener('change', (e) => { target = +e.target.value; buildSteps(); });

  const redraw = () => (explain.hidden ? render() : stepper.steps[stepper.i]?.draw());
  new ResizeObserver(redraw).observe(card);

  return {
    showPlay: () => setOrder(order),
    showExplain: () => (stepper.steps.length ? stepper.go(stepper.i) : buildSteps()),
  };
}
