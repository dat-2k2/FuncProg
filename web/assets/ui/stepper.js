// "How it works" stepper: progress bar, one idea per step, Back / Next,
// and a hook that lets the demo redraw its picture and code highlight.

export class Stepper {
  constructor(root, { onStep }) {
    this.root = root;
    this.onStep = onStep;
    this.steps = [];
    this.i = 0;
    root.innerHTML = `
      <div class="progress" role="group" aria-label="Steps"></div>
      <div class="explain-figure"></div>
      <div class="step-text" aria-live="polite">
        <h3></h3>
        <p></p>
        <pre class="trace" hidden></pre>
      </div>
      <div class="step-nav">
        <span class="step-count"></span>
        <div class="btn-row">
          <button class="btn secondary" data-act="back">← Back</button>
          <button class="btn primary" data-act="next">Next →</button>
        </div>
      </div>`;
    this.figure = root.querySelector('.explain-figure');
    root.querySelector('[data-act=back]').addEventListener('click', () => this.go(this.i - 1));
    root.querySelector('[data-act=next]').addEventListener('click', () => this.go(this.i + 1));
    root.addEventListener('keydown', (e) => {
      if (e.target.closest('input, select')) return;
      if (e.key === 'ArrowRight') this.go(this.i + 1);
      if (e.key === 'ArrowLeft') this.go(this.i - 1);
    });
  }

  setSteps(steps, keepIndex = false) {
    this.steps = steps;
    this.go(keepIndex ? Math.min(this.i, steps.length - 1) : 0);
  }

  go(i) {
    if (i < 0 || i >= this.steps.length) return;
    this.i = i;
    const s = this.steps[i];
    const progress = this.root.querySelector('.progress');
    progress.innerHTML = this.steps.map((_, k) =>
      `<button class="${k <= i ? 'on' : ''}" aria-label="Go to step ${k + 1}" data-k="${k}"></button>`).join('');
    progress.querySelectorAll('button').forEach((b) => b.addEventListener('click', () => this.go(+b.dataset.k)));
    this.root.querySelector('h3').textContent = `${i + 1} · ${s.title}`;
    this.root.querySelector('.step-text p').innerHTML = s.text;
    const trace = this.root.querySelector('.trace');
    trace.hidden = !s.trace;
    trace.innerHTML = s.trace || '';
    this.root.querySelector('.step-count').textContent = `${i + 1} / ${this.steps.length}`;
    this.root.querySelector('[data-act=back]').disabled = i === 0;
    this.root.querySelector('[data-act=next]').disabled = i === this.steps.length - 1;
    this.onStep(s, i);
  }
}

// Shorten long Haskell lists for traces: [[0,0],[1,0],…] (64 items)
export function showList(xs, keep = 4) {
  const show = (x) => (Array.isArray(x) ? `[${x.map(show).join(',')}]` : String(x));
  if (xs.length <= keep) return show(xs);
  return `[${xs.slice(0, keep).map(show).join(',')},…] <span class="dim">(${xs.length} items)</span>`;
}
