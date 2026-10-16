// Renders a Haskell source file with line numbers and light syntax colouring,
// and lets a demo highlight whole definitions or mark exact sub-expressions.

const KEYWORDS = new Set(['module', 'where', 'import', 'data', 'deriving', 'if', 'then', 'else',
  'let', 'in', 'do', 'case', 'of', 'type', 'newtype', 'class', 'instance', 'return']);

// One CSS class per character; simpler than a token stream once marks overlap tokens.
function classify(line) {
  const cls = new Array(line.length).fill('');
  const set = (a, b, c) => { for (let i = a; i < b; i++) cls[i] = c; };
  let i = 0;
  while (i < line.length) {
    const ch = line[i];
    if (ch === '-' && line[i + 1] === '-') { set(i, line.length, 'c'); break; }
    if (ch === '"') {
      let j = i + 1;
      while (j < line.length && line[j] !== '"') j += line[j] === '\\' ? 2 : 1;
      set(i, j + 1, 's'); i = j + 1; continue;
    }
    if (ch === "'" && line[i + 2] === "'" && !/\w/.test(line[i - 1] || '')) { set(i, i + 3, 's'); i += 3; continue; }
    if (/[0-9]/.test(ch) && !/\w/.test(line[i - 1] || '')) {
      let j = i; while (/[0-9]/.test(line[j] || '')) j++; set(i, j, 'n'); i = j; continue;
    }
    if (/[A-Za-z_]/.test(ch)) {
      let j = i; while (/[\w']/.test(line[j] || '')) j++;
      const word = line.slice(i, j);
      if (KEYWORDS.has(word)) set(i, j, 'k');
      else if (/^[A-Z]/.test(word)) set(i, j, 't');
      else if (i === 0) set(i, j, 'f'); // a top-level definition starts in column 0
      i = j; continue;
    }
    if (/[=\-+*<>|&\\:.$!/]/.test(ch)) { set(i, i + 1, 'o'); i++; continue; }
    i++;
  }
  return cls;
}

const esc = (s) => s.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' })[c]);

function renderLine(line, marks) {
  const cls = classify(line);
  const marked = new Array(line.length).fill(false);
  for (const [a, b] of marks) for (let i = a; i < b; i++) marked[i] = true;
  const spans = (a, b) => {
    let out = '';
    for (let i = a; i < b;) {
      let j = i;
      while (j < b && cls[j] === cls[i]) j++;
      const piece = esc(line.slice(i, j));
      out += cls[i] ? `<span class="hs-${cls[i]}">${piece}</span>` : piece;
      i = j;
    }
    return out;
  };
  // one <mark> per marked run, so a sub-expression is a single box
  let html = '';
  for (let i = 0; i < line.length;) {
    let j = i;
    while (j < line.length && marked[j] === marked[i]) j++;
    html += marked[i] ? `<mark>${spans(i, j)}</mark>` : spans(i, j);
    i = j;
  }
  return html || ' ';
}

export class CodePane {
  constructor(root, { file, href }) {
    this.root = root;
    this.file = file;
    this.lines = [];
    root.innerHTML = `
      <div class="code-tab"><span>${file}</span><a href="${href}" target="_blank" rel="noopener">Full file ↗</a></div>
      <pre class="code" tabindex="0" aria-label="Haskell source of ${file}"><code></code></pre>`;
    this.pre = root.querySelector('pre');
    this.code = root.querySelector('code');
  }

  async load(url) {
    try {
      const res = await fetch(url);
      if (!res.ok) throw new Error(res.status);
      this.lines = (await res.text()).replace(/\r/g, '').split('\n');
    } catch {
      this.lines = [`-- could not load ${this.file}`];
    }
    this.focus({});
  }

  // Line numbers (1-based) of a top-level definition: its signature and every equation.
  defLines(name) {
    const re = new RegExp(`^${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(\\s|::)`);
    const out = [];
    this.lines.forEach((l, i) => {
      if (re.test(l)) {
        out.push(i + 1);
        // include indented continuation lines (where-clauses, wrapped bodies)
        for (let j = i + 1; j < this.lines.length && /^\s+\S/.test(this.lines[j]); j++) out.push(j + 1);
      }
    });
    return [...new Set(out)];
  }

  // focus({ defs: ['rotr'], marks: [['shiftup (power2', 0, 'hilbert']], show: ['hilbert'] })
  //   defs  – definitions to highlight
  //   marks – [text, occurrence, within] to wrap in <mark>; occurrence -1 marks all of them,
  //           `within` limits the search to one definition
  //   show  – when nothing is highlighted, the definitions to scroll to
  focus({ defs = [], marks = [], show = [] }) {
    const hot = new Set(defs.flatMap((d) => this.defLines(d)));
    const lineMarks = this.lines.map(() => []);
    for (const [text, nth = 0, within] of marks) {
      const scope = within ? new Set(this.defLines(within)) : null;
      let seen = 0;
      this.lines.forEach((l, i) => {
        if (scope && !scope.has(i + 1)) return;
        let from = 0, at;
        while ((at = l.indexOf(text, from)) !== -1) {
          if (nth === -1 || seen === nth) { lineMarks[i].push([at, at + text.length]); hot.add(i + 1); }
          seen++; from = at + text.length;
        }
      });
    }
    this.code.innerHTML = this.lines.map((l, i) =>
      `<span class="ln${hot.has(i + 1) ? ' hot' : ''}" data-n="${i + 1}">${renderLine(l, lineMarks[i])}</span>`).join('');
    const target = [...hot].sort((a, b) => a - b)[0] ?? show.flatMap((d) => this.defLines(d))[0];
    if (target) {
      const el = this.code.querySelector(`[data-n="${target}"]`);
      // keep the page still; only scroll inside the pane
      this.pre.scrollTo({ top: Math.max(0, el.offsetTop - 48), behavior: this.ready ? 'smooth' : 'auto' });
    }
    this.ready = true;
  }
}
