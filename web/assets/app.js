import { CodePane } from './ui/code-pane.js';
import { mountHilbert } from './ui/hilbert-demo.js';
import { mountRooks } from './ui/rooks-demo.js';
import { mountTrust } from './ui/trust-demo.js';

const REPO = 'https://github.com/dat-2k2/FuncProg/blob/master/';
const MOUNT = { hilbert: mountHilbert, rooks: mountRooks, trust: mountTrust };

const params = new URLSearchParams(location.search);
const embed = params.has('embed') && params.get('embed') !== '0';
document.body.classList.toggle('embed', embed);

const demos = {};
for (const card of document.querySelectorAll('.card')) {
  const name = card.dataset.demo;
  const file = card.dataset.file;
  const code = new CodePane(card.querySelector('.source'), { file, href: REPO + file });
  const api = MOUNT[name](card, code);
  const setMode = (mode) => {
    card.querySelectorAll('[data-mode]').forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.mode === mode)));
    card.querySelector('.play').hidden = mode !== 'play';
    card.querySelector('.explain').hidden = mode !== 'explain';
    card.dataset.mode = mode;
    mode === 'play' ? api.showPlay() : api.showExplain();
  };
  card.querySelectorAll('[data-mode]').forEach((b) => b.addEventListener('click', () => {
    setMode(b.dataset.mode);
    if (b.dataset.mode === 'explain') card.querySelector('.explain').focus({ preventScroll: true });
    syncURL();
  }));
  demos[name] = { card, code, setMode, ready: code.load(`src/${file}`) };
}

let current = MOUNT[params.get('demo')] ? params.get('demo') : 'hilbert';
const startMode = params.get('mode') === 'explain' ? 'explain' : 'play';

function show(name) {
  current = name;
  for (const [key, d] of Object.entries(demos)) d.card.hidden = key !== name;
  document.querySelectorAll('.tabs [data-demo]').forEach((t) => t.setAttribute('aria-selected', String(t.dataset.demo === name)));
  const d = demos[name];
  // the code pane needs its source before a mode can highlight lines in it
  d.ready.then(() => d.setMode(d.card.dataset.mode || startMode));
}

function syncURL() {
  if (embed) return;
  const q = new URLSearchParams({ demo: current });
  if (demos[current].card.dataset.mode === 'explain') q.set('mode', 'explain');
  history.replaceState(null, '', `?${q}`);
}

document.querySelectorAll('.tabs [data-demo]').forEach((t) => t.addEventListener('click', () => { show(t.dataset.demo); syncURL(); }));
document.querySelector('.tabs').addEventListener('keydown', (e) => {
  if (e.key !== 'ArrowRight' && e.key !== 'ArrowLeft') return;
  const tabs = [...document.querySelectorAll('.tabs [data-demo]')];
  const i = tabs.findIndex((t) => t.dataset.demo === current);
  const next = tabs[(i + (e.key === 'ArrowRight' ? 1 : tabs.length - 1)) % tabs.length];
  next.focus();
  next.click();
});
show(current);

// Embed mode: tell the parent page how tall we are (see embed.js).
if (embed && window.parent !== window) {
  const post = () => parent.postMessage({ type: 'funcprog:resize', height: Math.ceil(document.documentElement.scrollHeight) }, '*');
  new ResizeObserver(post).observe(document.body);
  post();
}

// Embed snippet builder
const base = new URL('./', location.href).href;
const snippetEl = document.querySelector('[data-s=snippet]');
const buildSnippet = () => {
  const demo = document.querySelector('[data-act=embed-demo]').value;
  const explain = document.querySelector('[data-act=embed-explain]').checked;
  const title = document.querySelector(`[data-act=embed-demo] option[value=${demo}]`).textContent;
  const src = `${base}?demo=${demo}&embed=1${explain ? '&mode=explain' : ''}`;
  snippetEl.textContent =
`<iframe src="${src}"
  title="${title} demo" loading="lazy"
  style="width:100%;height:680px;border:0"></iframe>
<script src="${base}embed.js" async></script>`;
};
document.querySelector('[data-act=embed-demo]').addEventListener('change', buildSnippet);
document.querySelector('[data-act=embed-explain]').addEventListener('change', buildSnippet);
document.querySelector('[data-act=copy]').addEventListener('click', async (e) => {
  try {
    await navigator.clipboard.writeText(snippetEl.textContent);
    e.target.textContent = 'Copied';
  } catch {
    getSelection().selectAllChildren(snippetEl);
    e.target.textContent = 'Press Ctrl+C';
  }
  setTimeout(() => (e.target.textContent = 'Copy'), 1600);
});
buildSnippet();
