import { PLAYERS, choosePlayer, runGameStates, nextMoves, humanStep, newGame } from '../haskell/trust.js';
import { Stepper } from './stepper.js';

const verb = (m) => (m ? 'cooperates' : 'cheats');
const options = (sel) => PLAYERS.map((p) => `<option value="${p.id}" ${p.id === sel ? 'selected' : ''}>${p.name}</option>`).join('');
const dots = (moves, upto = moves.length) => !moves.length ? '<span class="dim">no moves yet</span>' :
  moves.map((m, i) => `<i class="coin ${m ? 'coop' : 'cheat'}${i < upto ? '' : ' later'}" title="round ${i + 1}: ${verb(m)}"></i>`).join('');

// Show a Game value the way `deriving Show` would, marking fields that changed.
function gameHTML(g, before) {
  const pair = ([a, b]) => `(${a},${b})`;
  const field = (name, val, changed) => `<span class="${changed ? 'changed' : ''}">${name} = ${val}</span>`;
  const mode = `(${g.mode.map((m) => (m ? 'True' : 'False')).join(',')})`;
  const log = `[${g.logGame.map(pair).join(',')}]`;
  return `<code class="record">Game { ${[
    field('rounds', g.rounds, false),
    field('mode', mode, before && before.mode.join() !== g.mode.join()),
    field('logGame', log, before && before.logGame.length !== g.logGame.length),
    field('points', pair(g.points), before && before.points.join() !== g.points.join()),
  ].join(',\n       ')} }</code>`;
}

export function mountTrust(card, code) {
  const play = card.querySelector('.play');
  const explain = card.querySelector('.explain');
  play.innerHTML = `
    <div class="subtabs" role="tablist">
      <button role="tab" aria-selected="true" data-sub="match">Bot vs bot</button>
      <button role="tab" aria-selected="false" data-sub="human">Play it yourself</button>
    </div>
    <div data-panel="match">
      <div class="controls wrap">
        <label class="field"><span class="label">Player A</span><select data-act="a">${options(1)}</select></label>
        <span class="vs">vs</span>
        <label class="field"><span class="label">Player B</span><select data-act="b">${options(2)}</select></label>
        <label class="field grow"><span class="label">Rounds <b data-s="rounds">10</b></span><input type="range" min="1" max="30" value="10" data-act="rounds"></label>
        <button class="btn primary" data-act="run">▶ Run match</button>
      </div>
      <div class="timeline">
        <div class="lane"><span class="who" data-s="na"></span><div class="coins" data-s="ca"></div></div>
        <div class="lane"><span class="who" data-s="nb"></span><div class="coins" data-s="cb"></div></div>
        <div class="legend-row"><span><i class="coin coop"></i>cooperate (−1 for you, +3 for them)</span><span><i class="coin cheat"></i>cheat</span></div>
      </div>
      <div class="controls">
        <div class="control"><div class="stat"><b data-s="pa">0</b><span data-s="la"></span></div><div class="stat"><b data-s="pb">0</b><span data-s="lb"></span></div></div>
        <p class="verdict" data-s="verdict"></p>
      </div>
      <details class="matrix">
        <summary>Every pairing, same number of rounds</summary>
        <table><thead></thead><tbody></tbody></table>
        <p class="dim">Points for the row player against the column player.</p>
      </details>
    </div>
    <div data-panel="human" hidden>
      <div class="controls wrap">
        <label class="field"><span class="label">Opponent</span><select data-act="bot">${options(2)}</select></label>
        <p class="dim grow" data-s="rule"></p>
      </div>
      <div class="timeline">
        <div class="lane"><span class="who">You</span><div class="coins" data-s="hy"></div></div>
        <div class="lane"><span class="who" data-s="hn"></span><div class="coins" data-s="hb"></div></div>
      </div>
      <div class="controls">
        <div class="control"><div class="stat"><b data-s="hpy">0</b><span>you</span></div><div class="stat"><b data-s="hpb">0</b><span data-s="hl"></span></div><span class="mono dim" data-s="hr"></span></div>
        <div class="control">
          <button class="btn coop" data-move="1">Cooperate</button>
          <button class="btn cheat" data-move="0">Cheat</button>
          <button class="btn secondary" data-act="reset">Reset</button>
        </div>
      </div>
    </div>`;
  const $ = (sel) => play.querySelector(sel);
  let timer = 0;

  const showMatch = (animate) => {
    clearInterval(timer);
    const A = choosePlayer(+$('[data-act=a]').value), B = choosePlayer(+$('[data-act=b]').value);
    const rounds = +$('[data-act=rounds]').value;
    const states = runGameStates(rounds, A, B);
    const final = states.at(-1);
    const movesA = final.logGame.map((m) => m[0]), movesB = final.logGame.map((m) => m[1]);
    $('[data-s=na]').textContent = A.name;
    $('[data-s=nb]').textContent = B.name;
    $('[data-s=la]').textContent = `${A.name} (A)`;
    $('[data-s=lb]').textContent = `${B.name} (B)`;
    const paint = (k) => {
      $('[data-s=ca]').innerHTML = dots(movesA, k);
      $('[data-s=cb]').innerHTML = dots(movesB, k);
      $('[data-s=pa]').textContent = states[k].points[0];
      $('[data-s=pb]').textContent = states[k].points[1];
      const [pa, pb] = states[k].points;
      $('[data-s=verdict]').textContent = k < rounds ? `Round ${k} of ${rounds}…`
        : pa === pb ? 'A draw.' : `${pa > pb ? 'A' : 'B'} ends ahead by ${Math.abs(pa - pb)}.`;
    };
    if (animate) {
      let k = 0;
      paint(0);
      timer = setInterval(() => { paint(++k); if (k >= rounds) clearInterval(timer); }, Math.max(60, 900 / rounds));
    } else paint(rounds);
    // round-robin table
    $('.matrix thead').innerHTML = `<tr><th></th>${PLAYERS.map((p) => `<th>${p.name}</th>`).join('')}</tr>`;
    $('.matrix tbody').innerHTML = PLAYERS.map((r) => `<tr><th>${r.name}</th>${PLAYERS.map((c) => {
      const pts = runGameStates(rounds, r, c).at(-1).points[0];
      return `<td class="${r === A && c === B ? 'here' : ''}">${pts}</td>`;
    }).join('')}</tr>`).join('');
    code.focus({ defs: ['runGame'], show: ['runGame'] });
  };
  ['a', 'b'].forEach((k) => $(`[data-act=${k}]`).addEventListener('change', () => showMatch(false)));
  $('[data-act=rounds]').addEventListener('input', (e) => { $('[data-s=rounds]').textContent = e.target.value; showMatch(false); });
  $('[data-act=run]').addEventListener('click', () => showMatch(true));

  // you are player 1, as in runGameIO
  const HUMAN_ROUNDS = 10;
  let hg = newGame(HUMAN_ROUNDS);
  const showHuman = () => {
    const bot = choosePlayer(+$('[data-act=bot]').value);
    $('[data-s=rule]').textContent = bot.rule;
    $('[data-s=hn]').textContent = bot.name;
    $('[data-s=hl]').textContent = bot.name;
    $('[data-s=hy]').innerHTML = dots(hg.logGame.map((m) => m[0]));
    $('[data-s=hb]').innerHTML = dots(hg.logGame.map((m) => m[1]));
    $('[data-s=hpy]').textContent = hg.points[0];
    $('[data-s=hpb]').textContent = hg.points[1];
    const done = hg.logGame.length >= HUMAN_ROUNDS;
    $('[data-s=hr]').textContent = done ? 'Game over' : `round ${hg.logGame.length + 1} / ${HUMAN_ROUNDS}`;
    play.querySelectorAll('[data-move]').forEach((b) => (b.disabled = done));
    code.focus({ defs: ['runGameIO'], show: ['runGameIO'] });
  };
  play.querySelectorAll('[data-move]').forEach((b) => b.addEventListener('click', () => {
    hg = humanStep(hg, choosePlayer(+$('[data-act=bot]').value), +b.dataset.move);
    showHuman();
  }));
  $('[data-act=bot]').addEventListener('change', () => { hg = newGame(HUMAN_ROUNDS); showHuman(); });
  $('[data-act=reset]').addEventListener('click', () => { hg = newGame(HUMAN_ROUNDS); showHuman(); });

  let sub = 'match';
  play.querySelectorAll('[data-sub]').forEach((t) => t.addEventListener('click', () => {
    sub = t.dataset.sub;
    play.querySelectorAll('[data-sub]').forEach((x) => x.setAttribute('aria-selected', String(x === t)));
    play.querySelectorAll('[data-panel]').forEach((p) => (p.hidden = p.dataset.panel !== sub));
    sub === 'match' ? showMatch(false) : showHuman();
  }));

  // ----- How it works: one immutable Game value per recursive call -----
  let ea = 1, eb = 2, er = 6;
  const stepper = new Stepper(explain, { onStep: (s) => { s.draw(); code.focus(s.code); } });
  stepper.figure.innerHTML = `
    <div class="figure-side row">
      <label class="label">A <select data-act="ea">${options(ea)}</select></label>
      <label class="label">B <select data-act="eb">${options(eb)}</select></label>
      <label class="label">rounds <select data-act="er">${[3, 4, 5, 6, 8, 10].map((r) => `<option ${r === er ? 'selected' : ''}>${r}</option>`).join('')}</select></label>
    </div>
    <div class="state-view"></div>`;
  const view = stepper.figure.querySelector('.state-view');

  const buildSteps = () => {
    const A = choosePlayer(ea), B = choosePlayer(eb);
    const states = runGameStates(er, A, B);
    const draw = (k, before) => () => {
      const g = states[k];
      view.innerHTML = `
        <div class="timeline compact">
          <div class="lane"><span class="who">${A.name}</span><div class="coins">${dots(g.logGame.map((m) => m[0]))}</div></div>
          <div class="lane"><span class="who">${B.name}</span><div class="coins">${dots(g.logGame.map((m) => m[1]))}</div></div>
        </div>
        <div class="records">
          ${before ? `<div><span class="label">current</span>${gameHTML(before)}</div><div class="arrow" aria-hidden="true">↓ addMoves</div>` : ''}
          <div><span class="label">${before ? 'new value' : `g${k}`}</span>${gameHTML(g, before)}</div>
        </div>`;
    };
    const why = (p, label, mode, prev, move, first) => first
      ? `${label} (${p.name}) starts with <code>coopMove ${p.name.toLowerCase()} 1</code> → ${move}.`
      : `${label}'s mode is <code>${mode ? 'True' : 'False'}</code>, so it plays <code>${mode ? 'coopMove' : 'cheatMove'}</code>
         on its own last move (${prev}) → <b>${move}</b>.`;
    const steps = [{
      title: 'A game is one value',
      text: `<code>Game</code> is a record: how many rounds, each player's mood (<code>True</code> = still trusting),
             the log of moves so far, and the points. <code>runGame</code> never edits it. Each round makes a
             <em>new</em> <code>Game</code> with <code>addMoves</code> and passes it to the next recursive call.`,
      trace: `runGame (Game ${er} (True,True) [] (0,0)) ${A.name.toLowerCase()} ${B.name.toLowerCase()}`,
      code: { defs: ['data Game'], marks: [] },
      draw: draw(0),
    }];
    for (let k = 1; k <= er; k++) {
      const g0 = states[k - 1], g1 = states[k];
      const [m1, m2] = nextMoves(g0, A, B);
      const first = k === 1;
      const last = g0.logGame.at(-1) || [];
      const flips = [];
      if (g0.mode[0] && !g1.mode[0]) flips.push(`B cheated, so A's mode turns <code>False</code>`);
      if (g0.mode[1] && !g1.mode[1]) flips.push(`A cheated, so B's mode turns <code>False</code>`);
      steps.push({
        title: `Round ${k}: A ${verb(m1)}, B ${verb(m2)}`,
        text: `${why(A, 'A', g0.mode[0], last[0], m1, first)} ${why(B, 'B', g0.mode[1], last[1], m2, first)}
               ${flips.length ? `<br>${flips.join('; ')}. Because <code>newMode</code> uses <code>&amp;&amp;</code>, a mode that turns False can never turn True again.` : ''}`,
        trace: `addMoves g${k - 1} (${m1},${m2})\n= g${k}   -- points (${g0.points}) → (${g1.points})`,
        code: {
          defs: ['addMoves'],
          marks: [
            first ? ['coopMove player1 1', 0, 'runGame'] : ['makeMove player1 mode1', 0, 'runGame'],
            ...(flips.length ? [['newMode = (snd a /= 0 && fst (mode current), fst a /= 0 && snd (mode current))', 0, 'addMoves']] : []),
          ],
        },
        draw: draw(k, g0),
      });
    }
    steps.push({
      title: 'The recursion stops',
      text: `Now <code>length (logGame current) >= rounds current</code>, so <code>runGame</code> returns the
             value it was given instead of calling itself again. That last <code>Game</code> is the result.
             Final score: A ${states[er].points[0]}, B ${states[er].points[1]}.`,
      trace: `runGame g${er} … = g${er}`,
      code: { defs: [], marks: [['if length (logGame current) >= rounds current then current', 0, 'runGame']] },
      draw: draw(er),
    });
    stepper.setSteps(steps, true);
  };
  for (const [key, set] of [['ea', (v) => (ea = v)], ['eb', (v) => (eb = v)], ['er', (v) => (er = v)]]) {
    stepper.figure.querySelector(`[data-act=${key}]`).addEventListener('change', (e) => { set(+e.target.value); buildSteps(); });
  }

  return {
    showPlay: () => (sub === 'match' ? showMatch(false) : showHuman()),
    showExplain: () => (stepper.steps.length ? stepper.go(stepper.i) : buildSteps()),
  };
}
