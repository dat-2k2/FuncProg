// Line-for-line port of the trust game in Assignment3/assignment3.hs.
// A move is 1 (cooperate: pay 1 coin, the other player gets 3) or 0 (cheat).
// scripts/parity.mjs checks these against GHC's output.

// data Player = Player { typePlayer, coopMove, cheatMove }
export const PLAYERS = [
  // cheater _ = 0
  { id: 0, name: 'Cheater', coopMove: () => 0, cheatMove: () => 0,
    rule: 'Always cheats, whatever happens.' },
  // simpletonCoop a = a ; simpletonCheat a = 1 - a
  { id: 1, name: 'Simpleton', coopMove: (a) => a, cheatMove: (a) => 1 - a,
    rule: 'Repeats its own last move while you cooperate. Once cheated, it flips its own last move every round.' },
  // grudgerCoop _ = 1 ; grudgerCheat _ = 0
  { id: 2, name: 'Grudger', coopMove: () => 1, cheatMove: () => 0,
    rule: 'Cooperates until you cheat once, then cheats forever.' },
];

export const choosePlayer = (id) => PLAYERS[id];

// makeMove player mode prev = if mode then coopMove player prev else cheatMove player prev
export const makeMove = (player, mode, prev) => (mode ? player.coopMove(prev) : player.cheatMove(prev));

// data Game = Game { rounds, mode :: (Bool, Bool), logGame :: [(Int,Int)], points :: (Int,Int) }
export const newGame = (rounds) => ({ rounds, mode: [true, true], logGame: [], points: [0, 0] });

// addMoves current a = Game (rounds current) newMode (logGame current ++ [a]) (npoint1, npoint2)
//   newMode = (snd a /= 0 && fst (mode current), fst a /= 0 && snd (mode current))
export function addMoves(current, [a1, a2]) {
  return {
    rounds: current.rounds,
    mode: [a2 !== 0 && current.mode[0], a1 !== 0 && current.mode[1]],
    logGame: [...current.logGame, [a1, a2]],
    points: [
      current.points[0] + (a2 !== 0 ? 3 : 0) - (a1 !== 0 ? 1 : 0),
      current.points[1] + (a1 !== 0 ? 3 : 0) - (a2 !== 0 ? 1 : 0),
    ],
  };
}

// One recursive call of runGame: the moves both players pick from `current`.
export function nextMoves(current, p1, p2) {
  if (current.logGame.length === 0) return [p1.coopMove(1), p2.coopMove(1)];
  const [last1, last2] = current.logGame[current.logGame.length - 1];
  return [makeMove(p1, current.mode[0], last1), makeMove(p2, current.mode[1], last2)];
}

// runGame current p1 p2 = if length (logGame current) >= rounds current then current else runGame (addMoves ...)
// Returns every intermediate Game so the page can step through them.
export function runGameStates(rounds, p1, p2) {
  const states = [newGame(rounds)];
  let g = states[0];
  while (g.logGame.length < g.rounds) {
    g = addMoves(g, nextMoves(g, p1, p2));
    states.push(g);
  }
  return states;
}

export const runGame = (rounds, p1, p2) => runGameStates(rounds, p1, p2).at(-1);

// runGameIO: you are player 1, the bot is player 2.
//   addMoves currentGame (userMove, makeMove bot (snd $ mode currentGame)
//     (if not (null (logGame currentGame)) then snd $ last $ logGame currentGame else coopMove bot 1))
export function humanStep(current, bot, userMove) {
  const prev = current.logGame.length ? current.logGame[current.logGame.length - 1][1] : bot.coopMove(1);
  return addMoves(current, [userMove, makeMove(bot, current.mode[1], prev)]);
}
