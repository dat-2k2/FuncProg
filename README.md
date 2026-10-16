# FuncProg
FuncProg, Spring 2023

Assignment 1 is about the fractal. Assignment 2 is about the rookie problem, with JS visualization. Assignment 3 demonstrates a game between 2 strategies (see https://ncase.me/trust/). 

In Assignment 2, recommend to try assertion to break the recursion instead of filter (tested on Scala).

## Web demo

`web/` is a static site that runs all three assignments in the browser. Each demo has two modes:

- **Play**: change the inputs and watch the result (curve order, board size, strategies, or play the trust game yourself).
- **How it works**: a step-by-step walkthrough of the Haskell. Each step highlights the lines and sub-expressions that are running, next to a picture of the data at that point (the curve pieces, the recursion tree, or the `Game` record after each round).

The demos use JavaScript ports of the Haskell functions (`web/assets/haskell/`). `npm test` evaluates the real `.hs` files with `ghc -e` and checks that the ports return exactly the same values.

```sh
npm test          # needs ghc on PATH
npm run serve     # builds dist/ and serves it at http://localhost:8080
```

### Publishing

`.github/workflows/pages.yml` runs the GHC check, builds `dist/` and deploys it to GitHub Pages on every push to `master`.
Turn it on once under **Settings → Pages → Source: GitHub Actions**. The site is then at `https://dat-2k2.github.io/FuncProg/`.

`dist/` is plain static files with relative paths, so you can also copy it into any folder of an existing website.

### Embedding on another page

Add `embed=1` to get just the demo card. `mode=explain` opens it in "How it works". `embed.js` resizes the frame to fit:

```html
<iframe src="https://dat-2k2.github.io/FuncProg/?demo=hilbert&embed=1"
        title="Hilbert curve demo" loading="lazy"
        style="width:100%;height:680px;border:0"></iframe>
<script src="https://dat-2k2.github.io/FuncProg/embed.js" async></script>
```

`demo` is one of `hilbert`, `rooks`, `trust`. The "Embed" section of the site builds this snippet for you.
