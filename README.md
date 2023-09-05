# Progressively

Add functions useful for showing progress bar in R packages `purrr` and `furrr`.

Works well in jupyter/jupyterlab where `handlers(global = TRUE)` gives error, and `.progress=TRUE` in purrr cannot delete previous line.

# Usage
```
source('progressively.R')
```

# Functions
- base purrr and furrr function (abbreved): 
  - `map`, `map2`, `imap` and `pmap`
  - `fmap`, `fmap2`, `fimap` and `fpmap`
- prefix with "v": add progress bar
  - `vmap`, `vmap2`, `vimap` and `vpmap`
  - `fmap`, `fmap2`, `fimap` and `fpmap`
- suffix with "s": simplified result (actually same as "_vec" suffix in `purrr`)
  - `maps`, `map2s`, `imaps` and `pmaps`
  - `fmaps`, `fmap2s`, `fimaps` and `fpmaps`
- prefix with "v" and suffix with "s":
  - `vmaps`, `vmap2s`, `vimaps` and `vpmaps`
  - `fvmaps`, `fvmap2s`, `fvimaps` and `fvpmaps`

# Explanation
- default handler registered for progressr is `handlers(myhandler_txtprogressbar(style = 4L, file = "", intrusiveness = 1, clear = T))`, whose parameters can be changed.
  - style 4 is a new added one that shows the exact iteration times instead of percent.
- parameter `rp` in "fv" series: since updating progress bar could dramatically reduce speed especially when `.x` size is large, set `rp` to a higher value allows only updating with a 1/rp possibilities.
  - (similar to parameter `intrusiveness` in progressr, but faster.)