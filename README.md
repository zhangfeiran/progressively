# Progressively

Add functions useful for showing progress bar in R packages `purrr` and `furrr`.

# Usage
`source('progressively.R')`

# Functions
- base purrr function: `map`, `map2`, `imap` and `pmap`
- base furrr function: `fmap`, `fmap2`, `fimap` and `fpmap`
- prefix with "v": add progress bar
  - `vmap`, `vmap2`, `vimap` and `vpmap`
  - `fmap`, `fmap2`, `fimap` and `fpmap`
- suffix with "s": simplified result (actually same as "_vec" suffix in `purrr`)
  - `maps`, `map2s`, `imaps` and `pmaps`
  - `fmaps`, `fmap2s`, `fimaps` and `fpmaps`
- prefix with "v" and suffix with "s":
  - `vmaps`, `vmap2s`, `vimaps` and `vpmaps`
  - `fvmaps`, `fvmap2s`, `fvimaps` and `fvpmaps`

