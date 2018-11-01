
Heron Reference Manual
===================

This manual highlights specific features of the Heron solver that are undocumented in the help page. For the TESL language itself, please refer to the [TESL Reference](http://wdi.supelec.fr/software/TESL/RefMan).

```bash
./heron --help
```

Emacs mode
-------------------
A simple syntax highlighting mode is provided for Emacs in `lib/tesl-mode.el`. To use it, add the following lines to your `~/.emacs`:

```
(setq load-path
      (append load-path
		'("[YOUR PATH TO THE CLONED REPOSITORY]/heron/lib")))
(require 'tesl-mode)
```

Run diagrams
-------------------

### Output in terminal

Outputs can be made with the directive `@print`.

### TikZ/LaTeX/PDF formatting

It is also possible to produce a run diagram in TikZ/LaTeX by adding at the end of the specification the directive

```
@output tikz  // for a TikZ figure
@output tex   // for a standalone LaTeX file
```

A file named `output.tex` will be produced in the local directory. To compile it, please use `tesl.sty` (provided in `lib/`)

```bash
TEXINPUTS=$TEXINPUTS:"./lib" pdflatex output.tex
```
Or, just simply use in your specification

```
@output pdf
```

#### Hide non-ticking primitives explicitly dsiplayed
 Just change in `lib/TESL.sty` the color gradient of `nontickcolor` as
```
\definecolor{nontickcolor}{gray}{1.0}
```
Then, recompile your output.

### Runtime 

Run generation may take some time depending on size and defined settings. To output the run diagram incrementally, you can set the argument `--runtime-print`. For instance

```bash
./heron  --use examples/reference-gallery/ConcurrentComputations.tesl --runtime-print
```

