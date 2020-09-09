
Heron Reference Manual
===================

This manual highlights specific features of the Heron solver that are undocumented in the help page. For the TESL language itself, please refer to the [TESL Reference](http://wdi.supelec.fr/software/TESL/RefMan).

```bash
./heron --help
```

Compile from sources
-------------------

Two compilers can be used to build from sources whether you wish you obtain a singlecore or a multicore version of Heron.

### Linux (with Dockerized MPL)

Both single and multi-core are supported. It is advisable to use Docker to provide the latest version of MPL. To build:

```bash
git clone https://github.com/heron-solver/heron.git
cd heron
docker run -v "$PWD":"$PWD" -w "$PWD" shwestrick/mpl make
```

### macOS

Only single-core is supported in macOS by using the MLton compiler. The `make` command will simply detect your OS and use MLton accordingly:

```bash
git clone https://github.com/heron-solver/heron.git
cd heron
make
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

#### Hide non-ticking primitives explicitly displayed
 Just change in `lib/TESL.sty` the color gradient of `nontickcolor` as
```
\definecolor{nontickcolor}{gray}{1.0}
```
Then, recompile your output.

### Runtime 

Trace generation may take some time depending on size and defined settings. To output the run diagram incrementally, you can set the argument `--runtime-print`. For instance

```bash
./heron  --use examples/reference-gallery/ConcurrentComputations.tesl --runtime-print
```

Model-checking against CTL (experimental)
-------------------
This work-in-progress explores the idea that generated TESL run traces seem to follow some recurrence pattern (provided some timestamps are abstracted). This allows to abstract a run tree (i.e., an infinite-state machine) into a finite-state machine. Each vertex of the machine contains a run instant. Hence, at each run tree unfolding (i.e., each step generation), the leaves are compared to previously-generated ones to find an exact match (and hence create an ε-transition). Whenever all leaves are refolded, the model is created.

In the theoretical-side, this refolding principle is only possible if we abstract timestamps from `time delayed` expressions. Indeed, timestamps in such expressions are expected to be infinitely-different at each run step generation (as time needs to flow and increase). The consequence of this apparatus is losing completeness: the model-checker may only answer "Yes, this TESL specification models this CTL formula" or "I don't know".

To build the model-checker, type
```bash
make hmc
```

The model-checker can be invoked on a small example with
```bash
./hmc --use ModelChecking.tesl
```
