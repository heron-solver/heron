
Heron Reference Manual
===================

This manual highlights specific features of the Heron solver that are undocumented in the help page. For the TESL language itself, please refer to the [TESL Reference](http://wdi.supelec.fr/software/TESL/RefMan).

```bash
./heron --help
```

Run diagrams
-------------------

### Output in terminal

Outputs can be made with the directive `@print`.

### LaTeX formatting

It is also possible to produce a run diagram in LaTeX by adding at the end of the specification the directive

```
@output tex
```

A file named `output.tex` will be produced in the local directory. To compile it, please use `document.tex` (provided in `lib/`) in the same directory

```bash
pdflatex document.tex
```

### Runtime 

Run generation may take some time depending on size and defined settings. To output the run diagram incrementally, you can set the argument `--runtime-print`. For instance

```bash
./heron  --use examples/reference-gallery/ConcurrentComputations.tesl --runtime-print
```

