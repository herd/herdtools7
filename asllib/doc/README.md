# Some rules

## Empty sections

By default sections, subsections, and subsubsections etc.  are included
in the document. To get rid of them: `make no-empty`.

For the trick to work, the sectioning commands of empty section must
appear as follows:
```
\isempty{\section{...}}
```

## ``Formally'' material (sections)

By default, this material is included, to get rid of it: `make no-formal`

For the trick to work, the ``formal'' material must appear inside the `formal` environment.
```
\begin{formal}
...
\end{formal}
```

## And if I have ``empty'' and ``Formally'' sections?

Use the `emptyformal` environement:
```
\begin{emptyformal}
...
\end{emptyformal}
```
The contents of the `emptyformal` environement will reach output only in the default build `make all`.



## Getting rid of empty sections and ``Formally'' material

Just type `make short`.

