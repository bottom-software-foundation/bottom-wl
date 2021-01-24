# bottom-wl
An implementation of the humorous [Bottom encoding format](https://github.com/bottom-software-foundation/spec) in Mathematica's Wolfram Language, using WolframScript so that it can be run from the command line.

Script usage:
```sh
wolframscript BottomSF.wls [--bottomify/--regress] input_file output_file
```
Only one of `--bottomify` or `--regress` should be used.

`input_file` and `output_file` can be a simple filename or a full path.
