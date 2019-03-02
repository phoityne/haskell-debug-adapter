
# HDA : Haskell Debug Adapter

A [debug adapter](https://microsoft.github.io/debug-adapter-protocol/) for Haskell debugging system.

![101_deploy.png](https://raw.githubusercontent.com/phoityne/haskell-debug-adapter/master/docs/design/101_deploy.png)

Started developing based on [phoityne-vscode-0.0.28.0](https://hackage.haskell.org/package/phoityne-vscode).  
Changed package name (because a name "phoityne-vscode" is ambiguous.), and with some refactoring.

# Requirement
  - haskell-dap
  - ghci-dap

```
> stack install haskell-dap ghci-dap haskell-debug-adapter
```


# Limitation
Currently this project is an __experimental__ design and implementation.

* Dev and checked on windows10 ghc-8.4
* The source file extension must be ".hs"
* Can not use STDIN handle while debugging. 


# Configuration

## launch.json

|NAME|REQUIRED OR OPTIONAL|DEFAULT SETTING|DESCRIPTION|
|:--|:--:|:--|:--|
|startup|required|${workspaceRoot}/test/Spec.hs|debug startup file, will be loaded automatically.|
|startupFunc|optional|"" (empty string)|debug startup function, will be run instead of main function.|
|startupArgs|optional|"" (empty string)|arguments for startup function. set as string type.|
|ghciCmd|required|stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show|launch ghci command, must be Prelude module loaded. For example, "ghci -i${workspaceRoot}/src", "cabal exec -- ghci -i${workspaceRoot}/src"|
|ghciPrompt|required|H>>=|ghci command prompt string.|
|ghciInitialPrompt|optional|"Prelude> "|initial pormpt of ghci. set it when using custom prompt. e.g. set in .ghci|
|stopOnEntry|required|false|stop or not after debugger launched.
|mainArgs|optional|"" (empty string)|main arguments.|
|logFile|required|${workspaceRoot}/.vscode/phoityne.log|internal log file.|
|logLevel|required|WARNING|internal log level.|


