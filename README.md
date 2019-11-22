
# HDA : Haskell Debug Adapter

A [debug adapter](https://microsoft.github.io/debug-adapter-protocol/) for Haskell debugging system.

![101_deploy.png](https://raw.githubusercontent.com/phoityne/haskell-debug-adapter/master/docs/design/101_deploy.png)

Started development based on [phoityne-vscode-0.0.28.0](https://hackage.haskell.org/package/phoityne-vscode).  
Changed package name (because a name "phoityne-vscode" is ambiguous.), and with some refactoring.

# Requirement
  - haskell-dap
  - ghci-dap

Install them at once.

```
> stack install haskell-dap ghci-dap haskell-debug-adapter
```


# Limitation
Currently this project is an __experimental__ design and implementation.

* Dev and checked on windows10 ghc-8.6
* The source file extension must be ".hs"
* Can not use STDIN handle while debugging. 


# Configuration

## launch.json

  see : https://github.com/phoityne/hdx4vsc#vscodelaunchjson
