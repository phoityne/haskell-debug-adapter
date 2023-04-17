
# Haskell Debug Adapter

A [debug adapter](https://microsoft.github.io/debug-adapter-protocol/) for Haskell debugging system.

![101_deploy.png](https://raw.githubusercontent.com/phoityne/haskell-debug-adapter/master/docs/design/101_deploy.png)

Started development based on [phoityne-vscode-0.0.28.0](https://hackage.haskell.org/package/phoityne-vscode).  
Changed package name (because a name "phoityne-vscode" is ambiguous.), and with some refactoring.

* Haskell Debugger
  * haskell-debug-adapter  
    This library.
  * [haskell-dap](https://github.com/phoityne/haskell-dap)  
    Haskell implementation of DAP interface data.
  * [ghci-dap](https://github.com/phoityne/ghci-dap)  
    A GHCi having DAP interface.

* Debug adapter clients
  * [phoityne-vscode](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)([hdx4vsc](https://github.com/phoityne/hdx4vsc))  
    An extension for VSCode.
  * [hdx4vim](https://github.com/phoityne/hdx4vim)  
    This is just a configuration for the [vimspector](https://github.com/puremourning/vimspector) which is a debug adapter client of Vim.   
  See a sample configuration.
  * [hdx4emacs](https://github.com/phoityne/hdx4emacs)  
    This is just a configuration for dap-mode of Emacs.  
    See a sample configuration.
  * [hdx4vs](https://github.com/phoityne/hdx4vs)  
    An extension for Visual Studio.
  * [haskell-tools.nvim](https://github.com/mrcjkb/haskell-tools.nvim)
    A Neovim plugin that discovers launch configurations for [nvim-dap](https://github.com/mfussenegger/nvim-dap)
    from cabal and stack projects, and from Visual Studio Code launch.json files.

# Requirement
  - haskell-dap
  - ghci-dap

Install these libraries at once.

```
> stack install haskell-dap ghci-dap haskell-debug-adapter
```


# Limitation
Currently this project is an __experimental__ design and implementation.

* The source file extension must be ".hs"
* Can not use STDIN handle while debugging. 


