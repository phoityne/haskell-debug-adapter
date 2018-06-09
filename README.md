

# Phoityne VSCode

Phoityne is a Haskell GHCi debug adapter for Visual Studio Code.


## Information
* [2018/06/01] phoityne-vscode released.  
  * Marketplace [phoityne-vscode-0.0.19](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)
  * hackage [phoityne-vscode-0.0.24.0](https://hackage.haskell.org/package/phoityne-vscode)  
  __Need update from hackage !!.__
* Release Summary
  * [UPDATE] supported haskell-dap-0.0.5.0

![10_quick_start.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/10_quick_start.gif)  
(This sample project is available from [here](https://github.com/phoityne/stack-project-template).)


## Important

* __LIMITATION__: Source file extension must be ".hs"
* __LIMITATION__: Can not use STDIN handle while debugging. 
* When you start debugging for the first time, .vscode/tasks.json will be created automatically. Then you can use F6, F7, F8 shortcut key.
  * F5 : start debug
  * F6 : show command menu (for stack watch)
  * Shift + F6 : stop stack watch
  * F7 : stack clean & build
  * F8 : stack test
  * F9 : put bp on current line
  * Shift + F9 : put bp on current column
* While debugging, you can use F5, F9, F10, F11 shortcut key.
  * F5 : jump to next bp
  * F9 : put bp on the line
  * Shift + F9 : put bp on the column
  * F10 : step next
  * F11 : step into

  
## Features

### Run to Cursor

![03_run_to_cursor.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/03_run_to_cursor.gif)


### Bindings & Watch

The variable added to watch will be forced.

![02_watch.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/02_watch.gif)


### Stack trace

![05_stacktrace.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/05_stacktrace.gif)


### Break condition

![04_condition.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/04_condition.gif)

### Hit count break condition

Supports these operators.
*  ==
*  /=
*  <, >
*  <=, >=
*  mod, %
*  just digit is same with '>='

![07_hit_count.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/07_hit_count.gif)

### Break on Exception

![08_exception.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/08_exception.gif)


### Repl & Completions

![06_repl.gif](https://raw.githubusercontent.com/phoityne/phoityne-vscode/master/docs/06_repl.gif)

### and more

Better inspection. This is an experimental enhancement.  
There are limitations and additional installation.  
[Here are the details](https://github.com/phoityne/haskell-dap). 

![01_inspect_variables.gif](https://raw.githubusercontent.com/phoityne/haskell-dap/master/docs/01_inspect_variables.gif)  

## Capabilites

* supportsConfigurationDoneRequest : **yes**
* supportsFunctionBreakpoints : **yes**
* supportsConditionalBreakpoints : **yes**
* supportsHitConditionalBreakpoints : **yes**
* supportsEvaluateForHovers : **yes**
* exceptionBreakpointFilters : **yes**
* supportsStepBack : no
* supportsSetVariable : no
* supportsRestartFrame : no
* supportsGotoTargetsRequest : no
* supportsStepInTargetsRequest : no
* supportsCompletionsRequest : **yes**
* supportsModulesRequest : no
* additionalModuleColumns : no
* supportedChecksumAlgorithms : no
* supportsRestartRequest : no
* supportsExceptionOptions : no
* supportsValueFormattingOptions : no
* supportsExceptionInfoRequest : no
* supportTerminateDebuggee : no
* supportsDelayedStackTraceLoading : no
* supportsLogPoints : **yes** (by haskell-dap)


## Install


### Run stack install

    % stack install phoityne-vscode
      . . . . .
    %

Add 'phoityne-vscode.exe' to PATH environment.

Windows)

    % where $path:phoityne-vscode.exe
    C:\Users\[user name]\AppData\Roaming\local\bin\phoityne-vscode.exe
    
    % phoityne-vscode --version
    phoityne-vscode-x.x.x.x
    %
    % code

linux)

    $ which phoityne-vscode
    ~/.local/bin/phoityne-vscode
    $
    $ phoityne-vscode --version
    phoityne-vscode-x.x.x.x
    $
    $ code

### Install vscode extensions

1. run VSCode and open stack project __Folder__ from file menu. 
2. open Extensions from side menu of VSCode.
3. search "haskell" 
4. select "[__Haskell GHCi debug adapter Phoityne__](https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode)"

  

## Configuration

### __.vscode/launch.json__

|NAME|REQUIRED OR OPTIONAL|DEFAULT SETTING|DESCRIPTION|
|:--|:--:|:--|:--|
|startup|required|${workspaceRoot}/test/Spec.hs|debug startup file, will be loaded automatically.|
|startupFunc|optional|"" (empty string)|debug startup function, will be run instead of main function.|
|startupArgs|optional|"" (empty string)|arguments for startup function. set as string type.|
|ghciCmd|required|stack ghci --test --no-load --no-build --main-is TARGET --ghci-options -fprint-evld-with-show|launch ghci command, must be Prelude module loaded. For example, "ghci -i${workspaceRoot}/src", "cabal exec -- ghci -i${workspaceRoot}/src"|
|ghciPrompt|required|H>>=|ghci command prompt string.|
|ghciInitialPrompt|optional|"Prelude> "|initial pormpt of ghci. set it when using custom prompt. e.g. set in .ghci|
|stopOnEntry|required|true|stop or not after debugger launched.
|mainArgs|optional|"" (empty string)|main arguments.|
|logFile|required|${workspaceRoot}/.vscode/phoityne.log|internal log file.|
|logLevel|required|WARNING|internal log level.|


#### changing ghci initial prompt 

If you change ghci prompt in .ghci file, or ghci prompt is changed from "Prelude>" by applying _NoImplicitPrelude_ extension, set the initial prompt variable to same prompt string.

    % diff .vscode/launch.json.old .vscode/launch.json
    19c19
    <             "ghciInitialPrompt": "Prelude> "      // default value.
    ---
    >             "ghciInitialPrompt": "> "             // e.g.
    %

Make sure needs of the last space, and don't forget adding it.


#### setting the startup hs file

Set the startup variable to the path of .hs file in which main function is defined.

    % diff .vscode/launch.json.old .vscode/launch.json
    10c10
    <             "startup": "${workspaceRoot}/test/Spec.hs",    // default value.
    ---
    >             "startup": "${workspaceRoot}/app/run.hs",     // e.g.
    %


#### setting the startup function

If you want to run the specific function instead of main function, set the startupFunc variable.  
For example, when specifying the following startDebug function,

    startDebug :: String -> IO ()
    startDebug name = do
      putStrLn "hello"
      putStrLn name 

set the valiavles in the launch.json file.

    % diff .vscode/launch.json.old .vscode/launch.json
    11c12
    <             "startupFunc": "",    // default value.
    <             "startupArgs": "",    // default value.
    ---
    >             "startupFunc": "startDebug",       // e.g.
    >             "startupArgs": "\"phoityne\"",     // e.g.
    %


#### changing log level

For debuging phoityen itself, change the log level to DEBUG.  
Adding Issue with the debug log.

    % diff .vscode/launch.json.old .vscode/launch.json
    12c12
    <             "logLevel": "WARNING",               // default value.
    ---
    >             "logLevel": "DEBUG",                 // e.g.
    %


### __.vscode/tasks.json__

|TASK NAME|REQUIRED OR OPTIONAL|DEFAULT SETTING|DESCRIPTION|
|:--|:--:|:--|:--|
|stack build|required|stack build|task definition for F6 shortcut key.|
|stack clean & build|required|stack clean && stack build|task definition for F7 shortcut key.|
|stack test|required|stack test|task definition for F8 shortcut key.|
|stack watch|required|stack build --test --no-run-tests --file-watch|task definition for F6 shortcut key.|

