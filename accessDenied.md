## Fri 10-02-23
* 06:03 start
* 06:03 ghcid 
* 07:07 restart VSCode  
* 07:12 restart ghcid
* 08:33 hibernate 
* 09:08 unhibernate
* 09:34 hibernate
* 11:20 unhibernate
* 12:07 hibernate
* 12:45 unhibernate
* 16:25 hibernate

## Sat 11-02-23
* 07:19 unhibernate
* 14:40 hibernate
* 18:18 unhibernate
  
## Sat 12-02-23
* 07:01 unhibrenate
* 07:20 hibernate
* 14:00 unhibernate
* 14:22 
  
```
Error during compileC:\Users\thegh\AppData\Local\ghcide\main-c70615d89a7a639097b386336e61e4aed219e86e\Internal\ext42B9: renameFile:renamePath:MoveFileEx "\\\\?\\C:\\Users\\thegh\\AppData\\Local\\ghcide\\main-c70615d89a7a639097b386336e61e4aed219e86e\\Internal\\ext42B9" Just "\\\\?\\C:\\Users\\thegh\\AppData\\Local\\ghcide\\main-c70615d89a7a639097b386336e61e4aed219e86e\\Internal\\SuiteRuntime.hi.core": permission denied (Access is denied.)compile

suitruntime edited earlier 
+ syntax error in suitruntime test 
C:\Pyrethrum\test\SuiteRuntimeTest.hs:273:25-29: error:
    * Variable not in scope: accum :: M.Map Loc b5
    * Perhaps you meant `accm' (line 236)
    |
273 |                         accum -- fixture

    |                         ^^^^^

=> fix syntax error
=> set main-c70615d89a7a639097b386336e61e4aed219e86e readOnly checkbox to false in windows explorer and apply to subfolders
=> restart HLS
 
```
## Mon 13-02-2023
* 05:45 unhibernate - seems to be stuck processing 6//60
* 06:40 - restart vscode
* 07:04 - hibernate

## Wed 16-02-23
* 05:55 unhibernate
* 06:45 whilst editing SuitRuntimeTest.hs
  * Note: ghcid not running

```
Error during compileC:\Users\thegh\AppData\Local\ghcide\main-c70615d89a7a639097b386336e61e4aed219e86e\Internal\ext5C55: renameFile:renamePath:MoveFileEx "\\\\?\\C:\\Users\\thegh\\AppData\\Local\\ghcide\\main-c70615d89a7a639097b386336e61e4aed219e86e\\Internal\\ext5C55" Just "\\\\?\\C:\\Users\\thegh\\AppData\\Local\\ghcide\\main-c70615d89a7a639097b386336e61e4aed219e86e\\Internal\\SuiteRuntime.hi.core": permission denied (Access is denied.)compile

hls log
2023-02-12 19:37:33.5620000 [client] INFO Finding haskell-language-server
2023-02-12 19:37:33.5700000 [client] INFO Checking for ghcup installation
2023-02-12 19:37:34.5800000 [client] INFO found ghcup at ghcup
2023-02-12 19:37:34.5860000 [client] INFO Executing 'ghcup --no-verbose upgrade' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:37:44.6600000 [client] INFO Checking for ghcup installation
2023-02-12 19:37:45.5680000 [client] INFO found ghcup at ghcup
2023-02-12 19:37:45.5740000 [client] INFO Executing 'ghcup --no-verbose list -t hls -c installed -r' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:37:47.5990000 [client] INFO Checking for ghcup installation
2023-02-12 19:37:48.3830000 [client] INFO found ghcup at ghcup
2023-02-12 19:37:48.3880000 [client] INFO Executing 'ghcup --no-verbose list -t cabal -c installed -r' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:37:49.7240000 [client] INFO Checking for ghcup installation
2023-02-12 19:37:50.5620000 [client] INFO found ghcup at ghcup
2023-02-12 19:37:50.5660000 [client] INFO Executing 'ghcup --no-verbose list -t stack -c installed -r' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:38:06.3940000 [client] INFO Checking for ghcup installation
2023-02-12 19:38:07.1330000 [client] INFO found ghcup at ghcup
2023-02-12 19:38:07.1390000 [client] INFO Executing 'ghcup --no-verbose whereis hls 1.9.0.0' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:38:15.0540000 [client] INFO Checking for ghcup installation
2023-02-12 19:38:16.1370000 [client] INFO found ghcup at ghcup
2023-02-12 19:38:16.1410000 [client] INFO Executing 'ghcup --no-verbose whereis cabal 3.6.2.0' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:38:18.3070000 [client] INFO Checking for ghcup installation
2023-02-12 19:38:19.1390000 [client] INFO found ghcup at ghcup
2023-02-12 19:38:19.1430000 [client] INFO Executing 'ghcup --no-verbose whereis stack 2.9.1' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:38:20.9880000 [client] INFO Executing 'ghc.exe --numeric-version' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:38:22.0500000 [client] INFO Checking for ghcup installation
2023-02-12 19:38:23.0090000 [client] INFO found ghcup at ghcup
2023-02-12 19:38:23.0130000 [client] INFO Executing 'ghcup --no-verbose run --hls 1.9.0.0 --cabal 3.6.2.0 --stack 2.9.1 --install' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:38:26.8150000 [client] INFO Working out the project GHC version. This might take a while...
2023-02-12 19:38:26.8250000 [client] INFO Executing 'haskell-language-server-wrapper --project-ghc-version' in cwd 'c:\Pyrethrum'
2023-02-12 19:39:06.8970000 [client] INFO The GHC version for the project or file: 9.0.2
2023-02-12 19:39:06.9940000 [client] INFO Reading cached release data at c:\Users\thegh\AppData\Roaming\Code\User\globalStorage\haskell.haskell\ghcupReleases.cache.json
2023-02-12 19:39:07.0040000 [client] INFO Platform constants: Windows, A_64
2023-02-12 19:39:07.0060000 [client] INFO Checking for ghcup installation
2023-02-12 19:39:07.7230000 [client] INFO found ghcup at ghcup
2023-02-12 19:39:07.7280000 [client] INFO Executing 'ghcup --no-verbose list -t hls -c installed -r' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:39:08.2950000 [client] INFO Checking for ghcup installation
2023-02-12 19:39:09.0160000 [client] INFO found ghcup at ghcup
2023-02-12 19:39:09.0190000 [client] INFO Executing 'ghcup --no-verbose whereis bindir' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:39:09.3050000 [client] INFO Checking for ghcup installation
2023-02-12 19:39:10.0080000 [client] INFO found ghcup at ghcup
2023-02-12 19:39:10.0120000 [client] INFO Executing 'ghcup --no-verbose whereis hls 1.9.0.0' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:39:10.2910000 [client] INFO Checking for ghcup installation
2023-02-12 19:39:10.9950000 [client] INFO found ghcup at ghcup
2023-02-12 19:39:11.0000000 [client] INFO Executing 'ghcup --no-verbose whereis ghc 9.0.2' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:39:11.2740000 [client] INFO Checking for ghcup installation
2023-02-12 19:39:11.9810000 [client] INFO found ghcup at ghcup
2023-02-12 19:39:11.9860000 [client] INFO Executing 'ghcup --no-verbose run --hls 1.9.0.0 --cabal 3.6.2.0 --stack 2.9.1 --ghc 9.0.2 --install' in cwd 'C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code'
2023-02-12 19:39:15.3260000 [client] INFO Activating the language server in working dir: c:\Pyrethrum (the workspace folder)
2023-02-12 19:39:15.3300000 [client] INFO run command: C:\ghcup\tmp\ghcup-ghc-9.0.2_cabal-3.6.2.0_hls-1.9.0.0_stack-2.9.1\haskell-language-server-wrapper.exe --lsp
2023-02-12 19:39:15.3300000 [client] INFO debug command: C:\ghcup\tmp\ghcup-ghc-9.0.2_cabal-3.6.2.0_hls-1.9.0.0_stack-2.9.1\haskell-language-server-wrapper.exe --lsp
2023-02-12 19:39:15.3300000 [client] INFO server environment variables:
2023-02-12 19:39:15.3300000 [client] INFO   PATH=C:\ghcup\tmp\ghcup-ghc-9.0.2_cabal-3.6.2.0_hls-1.9.0.0_stack-2.9.1;C:\Python39\Scripts\;C:\Python39\;C:\Python27\;C:\Python27\Scripts;C:\ProgramData\Oracle\Java\javapath;C:\Program Files\Dell\DW WLAN Card;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Program Files\WIDCOMM\Bluetooth Software\;C:\Program Files\WIDCOMM\Bluetooth Software\syswow64;C:\Program Files (x86)\NVIDIA Corporation\PhysX\Common;C:\Program Files\TortoiseHg\;C:\Users\thegh\.dnx\bin;C:\Program Files\Microsoft DNX\Dnvm\;C:\Program Files\Microsoft SQL Server\130\Tools\Binn\;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\Program Files (x86)\scala\bin;C:\Users\thegh\AppData\Roaming\npm;C:\Program Files (x86)\Pandoc\;C:\Program Files (x86)\sbt\bin;C:\ProgramData\chocolatey\bin;C:\ProgramData\osquery;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem;C:\WINDOWS\System32\WindowsPowerShell\v1.0\;C:\Program Files\Java\jdk1.8.0_131\bin;C:\Program Files\watchman;C:\Android\android-sdk\tools;C:\Android\android-sdk\platform-tools;C:\WINDOWS\System32\OpenSSH\;C:\Program Files\Microsoft VS Code\bin;C:\Program Files (x86)\Yarn\bin\;C:\Program Files\nodejs\;C:\Program Files\PowerShell\6\;C:\Program Files\TortoiseGit\bin;C:\Program Files\dotnet\;C:\Program Files (x86)\dotnet\;C:\Program Files (x86)\Intel\Intel(R) Management Engine Components\DAL;C:\Program Files\Intel\Intel(R) Management Engine Components\DAL;C:\Program Files\PuTTY\;C:\Program Files\Graphviz\bin;C:\Program Files\Docker\Docker\resources\bin;C:\ProgramData\DockerDesktop\version-bin;C:\Program Files\NVIDIA Corporation\NVIDIA NvDLISR;C:\Program Files\Git\cmd;C:\ghcup\bin;C:\Users\thegh\.dotnet\tools;C:\Program Files\dotnet\;C:\Program Files\dotnet\dotnet.exe;C:\Ruby25-x64\bin;C:\Users\thegh\AppData\Roaming\local\bin\stack.exe;C:\Users\thegh\AppData\Local\atom\bin;C:\Users\thegh\AppData\Roaming\local\bin\;C:\Users\thegh\AppData\Local\Microsoft\WindowsApps;C:\Program Files\Git\bin;C:\Program Files\Git\cmd;C:\Program Files\gheckoWebDriver;C:\Users\thegh\AppData\Local\Programs\stack\x86_64-windows\ghc-8.2.2\bin;C:\Program Files\Hasktags;C:\Program Files\Microsoft VS Code\bin;C:\Program Files (x86)\Oni;C:\Users\thegh\AppData\Local\Yarn\bin;C:\Users\thegh\AppData\Local\Programs\Microsoft VS Code\bin;C:\Users\thegh\AppData\Roaming\npm;C:\Users\thegh\AppData\Roaming\npm\node_modules\typescript\bin;C:\Users\thegh\AppData\Local\GitHubDesktop\bin;C:\Users\thegh\AppData\Local\Pandoc\;
2023-02-12 19:39:15.3520000 [client] INFO Starting language server
Found "c:\Pyrethrum\hie.yaml" for "c:\Pyrethrum\a"
Run entered for haskell-language-server-wrapper(haskell-language-server-wrapper-1.9.0.0.exe) Version 1.9.0.0 x86_64 ghc-9.4.4
Current directory: c:\Pyrethrum
Operating system: mingw32
Arguments: ["--lsp"]
Cradle directory: c:\Pyrethrum
Cradle type: Stack

Tool versions found on the $PATH
cabal:          3.6.2.0
stack:          2.9.1
ghc:            9.0.1


Consulting the cradle to get project GHC version...
Project GHC version: 9.0.2
haskell-language-server exe candidates: ["haskell-language-server-9.0.2.exe","haskell-language-server.exe"]
Launching haskell-language-server exe at:C:\ghcup\tmp\ghcup-ghc-9.0.2_cabal-3.6.2.0_hls-1.9.0.0_stack-2.9.1\haskell-language-server-9.0.2.exe
2023-02-12T19:39:41.763359Z | Info | No log file specified; using stderr.
2023-02-12T19:39:41.768358Z | Info | haskell-language-server version: 1.9.0.0 (GHC: 9.0.2) (PATH: C:\ghcup\bin\haskell-language-server-9.0.2~1.9.0.0.exe)
2023-02-12T19:39:41.771348Z | Info | Directory: c:\Pyrethrum
2023-02-12T19:39:41.773356Z | Info | Starting (haskell-language-server) LSP server...
  GhcideArguments {argsCommand = LSP, argsCwd = Nothing, argsShakeProfiling = Nothing, argsTesting = False, argsExamplePlugin = False, argsDebugOn = False, argsLogFile = Nothing, argsThreads = 0, argsProjectGhcVersion = False}
  PluginIds: [ pragmas
             , LSPRecorderCallback
             , rename
             , ghcide-completions
             , class
             , refineImports
             , splice
             , cabal
             , changeTypeSignature
             , qualifyImportedNames
             , alternateNumberFormat
             , hlint
             , cabalfmt
             , explicit-fields
             , ghcide-code-actions-fill-holes
             , floskell
             , ghcide-extend-import-action
             , codeRange
             , haddockComments
             , importLens
             , retrie
             , ghcide-type-lenses
             , ghcide-code-actions-imports-exports
             , ghcide-hover-and-symbols
             , eval
             , gadt
             , fourmolu
             , tactics
             , callHierarchy
             , stylish-haskell
             , ghcide-code-actions-type-signatures
             , ghcide-code-actions-bindings
             , moduleName
             , ormolu
             , ghcide-core
             , explicit-fixity ]
2023-02-12T19:39:41.800338Z | Info | Logging heap statistics every 60.00s
 2023-02-12T19:39:41.831345Z | Info | Starting LSP server...
  If you are seeing this in a terminal, you probably should have run WITHOUT the --lsp option!
  PluginIds: [ pragmas
             , LSPRecorderCallback
             , rename
             , ghcide-completions
             , class
             , refineImports
             , splice
             , cabal
             , changeTypeSignature
             , qualifyImportedNames
             , alternateNumberFormat
             , hlint
             , cabalfmt
             , explicit-fields
             , ghcide-code-actions-fill-holes
             , floskell
             , ghcide-extend-import-action
             , codeRange
             , haddockComments
             , importLens
             , retrie
             , ghcide-type-lenses
             , ghcide-code-actions-imports-exports
             , ghcide-hover-and-symbols
             , eval
             , gadt
             , fourmolu
             , tactics
             , callHierarchy
             , stylish-haskell
             , ghcide-code-actions-type-signatures
             , ghcide-code-actions-bindings
             , moduleName
             , ormolu
             , ghcide-core
             , explicit-fixity ]
2023-02-12T19:39:41.834339Z | Info | Starting server
2023-02-12T19:39:41.862341Z | Info | Started LSP server in 0.03s
2023-02-12T19:40:29.830689Z | Info | Registering IDE configuration: IdeConfiguration {workspaceFolders = fromList [NormalizedUri 577147563957033084 "file:///C:/Pyrethrum"], clientSettings = hashed Nothing}
[Info  - 6:40:29 AM] haskell-language-server version: 1.9.0.0 (GHC: 9.0.2) (PATH: C:\ghcup\bin\haskell-language-server-9.0.2~1.9.0.0.exe)
[Info  - 6:40:29 AM] Directory: c:\Pyrethrum
[Info  - 6:40:29 AM] Starting (haskell-language-server) LSP server...
  GhcideArguments {argsCommand = LSP, argsCwd = Nothing, argsShakeProfiling = Nothing, argsTesting = False, argsExamplePlugin = False, argsDebugOn = False, argsLogFile = Nothing, argsThreads = 0, argsProjectGhcVersion = False}
  PluginIds: [ pragmas
             , LSPRecorderCallback
             , rename
             , ghcide-completions
             , class
             , refineImports
             , splice
             , cabal
             , changeTypeSignature
             , qualifyImportedNames
             , alternateNumberFormat
             , hlint
             , cabalfmt
             , explicit-fields
             , ghcide-code-actions-fill-holes
             , floskell
             , ghcide-extend-import-action
             , codeRange
             , haddockComments
             , importLens
             , retrie
             , ghcide-type-lenses
             , ghcide-code-actions-imports-exports
             , ghcide-hover-and-symbols
             , eval
             , gadt
             , fourmolu
             , tactics
             , callHierarchy
             , stylish-haskell
             , ghcide-code-actions-type-signatures
             , ghcide-code-actions-bindings
             , moduleName
             , ormolu
             , ghcide-core
             , explicit-fixity ]
[Info  - 6:40:29 AM] Logging heap statistics every 60.00s
[Info  - 6:40:29 AM] Starting LSP server...
  If you are seeing this in a terminal, you probably should have run WITHOUT the --lsp option!
  PluginIds: [ pragmas
             , LSPRecorderCallback
             , rename
             , ghcide-completions
             , class
             , refineImports
             , splice
             , cabal
             , changeTypeSignature
             , qualifyImportedNames
             , alternateNumberFormat
             , hlint
             , cabalfmt
             , explicit-fields
             , ghcide-code-actions-fill-holes
             , floskell
             , ghcide-extend-import-action
             , codeRange
             , haddockComments
             , importLens
             , retrie
             , ghcide-type-lenses
             , ghcide-code-actions-imports-exports
             , ghcide-hover-and-symbols
             , eval
             , gadt
             , fourmolu
             , tactics
             , callHierarchy
             , stylish-haskell
             , ghcide-code-actions-type-signatures
             , ghcide-code-actions-bindings
             , moduleName
             , ormolu
             , ghcide-core
             , explicit-fixity ]
[Info  - 6:40:29 AM] Starting server
[Info  - 6:40:29 AM] Started LSP server in 0.03s
[Info  - 6:40:29 AM] Registering IDE configuration: IdeConfiguration {workspaceFolders = fromList [NormalizedUri 577147563957033084 "file:///C:/Pyrethrum"], clientSettings = hashed Nothing}
2023-02-12T19:40:30.141026Z | Info | Cradle path: test\SuiteRuntimeTest.hs
[Info  - 6:40:30 AM] Cradle path: test\SuiteRuntimeTest.hs
2023-02-12T19:40:41.809078Z | Info | Live bytes: 0.00MB Heap size: 0.00MB
[Info  - 6:40:41 AM] Live bytes: 0.00MB Heap size: 0.00MB
2023-02-12T19:41:41.812272Z | Info | Live bytes: 0.00MB Heap size: 0.00MB
[Info  - 6:41:41 AM] Live bytes: 0.00MB Heap size: 0.00MB
2023-02-12T19:42:41.828301Z | Info | Live bytes: 0.00MB Heap size: 0.00MB
[Info  - 6:42:41 AM] Live bytes: 0.00MB Heap size: 0.00MB
2023-02-12T19:43:06.356830Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:43:06.356830Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:43:06.356830Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:43:06.357826Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:43:06.357826Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:43:06 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:43:06 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:43:06 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:43:06 AM] retrie: InternalError: typecheck
[Warn  - 6:43:06 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:43:07.291880Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:43:07 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:43:07.353174Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:43:07.354175Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:43:07.354175Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:43:07.355180Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:43:07.355180Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:43:07 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:43:07 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:43:07 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:43:07 AM] retrie: InternalError: typecheck
[Warn  - 6:43:07 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:43:09.834466Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:43:09.834466Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:43:09.834466Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:43:09 AM] class: InternalError: Unable to typecheck
[Warn  - 6:43:09 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:43:09 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:43:41.847857Z | Info | Live bytes: 25.63MB Heap size: 578.81MB
[Info  - 6:43:41 AM] Live bytes: 25.63MB Heap size: 578.81MB
2023-02-12T19:44:02.287516Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:02.287516Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:02.287516Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:02.288509Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:02.288509Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:02 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:02 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:02 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:02 AM] retrie: InternalError: typecheck
[Warn  - 6:44:02 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:08.261909Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:08.262919Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:08.262919Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:08 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:08.262919Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:08.262919Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:08 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:08 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:08 AM] retrie: InternalError: typecheck
[Warn  - 6:44:08 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:11.499657Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:44:11.499657Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:44:11.499657Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:44:11 AM] class: InternalError: Unable to typecheck
[Warn  - 6:44:11 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:44:11 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:44:11.663198Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:11.663198Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:11.663198Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:11.663198Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:11.663198Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:11 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:11 AM] retrie: InternalError: typecheck
[Warn  - 6:44:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:13.103548Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:13.103548Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:13.103548Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:13.103548Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:13.103548Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:13 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:13 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:13 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:13 AM] retrie: InternalError: typecheck
[Warn  - 6:44:13 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:13.723244Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:13.723244Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:13.723244Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:13.723244Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:13.723244Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:13 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:13 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:13 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:13 AM] retrie: InternalError: typecheck
[Warn  - 6:44:13 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:17.132320Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:17.132320Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:17.132320Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:17.133323Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:17.133323Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:17 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:17 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:17 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:17 AM] retrie: InternalError: typecheck
[Warn  - 6:44:17 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:26.912594Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:44:26.913589Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:44:26.913589Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:44:26 AM] class: InternalError: Unable to typecheck
[Warn  - 6:44:26 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:44:26 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:44:27.050586Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:27.050586Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:27.050586Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:27.050586Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:27.050586Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:27 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:27 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:27 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:27 AM] retrie: InternalError: typecheck
[Warn  - 6:44:27 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:40.214504Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:44:40.215500Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:44:40.215500Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:44:40.215500Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:44:40.215500Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:44:40 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:44:40 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:44:40 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:44:40 AM] retrie: InternalError: typecheck
[Warn  - 6:44:40 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:44:41.864290Z | Info | Live bytes: 25.63MB Heap size: 578.81MB
[Info  - 6:44:41 AM] Live bytes: 25.63MB Heap size: 578.81MB
2023-02-12T19:45:13.687388Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:45:13.687388Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:45:13.688386Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:45:13.688386Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:45:13.688386Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:45:13 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:45:13 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:45:13 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:45:13 AM] retrie: InternalError: typecheck
[Warn  - 6:45:13 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:45:41.881780Z | Info | Live bytes: 25.63MB Heap size: 578.81MB
[Info  - 6:45:41 AM] Live bytes: 25.63MB Heap size: 578.81MB
2023-02-12T19:46:11.128320Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:46:11.128320Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:46:11.129326Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:46:11.129326Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:46:11.129326Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:11 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:46:11 AM] retrie: InternalError: typecheck
[Warn  - 6:46:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:46:11.547416Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:46:11.547416Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:46:11.547416Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:46:11.547416Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:46:11.547416Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:11 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:46:11 AM] retrie: InternalError: typecheck
[Warn  - 6:46:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:46:41.902276Z | Info | Live bytes: 25.63MB Heap size: 578.81MB
[Info  - 6:46:41 AM] Live bytes: 25.63MB Heap size: 578.81MB
2023-02-12T19:46:48.436913Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:46:48.436913Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:46:48.436913Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:46:48 AM] class: InternalError: Unable to typecheck
[Warn  - 6:46:48 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:46:48 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:46:48.599699Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:46:48.599699Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:46:48.600700Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:46:48.600700Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:46:48.600700Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:48 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:48 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:48 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:46:48 AM] retrie: InternalError: typecheck
[Warn  - 6:46:48 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:09.609870Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:09.609870Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:09.609870Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:09.610879Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:09.610879Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:09 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:09 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:09 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:09 AM] retrie: InternalError: typecheck
[Warn  - 6:47:09 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:10.599443Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:10.599443Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:10.599443Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:10.600454Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:10.600454Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:10 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:10 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:10 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:10 AM] retrie: InternalError: typecheck
[Warn  - 6:47:10 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:12.294286Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:12.294286Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:12.294286Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:12.295246Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:12.295246Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:12 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:12 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:12 AM] retrie: InternalError: typecheck
[Warn  - 6:47:12 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:14.834093Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:14.834093Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:14.834093Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:14.834093Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:14.835096Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:14 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:14 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:14 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:14 AM] retrie: InternalError: typecheck
[Warn  - 6:47:14 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:16.009904Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:16.009904Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:16.009904Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:16 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:16 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:16 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:16.619226Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:16 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:17.536731Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:17.536731Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:17.536731Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:17 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:17 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:17 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:17.558396Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:17.558396Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:17.559398Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:17.559398Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:17.559398Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:17 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:17 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:17 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:17 AM] retrie: InternalError: typecheck
[Warn  - 6:47:17 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:17.757648Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:17 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:17.983306Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:17.984304Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:17.984304Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:17.984304Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:17.984304Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:17 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:17 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:17 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:17 AM] retrie: InternalError: typecheck
[Warn  - 6:47:17 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:19.541752Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:19.542747Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:19.542747Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:19 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:19 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:19 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:19.575809Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:19.575809Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:19.576816Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:19.576816Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:19.576816Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:19 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:19 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:19 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:19 AM] retrie: InternalError: typecheck
[Warn  - 6:47:19 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:19.647366Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:19 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:24.132452Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:24.132452Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:24.132452Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:24.134538Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:24 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:24 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:24 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:24 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:24.899919Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:24 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:24.904920Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:24.904920Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:24.904920Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:24 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:24 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:24 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:26.876660Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:26 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:26.916662Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:26.917663Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:26.917663Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:26 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:26 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:26 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:26.932967Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:26.933967Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:26.933967Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:26.933967Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:26.933967Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:26 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:26 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:26 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:26 AM] retrie: InternalError: typecheck
[Warn  - 6:47:26 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:27.277573Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:27 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:27.350142Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:27.351135Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:27.351135Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:27 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:27 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:27 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:27.372134Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:27.373134Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:27.373134Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:27.373134Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:27.373134Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:27 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:27 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:27 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:27 AM] retrie: InternalError: typecheck
[Warn  - 6:47:27 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:29.082257Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:29.082257Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:29.082257Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:29.082257Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:29.082257Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:29 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:29 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:29 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:29 AM] retrie: InternalError: typecheck
[Warn  - 6:47:29 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:30.838552Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:30.838552Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:30.838552Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:30.838552Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:30.838552Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:30 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:30 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:30 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:30 AM] retrie: InternalError: typecheck
[Warn  - 6:47:30 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:31.353975Z | Info | Cradle path: src\Internal\RunTimeLogging.hs
[Info  - 6:47:31 AM] Cradle path: src\Internal\RunTimeLogging.hs
2023-02-12T19:47:32.569500Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:32.569500Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:32.569500Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:32 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:32 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:32 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:32.680197Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:32.680197Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:32.680197Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:32.680197Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:32.680197Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:32 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:32 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:32 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:32 AM] retrie: InternalError: typecheck
[Warn  - 6:47:32 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:34.060929Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:34 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:34.072939Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:34.073931Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:34 AM] class: InternalError: Unable to typecheck
2023-02-12T19:47:34.073931Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:34 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:34 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:34.092939Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:34.092939Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:34.092939Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:34.092939Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:34.093934Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:34 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:34 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:34 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:34 AM] retrie: InternalError: typecheck
[Warn  - 6:47:34 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:36.944104Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:36 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:37.152274Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:37.152274Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:37.153276Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:37 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:37 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:37 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:37.171274Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:37.171274Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:37.172275Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:37 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:37.172275Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:37.172275Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:37 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:37 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:37 AM] retrie: InternalError: typecheck
[Warn  - 6:47:37 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:38.556579Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:38.557578Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:38.557578Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:38.557578Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:38.558573Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:38 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:38 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:38 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:38 AM] retrie: InternalError: typecheck
[Warn  - 6:47:38 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:38.951433Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:38.951433Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:38.951433Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:38.952441Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:38.952441Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:38 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:38 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:38 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:38 AM] retrie: InternalError: typecheck
[Warn  - 6:47:38 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:40.264286Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:40 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:40.342283Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:40 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:40.431281Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:40.431281Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:40.432282Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:40 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:40 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:40 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:41.746057Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:47:41 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:47:41.810086Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:41.810086Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:41.810086Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:41 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:41 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:41 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:41.831091Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:41.832086Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:41.832086Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:41.832086Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:41.832086Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:41 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:41 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:41 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:41 AM] retrie: InternalError: typecheck
[Warn  - 6:47:41 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:41.912244Z | Info | Live bytes: 25.63MB Heap size: 578.81MB
[Info  - 6:47:41 AM] Live bytes: 25.63MB Heap size: 578.81MB
2023-02-12T19:47:43.434507Z | Info | Could not identify reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs"
[Info  - 6:47:43 AM] Could not identify reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs"
2023-02-12T19:47:43.970521Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:47:43.970521Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:47:43.970521Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:47:43 AM] class: InternalError: Unable to typecheck
[Warn  - 6:47:43 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:47:43 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:47:44.886491Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:44.887489Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:44.887489Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:44.887489Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:44.887489Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:44 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:44 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:44 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:44 AM] retrie: InternalError: typecheck
[Warn  - 6:47:44 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:47:46.738083Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:47:46.739084Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:47:46.739084Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:47:46.740084Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:47:46.740084Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:47:46 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:47:46 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:47:46 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:47:46 AM] retrie: InternalError: typecheck
[Warn  - 6:47:46 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:04.031654Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:04 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:04.264630Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:48:04.264630Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:48:04.265632Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:48:04.265632Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:48:04.265632Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:48:04 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:48:04 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:48:04 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:48:04 AM] retrie: InternalError: typecheck
[Warn  - 6:48:04 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:04.422754Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:04 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:04.810599Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:48:04.810599Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:48:04.811601Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:48:04 AM] class: InternalError: Unable to typecheck
[Warn  - 6:48:04 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:48:04 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:48:05.178650Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:05 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:05.586370Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:48:05.587369Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:48:05.587369Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:48:05 AM] class: InternalError: Unable to typecheck
[Warn  - 6:48:05 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:48:05 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:48:07.093957Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:07 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:07.363160Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:48:07.364213Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:48:07.364213Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:48:07 AM] class: InternalError: Unable to typecheck
[Warn  - 6:48:07 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:48:07 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:48:08.700869Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:08 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:09.274673Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:09 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:09.367606Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:48:09.368586Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:48:09.368586Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:48:09.368586Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:48:09.368586Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:48:09 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:48:09 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:48:09 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:48:09 AM] retrie: InternalError: typecheck
[Warn  - 6:48:09 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:09.799470Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:09 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:09.839472Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:48:09.839472Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:48:09.839472Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:48:09.840468Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:48:09.840468Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:48:09 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:48:09 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:48:09 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:48:09 AM] retrie: InternalError: typecheck
[Warn  - 6:48:09 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:10.205628Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:10 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:10.401832Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:48:10.401832Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:48:10.402805Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:48:10.402805Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:48:10.402805Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:48:10 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:48:10 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:48:10 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:48:10 AM] retrie: InternalError: typecheck
[Warn  - 6:48:10 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:10.464617Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:48:10.464617Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:48:10.464617Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:48:10 AM] class: InternalError: Unable to typecheck
[Warn  - 6:48:10 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:48:10 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:48:12.631124Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:48:12 AM] codeRange: bad dependency: GetCodeRange
2023-02-12T19:48:12.705800Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:48:12.706098Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:48:12.706098Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:48:12.706474Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:48:12.706474Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:48:12 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:48:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:48:12 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:48:12 AM] retrie: InternalError: typecheck
[Warn  - 6:48:12 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:12.900564Z | Warning | class: InternalError: Unable to typecheck
2023-02-12T19:48:12.900564Z | Warning | eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
2023-02-12T19:48:12.901556Z | Warning | moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
[Warn  - 6:48:12 AM] class: InternalError: Unable to typecheck
[Warn  - 6:48:12 AM] eval: InternalError: Exception in plugin PluginId "eval" while processing STextDocumentCodeLens: BadDependency "GetEvalComments"
[Warn  - 6:48:12 AM] moduleName: InternalError: Exception in plugin PluginId "moduleName" while processing STextDocumentCodeLens: BadDependency "GhcSession"
2023-02-12T19:48:14.050069Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-12T19:48:14.050069Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-12T19:48:14.051073Z | Warning | explicit-fields: InternalError: Could not get NextPragmaInfo
2023-02-12T19:48:14.051073Z | Warning | retrie: InternalError: typecheck
2023-02-12T19:48:14.051073Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:48:14 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:48:14 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:48:14 AM] explicit-fields: InternalError: Could not get NextPragmaInfo
[Warn  - 6:48:14 AM] retrie: InternalError: typecheck
[Warn  - 6:48:14 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-12T19:48:16.645418Z | Info | Interface files cache directory: C:\Users\thegh\AppData\Local\ghcide\main-c70615d89a7a639097b386336e61e4aed219e86e
2023-02-12T19:48:16.645418Z | Info | Making new HscEnv. In-place unit ids: [main]
[Info  - 6:48:16 AM] Interface files cache directory: C:\Users\thegh\AppData\Local\ghcide\main-c70615d89a7a639097b386336e61e4aed219e86e
[Info  - 6:48:16 AM] Making new HscEnv. In-place unit ids: [main]
2023-02-12T19:48:41.915039Z | Info | Live bytes: 49.91MB Heap size: 676.33MB
[Info  - 6:48:41 AM] Live bytes: 49.91MB Heap size: 676.33MB
2023-02-12T19:49:37.025544Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:49:37 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-12T19:49:41.931159Z | Info | Live bytes: 273.81MB Heap size: 870.32MB
[Info  - 6:49:41 AM] Live bytes: 273.81MB Heap size: 870.32MB
2023-02-12T19:50:41.950982Z | Info | Live bytes: 267.49MB Heap size: 1032.85MB
[Info  - 6:50:41 AM] Live bytes: 267.49MB Heap size: 1032.85MB
2023-02-12T19:51:41.956371Z | Info | Live bytes: 268.58MB Heap size: 1163.92MB
[Info  - 6:51:41 AM] Live bytes: 268.58MB Heap size: 1163.92MB
2023-02-12T19:52:41.961629Z | Info | Live bytes: 300.18MB Heap size: 1194.33MB
[Info  - 6:52:41 AM] Live bytes: 300.18MB Heap size: 1194.33MB
2023-02-12T19:53:41.964786Z | Info | Live bytes: 229.15MB Heap size: 1194.33MB
[Info  - 6:53:41 AM] Live bytes: 229.15MB Heap size: 1194.33MB
2023-02-12T19:54:12.963898Z | Warning | retrie: InternalError: pos
[Warn  - 6:54:13 AM] retrie: InternalError: pos
2023-02-12T19:54:41.978750Z | Info | Live bytes: 302.28MB Heap size: 1194.33MB
[Info  - 6:54:41 AM] Live bytes: 302.28MB Heap size: 1194.33MB
2023-02-12T19:55:41.993858Z | Info | Live bytes: 372.52MB Heap size: 1194.33MB
[Info  - 6:55:41 AM] Live bytes: 372.52MB Heap size: 1194.33MB
2023-02-12T19:56:42.009882Z | Info | Live bytes: 294.51MB Heap size: 1194.33MB
[Info  - 6:56:42 AM] Live bytes: 294.51MB Heap size: 1194.33MB
2023-02-12T19:57:42.018929Z | Info | Live bytes: 362.67MB Heap size: 1194.33MB
[Info  - 6:57:42 AM] Live bytes: 362.67MB Heap size: 1194.33MB
2023-02-12T19:58:42.032159Z | Info | Live bytes: 243.12MB Heap size: 1200.62MB
[Info  - 6:58:42 AM] Live bytes: 243.12MB Heap size: 1200.62MB
2023-02-12T19:58:42.380603Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:58:42 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-12T19:59:42.043598Z | Info | Live bytes: 267.48MB Heap size: 1200.62MB
[Info  - 6:59:42 AM] Live bytes: 267.48MB Heap size: 1200.62MB
2023-02-12T20:00:42.057325Z | Info | Live bytes: 267.48MB Heap size: 1200.62MB
[Info  - 7:00:42 AM] Live bytes: 267.48MB Heap size: 1200.62MB
2023-02-12T20:01:42.067312Z | Info | Live bytes: 267.48MB Heap size: 1200.62MB
[Info  - 7:01:42 AM] Live bytes: 267.48MB Heap size: 1200.62MB
2023-02-12T20:02:42.070633Z | Info | Live bytes: 267.48MB Heap size: 1200.62MB
[Info  - 7:02:42 AM] Live bytes: 267.48MB Heap size: 1200.62MB
2023-02-12T20:03:42.089339Z | Info | Live bytes: 323.26MB Heap size: 1200.62MB
[Info  - 7:03:42 AM] Live bytes: 323.26MB Heap size: 1200.62MB
2023-02-12T20:04:27.696181Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 7:04:27 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-12T20:04:42.094352Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:04:42 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-12T20:05:42.098962Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:05:42 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-12T20:06:42.108300Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:06:42 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-12T20:07:42.115742Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:07:42 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-12T20:08:42.125564Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:08:42 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-12T20:09:42.131543Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:09:42 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:47:09.664158Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:47:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:48:09.677004Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:48:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:49:09.682930Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:49:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:50:09.691628Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:50:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:51:09.698354Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:51:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:52:09.701097Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:52:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:53:09.710048Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:53:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:54:09.724855Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:54:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:55:09.726759Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:55:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:56:09.742302Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:56:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:57:09.747036Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:57:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:58:09.767830Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:58:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T06:59:09.775752Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:59:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:00:09.777231Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:00:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:01:09.779654Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:01:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:02:09.794159Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:02:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:03:09.799288Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:03:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:04:09.814977Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:04:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:05:09.825840Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:05:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:06:09.828628Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:06:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:07:09.842678Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:07:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:08:09.841074Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:08:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:09:09.851100Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:09:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:10:09.862603Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:10:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:11:09.863790Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:11:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:12:09.875014Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:12:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:13:09.876310Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:13:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:14:09.879302Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:14:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:15:09.881233Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:15:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:16:09.882569Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:16:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:17:09.894713Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:17:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:18:09.894016Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:18:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:19:09.892828Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:19:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:20:09.901289Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:20:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:21:09.907971Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:21:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:22:09.917182Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:22:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:23:09.923735Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:23:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:24:09.933031Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:24:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:25:09.944807Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:25:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:26:09.952579Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:26:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:27:09.954431Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:27:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:28:09.970658Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:28:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:29:09.976400Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:29:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:30:09.986411Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:30:09 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:31:10.002129Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:31:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:32:10.014824Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:32:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:33:10.021125Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:33:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:34:10.023740Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:34:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:35:10.042251Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:35:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:36:10.050598Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:36:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:37:10.067715Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:37:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:38:10.076370Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:38:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:39:10.077643Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:39:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:40:10.092263Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:40:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:41:10.105292Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:41:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:42:10.113710Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:42:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:43:10.118125Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:43:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:44:10.128738Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:44:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:45:10.132106Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:45:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:46:10.142460Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:46:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:47:10.148712Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:47:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:48:10.162592Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:48:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:49:10.176174Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:49:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:50:10.191036Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:50:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:51:10.200822Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:51:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:52:10.211292Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:52:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:53:10.217040Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:53:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:54:10.227739Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:54:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:55:10.229465Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:55:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:56:10.240953Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:56:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:57:10.254602Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:57:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:58:10.264138Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:58:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T07:59:10.274403Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:59:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:00:10.273881Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:00:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:01:10.287164Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:01:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:02:10.295919Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:02:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:03:10.304641Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:03:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:04:10.306184Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:04:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:05:10.319563Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:05:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:06:10.330297Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:06:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:07:10.331064Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:07:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:08:10.345117Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:08:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T08:09:10.347604Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:09:10 PM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T17:55:17.137243Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 4:55:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T17:56:17.153794Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 4:56:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T17:57:17.164799Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 4:57:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T17:58:17.177611Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 4:58:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T17:59:17.185865Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 4:59:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:00:17.208864Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:00:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:01:17.226106Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:01:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:02:17.230341Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:02:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:03:17.234911Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:03:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:04:17.238272Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:04:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:05:17.247654Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:05:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:06:17.258549Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:06:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:07:17.261977Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:07:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:08:17.269623Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:08:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:09:17.270785Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:09:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:10:17.280616Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:10:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:11:17.294367Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:11:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:12:17.296523Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:12:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:13:17.297566Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:13:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:14:17.312609Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:14:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:15:17.326441Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:15:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:16:17.351694Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:16:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:17:17.362152Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:17:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:18:17.378516Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:18:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:19:17.383104Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:19:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:20:17.388803Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:20:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:21:17.402970Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:21:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:22:17.418189Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:22:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:23:17.422476Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:23:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:24:17.424546Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:24:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:25:17.433924Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:25:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:26:17.441882Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:26:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:27:17.458982Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:27:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:28:17.468201Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:28:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:29:17.474607Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:29:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:30:17.484176Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:30:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:31:17.491840Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:31:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:32:17.510377Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:32:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:33:17.511382Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:33:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:34:17.514001Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:34:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:35:17.526016Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:35:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:36:17.539919Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:36:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:37:17.546106Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:37:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:38:17.555540Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:38:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:39:17.562898Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:39:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:40:17.576472Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:40:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:41:17.591522Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:41:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:42:17.596536Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:42:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:43:17.605975Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:43:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:44:17.611237Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:44:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:45:17.626344Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:45:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:46:17.640557Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:46:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:47:17.651878Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:47:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:48:17.658885Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:48:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:49:17.661454Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:49:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:50:17.677016Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:50:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:51:17.686458Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:51:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:52:17.690855Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:52:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:53:17.696127Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:53:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:54:17.715054Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:54:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:55:17.722320Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:55:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:56:17.727144Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:56:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:57:17.729275Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:57:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:58:17.741808Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:58:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T18:59:17.761523Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 5:59:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:00:17.767376Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:00:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:01:17.775440Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:01:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:02:17.777127Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:02:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:03:17.786280Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:03:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:04:17.802639Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:04:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:05:17.806030Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:05:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:06:17.825360Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:06:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:07:17.832917Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:07:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:08:17.844876Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:08:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:09:17.855714Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:09:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:10:17.866613Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:10:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:11:17.881479Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:11:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:12:17.892103Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:12:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:13:17.905318Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:13:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:14:17.911054Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:14:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:15:17.919214Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:15:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:16:17.934416Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:16:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:17:17.954374Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:17:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:18:17.970369Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:18:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:19:17.986979Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:19:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:20:17.991102Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:20:17 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:21:18.009462Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:21:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:22:18.016653Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:22:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:23:18.023966Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:23:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:24:18.043473Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:24:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:25:18.055254Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:25:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:26:18.069472Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:26:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:27:18.087111Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:27:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:28:18.091313Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:28:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:29:18.098274Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:29:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:30:18.111990Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:30:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:31:18.123678Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:31:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:32:18.134266Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:32:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:33:18.151988Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:33:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:34:18.171329Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:34:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:35:18.174908Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:35:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:36:18.178695Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:36:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:37:18.184875Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:37:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:38:18.188249Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:38:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:39:18.209314Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:39:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:40:18.224558Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:40:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:41:18.233129Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:41:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:42:18.244698Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:42:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:43:18.252915Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:43:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:44:18.266041Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:44:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:45:18.268588Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:45:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:46:18.287083Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:46:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:47:18.305273Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:47:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:48:18.326376Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:48:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:49:18.331591Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:49:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:50:18.332887Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:50:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:51:18.339489Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:51:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:52:18.353053Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:52:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:53:18.358743Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:53:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:54:18.368211Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:54:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:55:18.377126Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:55:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:56:18.383296Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:56:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:57:18.397886Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:57:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:58:18.402169Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:58:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T19:59:18.406863Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 6:59:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T20:00:18.413099Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:00:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T20:01:18.427437Z | Info | Live bytes: 342.75MB Heap size: 1200.62MB
[Info  - 7:01:18 AM] Live bytes: 342.75MB Heap size: 1200.62MB
2023-02-13T20:02:15.952191Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 7:02:15 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-13T20:02:18.446819Z | Info | Live bytes: 462.27MB Heap size: 1200.62MB
[Info  - 7:02:18 AM] Live bytes: 462.27MB Heap size: 1200.62MB
2023-02-13T20:03:08.076584Z | Warning | LSP: no handler for: SCustomMethod "$/setTrace"
[Warn  - 7:03:08 AM] LSP: no handler for: SCustomMethod "$/setTrace"
2023-02-13T20:03:18.462707Z | Info | Live bytes: 467.81MB Heap size: 1200.62MB
[Info  - 7:03:18 AM] Live bytes: 467.81MB Heap size: 1200.62MB
2023-02-13T20:04:18.469747Z | Info | Live bytes: 467.81MB Heap size: 1200.62MB
[Info  - 7:04:18 AM] Live bytes: 467.81MB Heap size: 1200.62MB
2023-02-14T19:00:39.275122Z | Info | Live bytes: 467.81MB Heap size: 1200.62MB
[Info  - 6:00:39 AM] Live bytes: 467.81MB Heap size: 1200.62MB
2023-02-14T19:01:39.279461Z | Info | Live bytes: 468.45MB Heap size: 1200.62MB
[Info  - 6:01:39 AM] Live bytes: 468.45MB Heap size: 1200.62MB
2023-02-14T19:02:39.285042Z | Info | Live bytes: 468.45MB Heap size: 1200.62MB
[Info  - 6:02:39 AM] Live bytes: 468.45MB Heap size: 1200.62MB
2023-02-14T19:03:39.301233Z | Info | Live bytes: 468.45MB Heap size: 1200.62MB
[Info  - 6:03:39 AM] Live bytes: 468.45MB Heap size: 1200.62MB
2023-02-14T19:04:39.305757Z | Info | Live bytes: 468.45MB Heap size: 1200.62MB
[Info  - 6:04:39 AM] Live bytes: 468.45MB Heap size: 1200.62MB
2023-02-14T19:05:39.013080Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:39.013080Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:05:39.014085Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:39.014085Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:39 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:05:39 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:39 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:39 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:05:39.312078Z | Info | Live bytes: 507.24MB Heap size: 1200.62MB
[Info  - 6:05:39 AM] Live bytes: 507.24MB Heap size: 1200.62MB
2023-02-14T19:05:39.511133Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:39 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:40.430374Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:40.430374Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:05:40.431369Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:40.431369Z | Warning | retrie: InternalError: pos
2023-02-14T19:05:40.431369Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:40 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:05:40 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:40 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:40 AM] retrie: InternalError: pos
[Warn  - 6:05:40 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:05:40.570345Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:40 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:40.587070Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:05:40 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:05:42.139512Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:05:42 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:05:42.263500Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:42 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:42.324803Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:42 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:43.213432Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:43 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:43.380434Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:43.380434Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:05:43.381431Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:43.381431Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:43 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:05:43 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:43 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:43 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:05:43.857224Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:43.857224Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:05:43.857224Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:43.857224Z | Warning | retrie: InternalError: pos
2023-02-14T19:05:43.858228Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:43 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:05:43 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:43 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:43 AM] retrie: InternalError: pos
[Warn  - 6:05:43 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:05:43.934243Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:43 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:44.452887Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:05:44 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:05:48.143341Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:48 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:48.540531Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:05:48 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:05:50.427689Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:05:50 AM] class: InternalError: Unable to typecheck
2023-02-14T19:05:51.769284Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:51.769284Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:05:51.769284Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:51.769284Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:05:51.800285Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:51.800285Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:51 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:51.801301Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:51.801301Z | Warning | retrie: InternalError: pos
2023-02-14T19:05:51.801301Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:51 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:51 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:51 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:51 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:05:51 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:51 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:51 AM] retrie: InternalError: pos
[Warn  - 6:05:51 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:05:51.870288Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:05:51.870288Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:05:51.870288Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:05:51.871288Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:05:51 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:05:51 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:05:51 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:05:51 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:06:39.328430Z | Info | Live bytes: 238.31MB Heap size: 1235.22MB
[Info  - 6:06:39 AM] Live bytes: 238.31MB Heap size: 1235.22MB
2023-02-14T19:07:39.335774Z | Info | Live bytes: 239.11MB Heap size: 1235.22MB
[Info  - 6:07:39 AM] Live bytes: 239.11MB Heap size: 1235.22MB
2023-02-14T19:08:39.337026Z | Info | Live bytes: 260.39MB Heap size: 1235.22MB
[Info  - 6:08:39 AM] Live bytes: 260.39MB Heap size: 1235.22MB
2023-02-14T19:09:10.174141Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:10 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:10.535183Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:10 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:10.770185Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:10 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:12.830085Z | Warning | class: InternalError: Unable to typecheck
2023-02-14T19:09:12.830085Z | Warning | class: InternalError: Unable to typecheck
2023-02-14T19:09:12.830085Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:12 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:12.877703Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:12.878703Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:12.879703Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:12.880701Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:12 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:12.884697Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:12.884697Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:12.884697Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:12.884697Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:12 AM] class: InternalError: Unable to typecheck
[Warn  - 6:09:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:12 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:12 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:12 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:12 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:12 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:12.981698Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:12 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:13.540695Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:13 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:13.645695Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:13 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:14.632697Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:14.632697Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:14.633702Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:14.633702Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:14.679696Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:14 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:14 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:14 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:14 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:14 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:15.015723Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:15.015723Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:15.016727Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:15.016727Z | Warning | retrie: InternalError: pos
2023-02-14T19:09:15.016727Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:15 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:15 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:15 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:15 AM] retrie: InternalError: pos
[Warn  - 6:09:15 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:15.077724Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:15 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:15.774807Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:15 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:16.245928Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:16.245928Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:16.245928Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:16.245928Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:16 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:16 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:16 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:16 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:16.293592Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:16 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:16.705903Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:16 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:17.001149Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:17 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:17.541671Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:17.541671Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:17.542671Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:17.542671Z | Warning | retrie: InternalError: pos
2023-02-14T19:09:17.542671Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:17 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:17 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:17 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:17 AM] retrie: InternalError: pos
[Warn  - 6:09:17 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:17.578663Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:17 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:17.823668Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:17 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:22.664367Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:22 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:37.418686Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:37.418686Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:37.418686Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:37 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:37.418686Z | Warning | retrie: InternalError: pos
2023-02-14T19:09:37.419695Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:37 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:37 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:37 AM] retrie: InternalError: pos
[Warn  - 6:09:37 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:39.069888Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:39.069888Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:39.069888Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:39.069888Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:39 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:39 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:39 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:39 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:39.124973Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:39 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:39.181784Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:39 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:39.344149Z | Info | Live bytes: 301.78MB Heap size: 1235.22MB
[Info  - 6:09:39 AM] Live bytes: 301.78MB Heap size: 1235.22MB
2023-02-14T19:09:39.632340Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:39 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:39.650348Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:39.650348Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:39.651340Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:39.651340Z | Warning | retrie: InternalError: pos
2023-02-14T19:09:39.651340Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:39 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:39 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:39 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:39 AM] retrie: InternalError: pos
[Warn  - 6:09:39 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:39.692111Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:39 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:40.211839Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:09:40 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:09:40.291352Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:09:40.291352Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:09:40.292352Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:09:40.292352Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:09:40 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:09:40 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:09:40 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:09:40 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:09:40.326530Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:40 AM] class: InternalError: Unable to typecheck
2023-02-14T19:09:45.399541Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:09:45 AM] class: InternalError: Unable to typecheck
2023-02-14T19:10:27.049692Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:10:27 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:10:27.157605Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:10:27 AM] class: InternalError: Unable to typecheck
2023-02-14T19:10:27.785341Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:10:27 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:10:27.888994Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:10:27.889994Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:10:27.891001Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:10:27.891001Z | Warning | retrie: InternalError: pos
2023-02-14T19:10:27.891001Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:10:27 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:10:27 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:10:27 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:10:27 AM] retrie: InternalError: pos
[Warn  - 6:10:27 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:10:27.924997Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:10:27 AM] class: InternalError: Unable to typecheck
2023-02-14T19:10:30.330708Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:10:30.330708Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:10:30.330708Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:10:30 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:10:30.330708Z | Warning | retrie: InternalError: pos
2023-02-14T19:10:30.330708Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:10:30 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:10:30 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:10:30 AM] retrie: InternalError: pos
[Warn  - 6:10:30 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:10:39.347023Z | Info | Live bytes: 360.06MB Heap size: 1235.22MB
[Info  - 6:10:39 AM] Live bytes: 360.06MB Heap size: 1235.22MB
2023-02-14T19:10:51.624343Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:10:51 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-14T19:11:39.362039Z | Info | Live bytes: 385.56MB Heap size: 1235.22MB
[Info  - 6:11:39 AM] Live bytes: 385.56MB Heap size: 1235.22MB
2023-02-14T19:12:39.372116Z | Info | Live bytes: 385.77MB Heap size: 1235.22MB
[Info  - 6:12:39 AM] Live bytes: 385.77MB Heap size: 1235.22MB
2023-02-14T19:13:39.374409Z | Info | Live bytes: 385.77MB Heap size: 1235.22MB
[Info  - 6:13:39 AM] Live bytes: 385.77MB Heap size: 1235.22MB
2023-02-14T19:14:39.374891Z | Info | Live bytes: 385.77MB Heap size: 1235.22MB
[Info  - 6:14:39 AM] Live bytes: 385.77MB Heap size: 1235.22MB
2023-02-14T19:15:39.379344Z | Info | Live bytes: 385.77MB Heap size: 1235.22MB
[Info  - 6:15:39 AM] Live bytes: 385.77MB Heap size: 1235.22MB
2023-02-14T19:16:39.388461Z | Info | Live bytes: 385.77MB Heap size: 1235.22MB
[Info  - 6:16:39 AM] Live bytes: 385.77MB Heap size: 1235.22MB
2023-02-14T19:17:39.391577Z | Info | Live bytes: 385.77MB Heap size: 1235.22MB
[Info  - 6:17:39 AM] Live bytes: 385.77MB Heap size: 1235.22MB
2023-02-14T19:18:39.410634Z | Info | Live bytes: 399.66MB Heap size: 1235.22MB
[Info  - 6:18:39 AM] Live bytes: 399.66MB Heap size: 1235.22MB
2023-02-14T19:19:07.634129Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:07 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:07.644130Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:07 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:10.083147Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:10.084147Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:10.084147Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:10.084147Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:10 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:10 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:10 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:10 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:10.389326Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:10 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:10.453336Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:10 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:11.118892Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:11.118892Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:11.118892Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:11.118892Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:11 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:11.909677Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:11.909677Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:11.910689Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:11.910689Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:11 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:12.547277Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:12.547277Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:12.548281Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:12.548281Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:12 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:12 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:12 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:12.708392Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:12 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:12.800966Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:12 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:14.094655Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:14.094655Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:14.094655Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:14.094655Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:14 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:14 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:14 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:14 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:14.942385Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:14.942385Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:14.942385Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:14.942385Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:15 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:15 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:15 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:15 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:15.261387Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:15 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:15.375943Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:15 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:19.212116Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:19.212116Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:19 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:19.212116Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:19.212116Z | Warning | retrie: InternalError: pos
2023-02-14T19:19:19.212116Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:19 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:19 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:19 AM] retrie: InternalError: pos
[Warn  - 6:19:19 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:20.490284Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:20 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:30.011901Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:30.011901Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:30.012902Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:30.012902Z | Warning | retrie: InternalError: pos
2023-02-14T19:19:30.012902Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:30 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:30 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:30 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:30 AM] retrie: InternalError: pos
[Warn  - 6:19:30 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:30.908678Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:30.909678Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:30.909678Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:30.909678Z | Warning | retrie: InternalError: pos
2023-02-14T19:19:30.909678Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:30 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:19:31 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:31 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:31 AM] retrie: InternalError: pos
[Warn  - 6:19:31 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:31.371673Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:31 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:31.562205Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:31.562205Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:19:31.562205Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:31.563207Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:31 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:19:31.563207Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:19:31 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:19:31 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:31 AM] retrie: InternalError: pos
[Warn  - 6:19:31 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:19:32.577110Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:32.577110Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:32 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:32 AM] retrie: InternalError: pos
2023-02-14T19:19:32.725153Z | Warning | class: InternalError: Unable to typecheck
2023-02-14T19:19:32.737645Z | Warning | codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:33.011900Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:33.012912Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:33 AM] class: InternalError: Unable to typecheck
[Warn  - 6:19:33 AM] codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:33 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:33 AM] retrie: InternalError: pos
2023-02-14T19:19:35.387071Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:35.387071Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:35 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:35 AM] retrie: InternalError: pos
2023-02-14T19:19:36.110744Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:36.110744Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:36 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:36 AM] retrie: InternalError: pos
2023-02-14T19:19:36.494949Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:36.494949Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:36 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:36 AM] retrie: InternalError: pos
2023-02-14T19:19:37.791641Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:37 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:39.477943Z | Info | Live bytes: 367.20MB Heap size: 1280.31MB
2023-02-14T19:19:39.528945Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:39.529950Z | Warning | retrie: InternalError: pos
[Info  - 6:19:39 AM] Live bytes: 367.20MB Heap size: 1280.31MB
[Warn  - 6:19:39 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:39 AM] retrie: InternalError: pos
2023-02-14T19:19:39.694664Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:39 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:39.738277Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:39.738277Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:39 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:39 AM] retrie: InternalError: pos
2023-02-14T19:19:39.941600Z | Warning | class: InternalError: Unable to typecheck
2023-02-14T19:19:40.158802Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:40.159652Z | Warning | retrie: InternalError: pos
2023-02-14T19:19:41.080722Z | Warning | codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:41.138918Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:41.138918Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:41 AM] class: InternalError: Unable to typecheck
[Warn  - 6:19:41 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:41 AM] retrie: InternalError: pos
[Warn  - 6:19:41 AM] codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:41 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:41 AM] retrie: InternalError: pos
2023-02-14T19:19:41.442577Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:41 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:41.710654Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:41.710654Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:41 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:41 AM] retrie: InternalError: pos
2023-02-14T19:19:42.207230Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:42.208235Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:42 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:42 AM] retrie: InternalError: pos
2023-02-14T19:19:43.488432Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:43.488432Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:43 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:43 AM] retrie: InternalError: pos
2023-02-14T19:19:46.705094Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:19:46 AM] class: InternalError: Unable to typecheck
2023-02-14T19:19:50.121298Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:50.121298Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:50 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:50 AM] retrie: InternalError: pos
2023-02-14T19:19:51.687850Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:51.688837Z | Warning | retrie: InternalError: pos
[Warn  - 6:19:51 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:51 AM] retrie: InternalError: pos
2023-02-14T19:19:59.735905Z | Warning | codeRange: bad dependency: GetCodeRange
2023-02-14T19:19:59.782899Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:59 AM] codeRange: bad dependency: GetCodeRange
[Warn  - 6:19:59 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:19:59.809897Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:19:59 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:00.169427Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:00 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:00.178995Z | Warning | class: InternalError: Unable to typecheck
2023-02-14T19:20:00.696189Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:00.704295Z | Warning | retrie: InternalError: pos
[Warn  - 6:20:01 AM] class: InternalError: Unable to typecheck
[Warn  - 6:20:01 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:01 AM] retrie: InternalError: pos
2023-02-14T19:20:03.672112Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:20:03 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:20:03.724109Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:03 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:04.048441Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:04.157031Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:05 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:05 AM] class: InternalError: Unable to typecheck
2023-02-14T19:20:05.151037Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:05 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:05.698028Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:05 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:06.959208Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:06.959208Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:06.959208Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:06.959208Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:07 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:07 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:07 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:07 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:07.084774Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:20:07 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:20:07.246781Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:07.247770Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:07.247770Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:07.247770Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:07 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:07 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:07 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:07 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:07.575273Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:07 AM] class: InternalError: Unable to typecheck
2023-02-14T19:20:12.670661Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:12 AM] class: InternalError: Unable to typecheck
2023-02-14T19:20:26.755936Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:26.755936Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:26.756936Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:26.756936Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:26 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:26 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:26 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:26 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:37.658977Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:37.658977Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:37.659973Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:37.659973Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:37 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:37 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:37 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:37 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:39.494248Z | Info | Live bytes: 510.06MB Heap size: 1280.31MB
[Info  - 6:20:39 AM] Live bytes: 510.06MB Heap size: 1280.31MB
2023-02-14T19:20:39.546223Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:39.547223Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:39.547223Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:39.547223Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:39 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:39 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:39 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:39 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:42.135005Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:42.135005Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:42.135005Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:42.135005Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:42 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:42 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:42 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:42 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:43.026434Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:20:43 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:20:43.301968Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:43 AM] class: InternalError: Unable to typecheck
2023-02-14T19:20:48.402095Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:48 AM] class: InternalError: Unable to typecheck
2023-02-14T19:20:55.189442Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:55.189442Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:55.189442Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:55.189442Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:55 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:55 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:55 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:55 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:55.402336Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:20:55 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:20:55.547328Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:55.547328Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:55.548328Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:55.548328Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:55 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:55 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:55 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:55 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:55.557333Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:55 AM] class: InternalError: Unable to typecheck
2023-02-14T19:20:59.610241Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:20:59 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:20:59.652433Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:20:59.652433Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:20:59.652433Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:20:59.652433Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:20:59.711432Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:20:59 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:20:59 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:20:59 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:20:59 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:20:59 AM] class: InternalError: Unable to typecheck
2023-02-14T19:21:00.319356Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:21:00 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:21:00.440338Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:21:00 AM] class: InternalError: Unable to typecheck
2023-02-14T19:21:00.543341Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:21:00.543341Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:21:00.544350Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:21:00.544350Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:00 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:00 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:00 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:00 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:21:00.882976Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:21:00.882976Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:21:00.882976Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:21:00.882976Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:00 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:00 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:00 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:00 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:21:27.815842Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:21:27.815842Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:21:27.816844Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:21:27.816844Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:27 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:27 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:27 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:27 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:21:28.219477Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:21:28.219477Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:21:28.220470Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:21:28.220470Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:28 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:28 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:28 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:28 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:21:28.303531Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:21:28 AM] class: InternalError: Unable to typecheck
2023-02-14T19:21:28.321667Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:21:28 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:21:29.695510Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:21:29.695510Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:21:29.695510Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:29 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:21:29.696505Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:29 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:29 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:29 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:21:39.495761Z | Info | Live bytes: 455.08MB Heap size: 1314.91MB
[Info  - 6:21:39 AM] Live bytes: 455.08MB Heap size: 1314.91MB
2023-02-14T19:22:24.873757Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:22:24 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-14T19:22:39.501398Z | Info | Live bytes: 258.24MB Heap size: 1314.91MB
[Info  - 6:22:39 AM] Live bytes: 258.24MB Heap size: 1314.91MB
2023-02-14T19:23:31.159170Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:23:31 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-14T19:23:39.517556Z | Info | Live bytes: 325.57MB Heap size: 1314.91MB
[Info  - 6:23:39 AM] Live bytes: 325.57MB Heap size: 1314.91MB
2023-02-14T19:24:22.315828Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-14T19:24:22.316822Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-14T19:24:22.316822Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:24:22.316822Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:24:22 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:24:22 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:24:22 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:24:22 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-14T19:24:39.527620Z | Info | Live bytes: 423.38MB Heap size: 1314.91MB
[Info  - 6:24:39 AM] Live bytes: 423.38MB Heap size: 1314.91MB
2023-02-14T19:25:39.547662Z | Info | Live bytes: 423.38MB Heap size: 1314.91MB
[Info  - 6:25:39 AM] Live bytes: 423.38MB Heap size: 1314.91MB
2023-02-14T19:26:39.558796Z | Info | Live bytes: 423.38MB Heap size: 1314.91MB
[Info  - 6:26:39 AM] Live bytes: 423.38MB Heap size: 1314.91MB
2023-02-14T19:27:39.564906Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:27:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:28:39.576995Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:28:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:29:39.580595Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:29:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:30:39.586395Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:30:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:31:39.589050Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:31:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:32:39.610101Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:32:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:33:39.619546Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:33:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:34:39.629794Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:34:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:35:39.631531Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:35:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:36:39.639895Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:36:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:37:39.652110Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:37:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:38:39.673182Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:38:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:39:39.681698Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:39:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:40:39.698187Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:40:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:41:39.700376Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:41:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:42:39.712914Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:42:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:43:39.716884Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:43:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:44:39.727991Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:44:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:45:39.735620Z | Info | Live bytes: 272.90MB Heap size: 1314.91MB
[Info  - 6:45:39 AM] Live bytes: 272.90MB Heap size: 1314.91MB
2023-02-14T19:46:39.738846Z | Info | Live bytes: 323.54MB Heap size: 1314.91MB
[Info  - 6:46:39 AM] Live bytes: 323.54MB Heap size: 1314.91MB
2023-02-14T19:47:39.741360Z | Info | Live bytes: 323.54MB Heap size: 1314.91MB
[Info  - 6:47:39 AM] Live bytes: 323.54MB Heap size: 1314.91MB
2023-02-14T19:48:39.751863Z | Info | Live bytes: 323.54MB Heap size: 1314.91MB
[Info  - 6:48:39 AM] Live bytes: 323.54MB Heap size: 1314.91MB
2023-02-14T19:49:39.767099Z | Info | Live bytes: 323.54MB Heap size: 1314.91MB
[Info  - 6:49:39 AM] Live bytes: 323.54MB Heap size: 1314.91MB
2023-02-14T19:50:27.507541Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:50:27 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-14T19:50:39.773701Z | Info | Live bytes: 242.01MB Heap size: 1314.91MB
[Info  - 6:50:39 AM] Live bytes: 242.01MB Heap size: 1314.91MB
2023-02-14T19:51:39.784533Z | Info | Live bytes: 242.01MB Heap size: 1314.91MB
[Info  - 6:51:39 AM] Live bytes: 242.01MB Heap size: 1314.91MB
2023-02-14T19:52:39.788725Z | Info | Live bytes: 262.90MB Heap size: 1314.91MB
[Info  - 6:52:39 AM] Live bytes: 262.90MB Heap size: 1314.91MB
2023-02-14T19:53:39.800602Z | Info | Live bytes: 262.90MB Heap size: 1314.91MB
[Info  - 6:53:39 AM] Live bytes: 262.90MB Heap size: 1314.91MB
2023-02-14T19:54:39.806729Z | Info | Live bytes: 262.90MB Heap size: 1314.91MB
[Info  - 6:54:39 AM] Live bytes: 262.90MB Heap size: 1314.91MB
2023-02-14T19:55:02.204455Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:55:02 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-14T19:55:13.900900Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:55:13 AM] codeRange: bad dependency: GetCodeRange
2023-02-14T19:55:13.937889Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:55:13.944884Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:55:13 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:55:13 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:55:13.961882Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:55:13 AM] class: InternalError: Unable to typecheck
2023-02-14T19:55:14.209764Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:55:14 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-14T19:55:40.056189Z | Info | Live bytes: 254.35MB Heap size: 1314.91MB
[Info  - 6:55:40 AM] Live bytes: 254.35MB Heap size: 1314.91MB
2023-02-14T19:56:40.061876Z | Info | Live bytes: 275.56MB Heap size: 1314.91MB
[Info  - 6:56:40 AM] Live bytes: 275.56MB Heap size: 1314.91MB
2023-02-14T19:57:40.073053Z | Info | Live bytes: 355.97MB Heap size: 1314.91MB
[Info  - 6:57:40 AM] Live bytes: 355.97MB Heap size: 1314.91MB
2023-02-14T19:58:23.596664Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:58:23 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-14T19:58:40.076373Z | Info | Live bytes: 382.14MB Heap size: 1314.91MB
[Info  - 6:58:40 AM] Live bytes: 382.14MB Heap size: 1314.91MB
2023-02-15T07:18:41.152080Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:18:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:19:41.156500Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:19:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:20:41.176643Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:20:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:21:41.187834Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:21:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:22:41.190906Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:22:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:23:41.209530Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:23:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:24:41.221962Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:24:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:25:41.240023Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:25:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:26:41.244242Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:26:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:27:41.256601Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:27:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:28:41.259292Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:28:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:29:41.267643Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:29:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:30:41.282173Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:30:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:31:41.283692Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:31:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:32:41.295375Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:32:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:33:41.308839Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:33:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:34:41.331375Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:34:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:35:41.349638Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:35:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:36:41.547902Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:36:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:37:41.570245Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:37:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:38:41.583291Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:38:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:39:41.595733Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:39:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:40:41.603146Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:40:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:41:41.619298Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:41:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:42:41.628875Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:42:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:43:41.650115Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:43:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:44:41.659868Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:44:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:45:41.685134Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:45:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:46:41.702667Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:46:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:47:41.721334Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:47:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:48:41.741625Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:48:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T07:49:41.750512Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 6:49:41 PM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:39:31.407185Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 5:39:31 AM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:40:31.410556Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 5:40:31 AM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:41:31.424786Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 5:41:31 AM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:42:31.438102Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 5:42:31 AM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:43:32.520738Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 5:43:32 AM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:44:32.533164Z | Info | Live bytes: 382.49MB Heap size: 1314.91MB
[Info  - 5:44:32 AM] Live bytes: 382.49MB Heap size: 1314.91MB
2023-02-15T18:45:32.540379Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:45:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:46:32.543487Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:46:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:47:32.547307Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:47:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:48:32.568531Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:48:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:49:32.572346Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:49:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:50:32.589079Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:50:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:51:32.602163Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:51:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:52:32.621188Z | Info | Live bytes: 390.22MB Heap size: 1314.91MB
[Info  - 5:52:32 AM] Live bytes: 390.22MB Heap size: 1314.91MB
2023-02-15T18:53:32.632667Z | Info | Live bytes: 414.13MB Heap size: 1314.91MB
[Info  - 5:53:32 AM] Live bytes: 414.13MB Heap size: 1314.91MB
2023-02-15T18:54:32.638312Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 5:54:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T18:55:32.650692Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 5:55:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T18:56:32.658862Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 5:56:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T18:57:32.667431Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 5:57:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T18:58:32.670506Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 5:58:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T18:59:32.754813Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 5:59:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:00:32.777140Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:00:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:01:32.797901Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:01:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:02:32.809775Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:02:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:03:32.825633Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:03:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:04:32.841568Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:04:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:05:32.861155Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:05:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:06:32.874987Z | Info | Live bytes: 454.02MB Heap size: 1314.91MB
[Info  - 6:06:32 AM] Live bytes: 454.02MB Heap size: 1314.91MB
2023-02-15T19:07:32.877517Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:07:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:07:38.872145Z | Warning | rename: InternalError: Explicit export list required for renaming
[Warn  - 6:07:38 AM] rename: InternalError: Explicit export list required for renaming
[Error - 6:07:38 AM] Request textDocument/rename failed.
  Message: Explicit export list required for renaming
  Code: -32603 
2023-02-15T19:08:32.883345Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:08:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:09:32.890388Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:09:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:10:32.891592Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:10:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:11:32.910398Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:11:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:12:32.920479Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:12:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:13:32.930922Z | Info | Live bytes: 490.13MB Heap size: 1314.91MB
[Info  - 6:13:32 AM] Live bytes: 490.13MB Heap size: 1314.91MB
2023-02-15T19:14:04.752223Z | Warning | LSP: no handler for: SCustomMethod "$/setTrace"
[Warn  - 6:14:04 AM] LSP: no handler for: SCustomMethod "$/setTrace"
2023-02-15T19:14:32.933016Z | Info | Live bytes: 287.71MB Heap size: 1320.16MB
[Info  - 6:14:32 AM] Live bytes: 287.71MB Heap size: 1320.16MB
2023-02-15T19:15:32.941848Z | Info | Live bytes: 303.89MB Heap size: 1320.16MB
[Info  - 6:15:32 AM] Live bytes: 303.89MB Heap size: 1320.16MB
2023-02-15T19:16:32.962200Z | Info | Live bytes: 303.89MB Heap size: 1320.16MB
[Info  - 6:16:32 AM] Live bytes: 303.89MB Heap size: 1320.16MB
2023-02-15T19:17:32.964671Z | Info | Live bytes: 303.89MB Heap size: 1320.16MB
[Info  - 6:17:32 AM] Live bytes: 303.89MB Heap size: 1320.16MB
2023-02-15T19:18:21.988699Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-15T19:18:21.988699Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\src\\Internal\\SuiteRuntime.hs": [ NormalizedFilePath "C:\\Pyrethrum\\src\\Internal\\ExeNode.hs"
                                                                                                          , NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs" ]
[Info  - 6:18:22 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:18:22 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\src\\Internal\\SuiteRuntime.hs": [ NormalizedFilePath "C:\\Pyrethrum\\src\\Internal\\ExeNode.hs"
                                                                                                          , NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs" ]
2023-02-15T19:18:32.975609Z | Info | Live bytes: 434.97MB Heap size: 1320.16MB
[Info  - 6:18:32 AM] Live bytes: 434.97MB Heap size: 1320.16MB
2023-02-15T19:19:32.976948Z | Info | Live bytes: 355.16MB Heap size: 1371.54MB
[Info  - 6:19:32 AM] Live bytes: 355.16MB Heap size: 1371.54MB
2023-02-15T19:20:32.978515Z | Info | Live bytes: 429.33MB Heap size: 1371.54MB
[Info  - 6:20:32 AM] Live bytes: 429.33MB Heap size: 1371.54MB
2023-02-15T19:21:32.980966Z | Info | Live bytes: 507.96MB Heap size: 1371.54MB
[Info  - 6:21:32 AM] Live bytes: 507.96MB Heap size: 1371.54MB
2023-02-15T19:21:55.964363Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:21:55.964363Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:21:55.964363Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:21:55.964363Z | Warning | retrie: InternalError: pos
2023-02-15T19:21:55.964363Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:55 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:55 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:55 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:55 AM] retrie: InternalError: pos
[Warn  - 6:21:55 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:21:56.169699Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:21:56 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:21:56.308926Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:21:56 AM] class: InternalError: Unable to typecheck
2023-02-15T19:21:57.178101Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:21:57.179097Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:21:57.179097Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:21:57.179097Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:57 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:57 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:57 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:57 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:21:58.798441Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:21:58 AM] class: InternalError: Unable to typecheck
2023-02-15T19:21:59.067189Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:21:59 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:21:59.467398Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:21:59.467398Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:21:59.467398Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:21:59.467398Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:21:59 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:21:59 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:21:59 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:21:59 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:21:59.965151Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:21:59 AM] class: InternalError: Unable to typecheck
2023-02-15T19:22:00.215169Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:22:00 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:22:32.997147Z | Info | Live bytes: 271.93MB Heap size: 1371.54MB
[Info  - 6:22:32 AM] Live bytes: 271.93MB Heap size: 1371.54MB
2023-02-15T19:22:44.461996Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:22:44.461996Z | Warning | retrie: InternalError: pos
[Warn  - 6:22:44 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:22:44 AM] retrie: InternalError: pos
2023-02-15T19:22:44.596285Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:22:44.596285Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:22:44.596285Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:22:44.596285Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:22:44 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:22:44 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:22:44 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:22:44 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:22:44.890588Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:22:44 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:22:44.892615Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:22:44 AM] class: InternalError: Unable to typecheck
2023-02-15T19:22:45.874967Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:22:45.876015Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:22:45.876015Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:22:45.876015Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:22:45 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:22:45 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:22:45 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:22:45 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:22:48.115258Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:22:48.115258Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:22:48.115258Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:22:48.115258Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:22:48 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:22:48 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:22:48 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:22:48 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:22:48.115258Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:22:48 AM] class: InternalError: Unable to typecheck
2023-02-15T19:22:48.198332Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:22:48 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:22:50.588596Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:22:50.588596Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:22:50.588596Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:22:50.588596Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:22:50 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:22:50 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:22:50 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:22:50 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:23:33.005699Z | Info | Live bytes: 461.56MB Heap size: 1371.54MB
[Info  - 6:23:33 AM] Live bytes: 461.56MB Heap size: 1371.54MB
2023-02-15T19:24:33.013421Z | Info | Live bytes: 422.77MB Heap size: 1395.65MB
[Info  - 6:24:33 AM] Live bytes: 422.77MB Heap size: 1395.65MB
2023-02-15T19:24:38.304277Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:24:38 AM] class: InternalError: Unable to typecheck
2023-02-15T19:24:38.635876Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:24:38 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:24:43.148291Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:24:43 AM] class: InternalError: Unable to typecheck
2023-02-15T19:24:43.911542Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:24:43 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:24:43.940899Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:24:43 AM] class: InternalError: Unable to typecheck
2023-02-15T19:24:44.182004Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:24:44 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:24:45.966508Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:24:45.968515Z | Warning | class: InternalError: Unable to typecheck
2023-02-15T19:24:45.978508Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:24:46 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:24:46.269923Z | Warning | codeRange: bad dependency: GetCodeRange
2023-02-15T19:24:46.383994Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:24:47 AM] class: InternalError: Unable to typecheck
[Warn  - 6:24:47 AM] class: InternalError: Unable to typecheck
[Warn  - 6:24:47 AM] codeRange: bad dependency: GetCodeRange
[Warn  - 6:24:47 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:24:52.723999Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:24:52.723999Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:24:52.723999Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:24:52.723999Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:24:52 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:24:52 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:24:52 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:24:52 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:24:53.090870Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:24:53 AM] class: InternalError: Unable to typecheck
2023-02-15T19:24:53.232567Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:24:53 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:25:33.033432Z | Info | Live bytes: 288.98MB Heap size: 1395.65MB
[Info  - 6:25:33 AM] Live bytes: 288.98MB Heap size: 1395.65MB
2023-02-15T19:26:33.043526Z | Info | Live bytes: 288.98MB Heap size: 1395.65MB
[Info  - 6:26:33 AM] Live bytes: 288.98MB Heap size: 1395.65MB
2023-02-15T19:27:33.054797Z | Info | Live bytes: 288.98MB Heap size: 1395.65MB
[Info  - 6:27:33 AM] Live bytes: 288.98MB Heap size: 1395.65MB
2023-02-15T19:28:33.068291Z | Info | Live bytes: 288.98MB Heap size: 1395.65MB
[Info  - 6:28:33 AM] Live bytes: 288.98MB Heap size: 1395.65MB
2023-02-15T19:28:52.007404Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:28:52 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-15T19:29:33.082459Z | Info | Live bytes: 309.23MB Heap size: 1395.65MB
[Info  - 6:29:33 AM] Live bytes: 309.23MB Heap size: 1395.65MB
2023-02-15T19:30:11.556304Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:30:11 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-15T19:30:33.084782Z | Info | Live bytes: 400.05MB Heap size: 1395.65MB
[Info  - 6:30:33 AM] Live bytes: 400.05MB Heap size: 1395.65MB
2023-02-15T19:31:33.086226Z | Info | Live bytes: 400.05MB Heap size: 1395.65MB
[Info  - 6:31:33 AM] Live bytes: 400.05MB Heap size: 1395.65MB
2023-02-15T19:32:33.087855Z | Info | Live bytes: 400.05MB Heap size: 1395.65MB
[Info  - 6:32:33 AM] Live bytes: 400.05MB Heap size: 1395.65MB
2023-02-15T19:33:33.095900Z | Info | Live bytes: 400.05MB Heap size: 1395.65MB
[Info  - 6:33:33 AM] Live bytes: 400.05MB Heap size: 1395.65MB
2023-02-15T19:34:33.102820Z | Info | Live bytes: 400.05MB Heap size: 1395.65MB
[Info  - 6:34:33 AM] Live bytes: 400.05MB Heap size: 1395.65MB
2023-02-15T19:35:33.105623Z | Info | Live bytes: 400.05MB Heap size: 1395.65MB
[Info  - 6:35:33 AM] Live bytes: 400.05MB Heap size: 1395.65MB
2023-02-15T19:35:43.587823Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:43 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:43.772419Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:43 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:45.684758Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:45.684758Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:45.684758Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:45.686887Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:45.686887Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:45 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:45 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:45 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:45 AM] retrie: InternalError: pos
[Warn  - 6:35:45 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:46.109870Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:46.109870Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:46.109870Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:46.109870Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:46.109870Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:46 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:46 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:46 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:46 AM] retrie: InternalError: pos
[Warn  - 6:35:46 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:48.148824Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:48.148824Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:48.148824Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:48.148824Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:48.148824Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:48 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:48 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:48 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:48 AM] retrie: InternalError: pos
[Warn  - 6:35:48 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:48.312718Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:48 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:48.434249Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:48 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:49.775986Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:49.775986Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:49.775986Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:49.775986Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:49 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:49 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:49 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:49 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:51.175360Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:51.176361Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:51.176361Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:51.176361Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:51 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:51 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:51 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:51 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:51.752899Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:51.752899Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:51.752899Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:51.753901Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:51.753901Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:51 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:51 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:51 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:51 AM] retrie: InternalError: pos
[Warn  - 6:35:51 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:51.867743Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:51 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:51.945750Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:51 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:53.004076Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:53.004076Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:53.004076Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:53.005073Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:53.005073Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:53 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:53 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:53 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:53 AM] retrie: InternalError: pos
[Warn  - 6:35:53 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:55.709005Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:55.709005Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:55.710002Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:55.710002Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:55.710002Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:55.778006Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:55 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:55 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:55 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:55 AM] retrie: InternalError: pos
[Warn  - 6:35:55 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:55 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:55.967008Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:55 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:56.043507Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:56.044507Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:56.044507Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:56.044507Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:56.044507Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:56 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:56 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:56 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:56 AM] retrie: InternalError: pos
[Warn  - 6:35:56 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:56.732827Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:56 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:56.763159Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:56.763159Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:56.763159Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:56.764156Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:56.764156Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:56.784154Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:56 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:57 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:57 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:57 AM] retrie: InternalError: pos
[Warn  - 6:35:57 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:57 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:57.266453Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:57 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:57.386466Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:57.386466Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:57 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:57.387463Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:57.388459Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:57.389461Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:57 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:57 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:57 AM] retrie: InternalError: pos
[Warn  - 6:35:57 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:57.404656Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:57 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:57.486697Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:57.486697Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:57.486697Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:57.486697Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:57.486697Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:57 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:57 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:57 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:57 AM] retrie: InternalError: pos
[Warn  - 6:35:57 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:58.031011Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:58 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:58.114100Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:58.114100Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:58.114100Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:58.114100Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:58.114100Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:58 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:58 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:58 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:58 AM] retrie: InternalError: pos
[Warn  - 6:35:58 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:35:58.267851Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:58 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:58.424300Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:35:58 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:35:58.573150Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:35:58 AM] class: InternalError: Unable to typecheck
2023-02-15T19:35:58.708003Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:35:58.708003Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:35:58.708003Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:35:58.708003Z | Warning | retrie: InternalError: pos
2023-02-15T19:35:58.708003Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:35:58 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:35:58 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:35:58 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:35:58 AM] retrie: InternalError: pos
[Warn  - 6:35:58 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:36:01.618397Z | Warning | codeRange: bad dependency: GetCodeRange
2023-02-15T19:36:01.620399Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:36:01 AM] codeRange: bad dependency: GetCodeRange
[Warn  - 6:36:01 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:36:01.700388Z | Warning | class: InternalError: Unable to typecheck
2023-02-15T19:36:01.707390Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:01.708390Z | Warning | retrie: InternalError: pos
[Warn  - 6:36:01 AM] class: InternalError: Unable to typecheck
2023-02-15T19:36:01.711392Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:36:01 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:01 AM] retrie: InternalError: pos
[Warn  - 6:36:01 AM] class: InternalError: Unable to typecheck
2023-02-15T19:36:02.055158Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:02.055158Z | Warning | retrie: InternalError: pos
[Warn  - 6:36:02 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:02 AM] retrie: InternalError: pos
2023-02-15T19:36:02.674747Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:02.674747Z | Warning | retrie: InternalError: pos
[Warn  - 6:36:02 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:02 AM] retrie: InternalError: pos
2023-02-15T19:36:04.769082Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:36:04 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:36:04.803084Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:04 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:04.843082Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:36:04 AM] class: InternalError: Unable to typecheck
2023-02-15T19:36:05.101406Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:05 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:06.279682Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:06 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:06.803095Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:06 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:08.432120Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:36:08 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:36:08.462127Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:36:08.463130Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:36:08.463130Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:08 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:36:08.463130Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:36:08 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:36:08 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:08 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:36:08.506661Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:36:08 AM] class: InternalError: Unable to typecheck
2023-02-15T19:36:08.647665Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:36:08.647665Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:36:08.648566Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:36:08.648853Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:36:08 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:36:08 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:36:08 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:36:08 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:36:08.903040Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:36:08 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:36:08.985346Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:36:09 AM] class: InternalError: Unable to typecheck
2023-02-15T19:36:33.126888Z | Info | Live bytes: 266.44MB Heap size: 1405.09MB
[Info  - 6:36:33 AM] Live bytes: 266.44MB Heap size: 1405.09MB
2023-02-15T19:37:33.134025Z | Info | Live bytes: 266.44MB Heap size: 1405.09MB
[Info  - 6:37:33 AM] Live bytes: 266.44MB Heap size: 1405.09MB
2023-02-15T19:38:33.141466Z | Info | Live bytes: 317.68MB Heap size: 1405.09MB
[Info  - 6:38:33 AM] Live bytes: 317.68MB Heap size: 1405.09MB
2023-02-15T19:38:41.030754Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:38:41 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:38:41.062380Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:41.062380Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:38:41 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:41.062380Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:38:41.062380Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:38:41 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:38:41 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:38:41 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:38:41.335638Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:41.335638Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:38:41.335638Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:38:41 AM] changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:41.335638Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:38:41 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:38:41 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:38:41 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:38:41.361290Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:38:41 AM] class: InternalError: Unable to typecheck
2023-02-15T19:38:41.468539Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:38:41 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:38:42.792303Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:42.792303Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:38:42.792303Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:38:42.792303Z | Warning | retrie: InternalError: pos
2023-02-15T19:38:42.792303Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:38:42 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:38:42 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:38:42 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:38:42 AM] retrie: InternalError: pos
[Warn  - 6:38:42 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:38:43.003172Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:38:43 AM] class: InternalError: Unable to typecheck
2023-02-15T19:38:43.370607Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:43.370607Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:38:43.370607Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:38:43.371604Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:38:43 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:38:43 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:38:43 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:38:43 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:38:43.918591Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:38:43 AM] class: InternalError: Unable to typecheck
2023-02-15T19:38:44.146585Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:38:44 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:38:46.295633Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:38:46.298251Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:38:46.298251Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:38:46.298251Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:38:46 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:38:46 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:38:46 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:38:46 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:38:48.822350Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:38:48 AM] class: InternalError: Unable to typecheck
2023-02-15T19:39:09.665362Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:39:09 AM] class: InternalError: Unable to typecheck
2023-02-15T19:39:09.792665Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:39:09 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:39:09.798963Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:39:09.798963Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:39:09.798963Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:39:09.798963Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:39:09 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:39:09 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:39:09 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:39:09 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:39:10.445931Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:39:10 AM] class: InternalError: Unable to typecheck
2023-02-15T19:39:10.533644Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:39:10 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:39:11.472097Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:39:11 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:39:11.476088Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:39:11.476088Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:39:11.477091Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:39:11.477091Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:39:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:39:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:39:11 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:39:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:39:11.516813Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:39:11 AM] class: InternalError: Unable to typecheck
2023-02-15T19:39:11.977625Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:39:11.989623Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:39:11.990625Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:39:11.990625Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:39:11 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:39:11 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:39:11 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:39:11 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:39:12.817618Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:39:12.827660Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:39:12.827847Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:39:12.827847Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:39:12 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:39:12 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:39:12 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:39:12 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:39:33.150656Z | Info | Live bytes: 275.86MB Heap size: 1413.48MB
[Info  - 6:39:33 AM] Live bytes: 275.86MB Heap size: 1413.48MB
2023-02-15T19:40:33.165526Z | Info | Live bytes: 275.86MB Heap size: 1413.48MB
[Info  - 6:40:33 AM] Live bytes: 275.86MB Heap size: 1413.48MB
2023-02-15T19:41:33.182524Z | Info | Live bytes: 281.53MB Heap size: 1413.48MB
[Info  - 6:41:33 AM] Live bytes: 281.53MB Heap size: 1413.48MB
2023-02-15T19:42:33.184445Z | Info | Live bytes: 281.53MB Heap size: 1413.48MB
[Info  - 6:42:33 AM] Live bytes: 281.53MB Heap size: 1413.48MB
2023-02-15T19:43:33.189447Z | Info | Live bytes: 281.53MB Heap size: 1413.48MB
[Info  - 6:43:33 AM] Live bytes: 281.53MB Heap size: 1413.48MB
2023-02-15T19:44:33.190779Z | Info | Live bytes: 309.10MB Heap size: 1413.48MB
[Info  - 6:44:33 AM] Live bytes: 309.10MB Heap size: 1413.48MB
2023-02-15T19:45:33.206633Z | Info | Live bytes: 431.84MB Heap size: 1413.48MB
[Info  - 6:45:33 AM] Live bytes: 431.84MB Heap size: 1413.48MB
2023-02-15T19:46:32.559918Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:46:32.559918Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:46:32.559918Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:32.559918Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:32 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:32 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:32 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:32 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:46:32.856931Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:46:32.856931Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:46:32.856931Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:32.856931Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:32 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:32 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:32 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:32 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:46:32.948736Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:32 AM] class: InternalError: Unable to typecheck
2023-02-15T19:46:33.053844Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:33 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:33.209979Z | Info | Live bytes: 366.66MB Heap size: 1413.48MB
[Info  - 6:46:33 AM] Live bytes: 366.66MB Heap size: 1413.48MB
2023-02-15T19:46:33.999420Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:46:33.999420Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:46:33.999420Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:33.999420Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:34 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:34 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:34 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:34 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:46:34.225647Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:34 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:34.274909Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:34 AM] class: InternalError: Unable to typecheck
2023-02-15T19:46:34.639481Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:46:34.639481Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:46:34.639481Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:34.639481Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:34 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:34 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:34 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:34 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:46:34.933063Z | Warning | class: InternalError: Unable to typecheck
2023-02-15T19:46:35.006479Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:35 AM] class: InternalError: Unable to typecheck
[Warn  - 6:46:35 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:41.565580Z | Warning | codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:41.565580Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:41 AM] codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:41 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:42.014210Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:42 AM] class: InternalError: Unable to typecheck
2023-02-15T19:46:42.034772Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:42 AM] class: InternalError: Unable to typecheck
2023-02-15T19:46:42.069098Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:42.070103Z | Warning | retrie: InternalError: pos
2023-02-15T19:46:42.074103Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:42 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:42 AM] retrie: InternalError: pos
[Warn  - 6:46:42 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:42.139099Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:42.140099Z | Warning | retrie: InternalError: pos
[Warn  - 6:46:42 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:42 AM] retrie: InternalError: pos
2023-02-15T19:46:42.166035Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:42.166035Z | Warning | retrie: InternalError: pos
[Warn  - 6:46:42 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:42 AM] retrie: InternalError: pos
2023-02-15T19:46:42.186450Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:42 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:42.196595Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:42 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:44.109285Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:44.109285Z | Warning | retrie: InternalError: pos
[Warn  - 6:46:44 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:44 AM] retrie: InternalError: pos
2023-02-15T19:46:44.819581Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:44 AM] class: InternalError: Unable to typecheck
2023-02-15T19:46:46.396893Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:46 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:46.748868Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:46 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:50.409137Z | Warning | changeTypeSignature: InternalError: Could not get Parsed Module
2023-02-15T19:46:50.410131Z | Warning | alternateNumberFormat: InternalError: Could not Collect Literals
2023-02-15T19:46:50.410131Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:50.410131Z | Warning | gadt: InternalError: Unable to get ParsedModuleWithComments
[Warn  - 6:46:50 AM] changeTypeSignature: InternalError: Could not get Parsed Module
[Warn  - 6:46:50 AM] alternateNumberFormat: InternalError: Could not Collect Literals
[Warn  - 6:46:50 AM] explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:50 AM] gadt: InternalError: Unable to get ParsedModuleWithComments
2023-02-15T19:46:50.416132Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:46:50 AM] explicit-fields: InternalError: Unable to TypeCheck
2023-02-15T19:46:50.598773Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:50 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:50.843441Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:50 AM] class: InternalError: Unable to typecheck
2023-02-15T19:46:55.144792Z | Warning | codeRange: bad dependency: GetCodeRange
[Warn  - 6:46:55 AM] codeRange: bad dependency: GetCodeRange
2023-02-15T19:46:55.473582Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:46:55 AM] class: InternalError: Unable to typecheck
2023-02-15T19:47:00.545455Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:47:00 AM] class: InternalError: Unable to typecheck
2023-02-15T19:47:05.500148Z | Info | Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
[Info  - 6:47:05 AM] Typechecking reverse dependencies for NormalizedFilePath "C:\\Pyrethrum\\test\\SuiteRuntimeTest.hs": [  ]
2023-02-15T19:47:06.496755Z | Warning | class: InternalError: Unable to typecheck
[Warn  - 6:47:06 AM] class: InternalError: Unable to typecheck
2023-02-15T19:47:33.212226Z | Info | Live bytes: 301.45MB Heap size: 1413.48MB
[Info  - 6:47:33 AM] Live bytes: 301.45MB Heap size: 1413.48MB
2023-02-15T19:47:43.711117Z | Warning | explicit-fields: InternalError: Unable to TypeCheck
[Warn  - 6:47:43 AM] explicit-fields: InternalError: Unable to TypeCheck

```