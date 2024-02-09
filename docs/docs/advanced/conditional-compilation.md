# Conditional Compilation




## Can I define my own symbols?

Sure you can.

Here is an example contributed by [𝓚𝓸𝓭𝓮𝓩𝔀𝓮𝓻𝓰 🇩🇪](https://discord.com/channels/570025060312547359/570025060312547361/1193531999542063134) (Unofficial Free Pascal Discord).

```pascal linenums="1"
{$IF Defined(DCC) or Defined(VER210) or Defined(VER200) or Defined(VER190) or Defined(VER185) or Defined(VER180) or Defined(VER170) or Defined(VER160) or Defined(VER150) or Defined(VER140) or Defined(VER130) or Defined(VER120) or Defined(VER100) or Defined(VER90) or Defined(VER80)}
  {$DEFINE Delphi} { Delphi }
{$IFEND}

{$IF Defined(DELPHI) and Declared(CompilerVersion) and (CompilerVersion >= 25)}
  {$LEGACYIFEND ON}
{$IFEND}

{$IF Defined(FPC)}
  {$DEFINE Lazarus} { Lazarus and Free Pascal }
{$IFEND}

{$IF Defined(DELPHI) and Declared(CompilerVersion) and (CompilerVersion >= 23)}
  {$DEFINE NameSpace} { Delphis NameSpace feature (eg Winapi.Windows instead of Windows) }
{$IFEND}

{$IF Defined(DELPHI) and Declared(CompilerVersion) and (CompilerVersion >= 20)}
  {$DEFINE UniCode} { Delphis UniCode support }
{$IFEND}

{$IF Defined(WIN32) or Defined(WIN64) or Defined(MSWindows)}
  {$DEFINE Windows} { We are on Windows }
{$IFEND Windows}

{$IF Defined(FPC) and Declared(FPC_VERSION) and (FPC_VERSION >= 3)}
  {$DEFINE UniCode} { FreePascal UniCode support }
{$IFEND}
```

And here's an example of how you might use these symbols.

```pascal linenums="1"
begin
{$IFDEF Delphi}
    // Specific code for Delphi mode
{$ENDIF}

{$IFDEF Lazarus}
    // Specific code for Lazarus and Free Pascal
{$ENDIF}

{$IFDEF Namespace}
    // Specific code for Delphi's namespace
{$ENDIF}

{$IFDEF Windows}
    // Specific code for Windows
{$ENDIF}

{$IFDEF UniCode}
    // Code relies on Unicode
{$ENDIF}
end.


```