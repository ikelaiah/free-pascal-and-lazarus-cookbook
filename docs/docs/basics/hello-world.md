# Run a Hello World!

## Hello World! in Lazarus IDE

### Setup a Project using a template and Save

1. Open [Lazarus IDE](https://www.lazarus-ide.org).
2. On the top menu bar, click `Project -> Simple Program -> OK`
3. Save your Project, by clicking `Project -> Save Project`, save the Project file as `HelloWorld.lpi` in a new folder. Lazarus will save the main source file as `HelloWorld.lpr`.
4. You will see a simple program in the **Source Editor** window. The `program`'s name will be the same as the Project's name, as shown below.

```pascal linenums="1"
program HelloWorld;

begin
end.
```

5. Now insert the following line between `begin` and `end.`.

```pascal
WriteLn('Hello World!');
```

6. Add the following compiler directives after the `program` declaration. ==Make sure to add this line in all your Object Pascal codes==. 

```pascal
{$mode objfpc}{$H+}{$J-}
```

Your final code would look as follows.

```pascal hl_lines="3 6" linenums="1"
program HelloWorld;

{$mode objfpc}{$H+}{$J-} // Add this line in your object pascal codes.

begin
  WriteLn('Hello World!');
end.
```

6. Press ++ctrl+s++ to save the code.

### Compile and Run

Press ++f9++ to run the compile and run the program.

Can you see the `Hello World!`? 

Does the console closes immediately?

You're on the right track. See [How to prevent a console from closing when run in Lazarus IDE](#how-to-prevent-a-console-from-closing-when-run-in-the-lazarus-ide)

## Hello World! in your fav editor

### Create a `.pas` file

1. Launch your favourite text editor
2. Create a new file and put the following snippet in it.

```pascal linenums="1"
{$mode objfpc}{$H+}{$J-} 

begin
    WriteLn('Hello World!');
end.
```

3. Save it as `HelloWorld.pas`.

### Compile using  `fpc` and run

#### Windows CLI

On Windows, compile and run as follows.

```bash
fpc HelloWorld.pas && HelloWorld.exe
```

If running `fpc` from CLI doesn't work, try one of the following options.

1. Supply the full path to the `fpc`.
2. Put the `fpc/bin/<architecture>` to your `PATH` then compile and run again.
3. Consider using [Lazarus IDE](https://www.lazarus-ide.org) instead.
4. Are you a [VSCode](https://code.visualstudio.com) or [VSCodium](https://vscodium.com) user? Make sure to setup [Pascal by Allesandro Fragnani](https://marketplace.visualstudio.com/items?itemName=alefragnani.pascal) properly.
5. Have you considered [OmniPascal](https://www.omnipascal.com)?

#### Linux  CLI

On Linux, compile and run as follows.

```bash
fpc HelloWorld.pas && ./HelloWorld.exe
```

## How to prevent a console from closing when run in the Lazarus IDE?

When you run a console program (non-GUI), the console may dissapear immediately after you press ++f9++. You can prevent this by adding [`ReadLn;`](https://www.freepascal.org/docs-html/rtl/system/readln.html) before the program ends.

```pascal hl_lines="9" linenums="1"
program HelloWorldPause;

{$mode objfpc}{$H+}{$J-} // Add this line in your object pascal codes.

begin
  WriteLn('Hello World!');

  WriteLn('Press Enter key to exit');
  ReadLn;
end.                      
```

## Can we make the directives more readable?

!!! Contribution

    𝓚𝓸𝓭𝓮𝓩𝔀𝓮𝓻𝓰 🇩🇪 and Gustavo 'Gus' Carreno 🇵🇹 (Unofficial Free Pascal Discord Server) suggested making directives more readable. paweld 🇵🇱, for pointing out `{$longstrings on}`.

    Thank you!

Sure. You can use the long names of compiler directives. See [Compiler Directives](https://www.freepascal.org/docs-html/prog/progch1.html#progse2.html). 

```pascal linenums="1" hl_lines="4-6"
program HelloWorldAlt;

  // Here is an example using more readable complier directives
  {$mode objfpc}
  {$longStrings on}
  {$writeableConst off}

begin
  WriteLn('Hello World!');

  WriteLn('Press Enter key to exit');
  ReadLn;
end.
```