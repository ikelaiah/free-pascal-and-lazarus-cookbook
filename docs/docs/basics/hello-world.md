# Run a Hello World!

## Hello World! using the Lazarus IDE

### Setup a Project and Save the Source File

1. Open [Lazarus IDE](https://www.lazarus-ide.org).
2. On the top menu bar, click `Project -> Simple Program -> OK`
3. Save your Project, by clicking `Project -> Save Project`, save the Project file as `hello_world.lpi` in a new folder. Lazarus will save the main source file as `wello_world.lpr`.
4. You will see a simple program in the **Source Editor** window. The `program`'s name will be the same as the Project's name, as shown below.

```pascal
program hello_world;

begin
end.
```

5. Now insert the following line between `begin` and `end.`.

```pascal
writeln('Hello World!');
```

Your final code would look as follows.

```pascal hl_lines="4"
program hello_world;

begin
    writeln('Hello World!');
end.
```

6. Press ++ctrl+s++ to save the code.

### Compile and Run in the Lazarus IDE

Press ++f9++ to run the compile and run the program.

## Hello World! using Your Favourite Text Editor

### Create the Source File

1. Launch your favourite text editor
2. Create a new file and put the following snippet in it.

```pascal
begin
    writeln('Hello World!');
end.
```

3. Save it as `hello_world.pas`.

### Compile using  `fpc` and run

#### Windows CLI

On Windows, compile and run as follows.

```bash
fpc hello_world.pas && hello_world.exe
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
fpc hello_world.pas && ./hello_world.exe
```

## Prevent a console program from disappearing when run in the Lazarus IDE

When you run a console program (non-GUI), the console may dissapear immediately after you press ++f9++. You can prevent this by adding [`readln;`](https://www.freepascal.org/docs-html/rtl/system/readln.html) before the program ends.

```pascal hl_lines="4"
begin
  writeln('Hello World!');
  writeln('Press Enter key to exit');
  readln;
end.                                 
```