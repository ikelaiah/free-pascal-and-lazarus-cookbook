# Debugging 

## Official docs on debugging Free Pascal programs

Here is the official docs by Michaël Van Canneyt and Florian Klämpfl; [Debugging your Program](https://www.freepascal.org/docs-html/user/userch10.html).


## Official docs on using Heaptrc to detect memory leaks

- [RTL - `heaptrc`](https://www.freepascal.org/docs-html/rtl/heaptrc/index.html)
- [RTL - `SetHeapTraceOutput`](https://www.freepascal.org/docs-html/rtl/heaptrc/setheaptraceoutput.html)
- [Controlling HeapTrc with environment variables](https://www.freepascal.org/docs-html/rtl/heaptrc/environment.html)
- [Using Heaptrc in FPC](https://wiki.freepascal.org/heaptrc)
- [Using LeakView in Lazarus](https://wiki.freepascal.org/leakview)

## Detecting heap memory leak in Lazarus

### 1. Enable Heaptrc in Lazarus

1. First, go to `Project | Project Options ...` 
2. In the Options window find `Compiler Options | Debugging`, then enable the following switches.

- **[Mandatory]** `Use Heaptrc unit (check for mem-leaks) (-gh)`
- **[Optional]** `Display line numbers in run-time errors backtraces (-gl)`

![Project options window](../../assets/use-heaptrc-unit-check-mem-leaks.png)

### 2. Redirect Heaptrc report to a file

Simply use [`SetHeapTraceOutput`](https://www.freepascal.org/docs-html/rtl/heaptrc/setheaptraceoutput.html) to redirect heap trace report to a file.

Here is an example.

1. Define a `DEBUG` symbol. We contain the heap trace report only in debug builds. Line 7.

      - FYI, the `{$DEFINE}` directive has a command-line equivalent, `-d`.  For example, `-dNAME`

2. Create a conditional compilation block for `DEBUG` builds, `{$IFDEF DEBUG}...{$ENDIF DEBUG}`, for redirecting heap trace to a file. Line 23-31.

      - Remove existing heap trace file. Line 28, 29.
      - Set a file for reporting heap memory leaks. Line 30.

```pascal linenums="1" hl_lines="7 23-31"
program HeapMemoryLeakLazarus;

{$mode objfpc}{$H+}{$J-}

// Define a symbol name DEBUG.
// With this compiler directives we can compile parts of code associated with this symbol.
{$DEFINE DEBUG}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

var
  stringList: TStringList;
  i: integer;

  // MAIN block
begin

  {$IFDEF DEBUG}
  // This block assumes your build mode sets -dDEBUG in `Project Options` or other means when defining -gh.
  // For production build, remove -dDEBUG in `Project Options` or other means and disable -gh.

  // Setup Heaptrc output for the Leak and Traces window in Lazarus.
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}

  // Create a string list
  stringList := TStringList.Create;

  try
    // Adding items
    WriteLn('Adding items');
    WriteLn('--------------------');
    for i := 0 to 4 do
      stringList.Add('Counting ' + IntToStr(i));

    // Printing contents
    for i:=0 to stringList.Count - 1 do
      WriteLn(stringList[i]);

  finally
    // If you don't free, the -gh will give report of memory leaks
    // If Leak and Traces window is set to a heap trace file, this will appear in the Leak and Traces windoww.
    // Otherwise, Heaptrc will print heap memory reports on CLI.
    stringList.Free;
  end;

  // Pause Console
  WriteLn('Press Enter key to quit.');
  ReadLn;
end.
```

After you run the program, you will get a heap trace report file.

### 3. View Leaks and Traces report

1. Click `View` form the top menu bar of Lazarus IDE.
2. Select `Leaks and Traces`
3. Set the `.trc` file to read by pressing the `...` (elipsis) button.
4. Click the **Update** button to reload latest changes to the `.trc` file.

![Project options window](../../assets/opening-leaks-and-traces-window.png)

Now, whenever you run the program in DEBUG mode from Lazarus IDE, simply press the **Update** button on the **Leaks and Traces** window to see the latest heap memory report.

If there is any leaks in the program, pressing the **Update** button will show leaking memory size, leaking blocks counts and the details of leaking blocks.

![Project options window](../../assets/leaks-in-leaks-and-traces-window.png)