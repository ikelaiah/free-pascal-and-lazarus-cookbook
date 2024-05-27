# Multi-threading

## Overview

!!! info
    The information in this section is a simplified version of the content available at the following URL; [Multithreaded Application Tutorial](https://wiki.freepascal.org/Multithreaded_Application_Tutorial), excluding the tutorial.

A multi-threaded application can run multiple tasks simultaneously by creating separate threads. The Main Thread manages user interactions, while other threads handle background tasks. 

This setup improves application responsiveness during intensive operations. Multi-threading is beneficial for running tasks in the background, maintaining user interface responsiveness, and managing multiple client requests in server applications.

### Do you need multi-threading?

Multi-threading is good for:

- Managing blocking handles like network tasks.
- Using multiple processors at once to process a large dataset.
- Etc.

Before using multi-threading to speed up tasks using many processors, make sure of the following items. 

1. Your program is already using all resources well on one CPU core. 
2. Check if your program is optimized at the highest level (level 3) for the best performance, as higher optimization can make your program faster.


### Multi-threaded apps are complex and harder to debug

For simpler tasks, one thread may be enough. Instead of many threads, you can split long tasks into smaller parts or use `Application.ProcessMessages` to handle user actions during long tasks.

!!! info
    What is `Application.ProcessMessages`?
    
    `Application.ProcessMessages` signals that the app can execute events from its event queue. Let's say that you have 2 buttons on a form with to `onclick` procedures assigned. The first procedure is a lengthly process (eg. `repeat..until true`). The second button has only `ShowMessage('haha')`.

    Now, without `Appllication.ProcessMessages` inserted in the first procedure in the repeat statement, if you press the first button then you will not be able to press the seccond button (or anything else) until the repeat statement finishes. So the user interface is frozen.

    With the `application.processmessages` inserted as follows

    ```pascal
    repeat
      Application.ProcessMessages;
      ...
    until true;
    ```

    If you press the first button and then the second button the `ShowMessage` will happen! So, it is a way to fake a multithread app :-))

    Source: [Lazarus: The effect of Application.ProcessMessages](https://stackoverflow.com/a/24789033/1179312)



### Units needed for a multi-threaded application

#### Windows

You don´t need any special unit for this to work with Windows. 

#### Linux macOS FreeBSD

With Linux, macOS and FreeBSD, you need the `cthreads` unit and it must be the first used unit of the project (the program source, usually the `.lpr` file)! 

In cases where you need  units like `cmem`, `cthreads`, and `cwstrings`; 

1. Place them first in the `uses` section, 
2. Due to how these units work, a sensible order is `cmem`, `cthreads` and `cwstrings`.

So, your FPC/Lazarus application code should begin as the following snippet.


```pascal linenums="1"
program AMultiThreadedProgram;

{$mode objfpc}{$H+}{$J-}
uses
{$ifdef unix}
  cmem, // the c memory manager is faster for multi-threading
        // on some systems
  cthreads,
{$endif}
  { you can add units here };

  // ... the rest of your code
```

## Perform tasks on multiple threads


!!! Contribution

    2024-02-08 - paweld 🇵🇱 caught a memory leak in the original code and fixed it.

    Thank you!

1. Create a class, for example `TTaskThread`, based on `TThread`. 
2. Override `Execute`. Line 27-34.
    - This procedure contains your task to perform.
3. Define a constructor. Line 36-43.
    - call constructor of `TThread`,
    - set free on terminate and
    - Start thread. 
4. Create all threads.

```pascal linenums="1" hl_lines="17-24 27-34 37-44 55 56"
program CreateThreads;

// 2024-02-08 - paweld 🇵🇱 fixed a memory leak issue in the original code.

{$mode objfpc}{$H+}{$J-}

uses
  {$ifdef unix}
  cmem, cthreads,
  {$endif}
  Classes,
  SysUtils;

type
  // Create a class based on TThread
  // TTaskThread
  TTaskThread = class(TThread)
  protected
    // Override the Execute procedure of TThread
    procedure Execute; override;
  public
    // Thread constructor with free on terminate
    constructor Create;
  end;

  // The Execute procedure, simulating a task
  procedure TTaskThread.Execute;
  begin
    WriteLn('Started a task on thread ID ', ThreadID);

    Sleep(Random(5)); // simulate a time to complete a task

    WriteLn('Sending a terminate signal to thread ID: ', ThreadID);
  end;

  // Constructor of TTaskThread
  constructor TTaskThread.Create;
  begin
    //create suspended
    inherited Create(True);
    FreeOnTerminate := True;
    //run thread
    Start;
  end;

var
  task1, task2: TThread;

begin
  WriteLn('---------------------');
  WriteLn('Started TThread demo');
  WriteLn('---------------------');

  // Create all threads
  task1 := TTaskThread.Create;
  task2 := TTaskThread.Create;

  // Start a task on the main thread
  Writeln('Starting a task from the main thread');
  Sleep(Random(5)); // simulate a task
  Writeln('Completed the task from the main thread');

  WriteLn('---------------------');
  WriteLn('Finished TThread demo');
  WriteLn('Press Enter to quit');
  WriteLn('---------------------');
  ReadLn;
end.
```

