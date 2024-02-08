# Threading(Concurrency) and Parallelism

Threading = Do various tasks together.

Parallelism = Do one task in less time using multiple cores.

## Perform tasks on multiple threads


!!! Contribution

    2024-02-08 - paweld 🇵🇱 caught memory leaks in the original code and fixed it.

    Thank you!

1. Create a class, for example `TTaskThread`, based on `TThread`. Prepare to override `Execute` and define a constructor `Create`. Line 17-24.
2. Override `Execute`. Line 27-34.
    - This procedure contains your task to perform.
3. Define a constructor. Line 36-43.
    - call constructor of `TThread`,
    - set free on terminate and
    - Start thread. 
4. Create all threads.

```pascal linenums="1" hl_lines="17-24 27-34 37-44 55 56"
program CreateThreads;

// 2024-02-08 - paweld 🇵🇱 fixed memory leak issues in the original code.

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

