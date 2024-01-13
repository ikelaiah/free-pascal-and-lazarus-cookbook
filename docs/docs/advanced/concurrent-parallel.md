# Threading(Concurrency) and Parallelism

Threading = Do various tasks together.

Parallelism = Do one task in less time using multiple cores.

## Perform tasks on multiple threads

```pascal linenums="1"
program Threading;

{$MODE OBJFPC}{$H+}{$J-}

uses
  {$ifdef unix}
  cthreads, cmem
  {$endif}
  Classes,Sysutils;

type
  // Create a class based on TThread
  TTaskThread = class(TThread)
    // Override the Execute procedure of TThread
    procedure Execute; override;
  end;

// The Execute procedure does the counting from 0 to 5.
// Finally, it will;
//     1. send Terminate signal to the thread, and
//     2. wait for the thread to terminate and returns the exit status.
procedure TTaskThread.Execute;
begin
  WriteLn('Started a task on thread ID ',ThreadID);

  Sleep(Random(5)); // simulate a time to complete a task

  WriteLn('Sending a terminate signal to thread ID: ',ThreadID);
  self.Terminate;
  self.WaitFor;
end;

var
  task1,task2: TThread;

begin
  WriteLn('---------------------');
  WriteLn('Started TThread demo');
  WriteLn('---------------------');

  // create all threads in suspended state
  task1 := TTaskThread.Create(true);
  task1.FreeOnTerminate := True;

  task2 := TTaskThread.Create(true);
  task2.FreeOnTerminate := True;

  // Start all threads
  task1.Start;
  task2.Start;

  // Start counting on the main thread
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

