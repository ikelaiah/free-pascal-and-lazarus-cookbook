# Multi-threading | Snippets

## Muti-threaded array sum - manually free threads afer collecting results

Important features of this example.

1. Use of `TIntegerDynArray`, which is a dynamic array of integers.
2. Thread Class `TMyThread` inherits from `TThread`. This class encapsulates the logic for summing a segment of the array within each thread.
3. The constructor `TMyThread.Create` initialises the thread with the input array, start index, and end index. This setup ensures each thread works on a specific portion of the array.
4. The `Execute` method in `TMyThread` performs the actual summing of numbers within the assigned segment of the array.
5. Main program
      - Calculates the segment size for each thread using `Math.Ceil((Length(inputArray) + MAX_THREADS - 1) / MAX_THREADS)`, ensuring that each thread gets an equal or almost equal portion of the array to process.
      - Creates and starts each thread, passing in the relevant segment of the array.
      - After all threads complete their work, collect the partial sums from each thread and calculate the total sum.
6. Manual memory cleanup by setting `FreeOnTerminate := False` and manually freeing the threads after collecting their results.
7. Lastly, the program displays information about each thread's segment and its partial sum.

```pascal linenums="1"
program EX2MultiThread;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  Types,
  Generics.Collections,
  Math { you can add units after this };

  // --- Custom thread type --------------------------------------
type
  // The TThread class encapsulates the native thread support of the OS.
  // To create a thread, (1) declare a child of the TThread object, ...
  TMyThread = class(TThread)
    // (with a data to work with)
  private
    // Variables to store the input array for this thread to sum, along with
    // start index and end index
    anArray: TIntegerDynArray;
    startIdx: integer;
    endIdx: integer;
    // The sum of array in the thread
    partialSum: integer;
  protected
    // (2) override the Execute method, and ...
    procedure Execute; override;
  public
    // (3) lastly, you may include a constructor to setup variables
    // for executing this thread.
    constructor Create(const isSuspended: boolean;
                       var   inputArray: TIntegerDynArray;
                       const startIndex: integer;
                       const endIndex: integer);
  end;

  constructor TMyThread.Create(const isSuspended: boolean;
                               var   inputArray: TIntegerDynArray;
                               const startIndex: integer;
                               const endIndex: integer);
  begin
    // Call parent's constructor
    // If user pass True, thread won't start automatically
    inherited Create(isSuspended);

    // Assign a data to work with.
    self.anArray := inputArray;
    self.startIdx := startIndex;
    self.endIdx := endIndex;

    // DO NOT Free thread when finished here.
    // The main thread will ...
    //   1. collect the results from n threads,
    //   2. free n threads from the main thread.
    FreeOnTerminate := False;
  end;

  procedure TMyThread.Execute;
  var
    index: integer;
  begin
    // Execute thread, and DO SOMETHING in this thread.

    // Initialise partialSum to 0 to start with
    self.partialSum := 0;

    // partialSum the numbers in the assigned array using for..do loop.
    for index := self.startIdx to self.endIdx do
      self.partialSum := self.partialSum + self.anArray[index];

    // Display user feedback from this thread
    WriteLn('Thread ', ThreadID, ' summed up ', self.partialSum);
  end;

// const and var for the main block ----------------------------
const
  // Specify max number of threads to use.
  MAX_THREADS = 4;
  // The length of an input array may come from a file.
  INPUT_ARRAY_LENGTH = 10000;

var
  // Input array containg numbers to sum.
  inputArray: TIntegerDynArray;
  // Setting an array for the threads.
  myThreads: array of TMyThread;
  // Size of a segment for each thread
  segmentSize: integer;
  // total sum -- will collect values from each thread
  totalSum: integer;
  // Indexes
  index, startIndex, endIndex: integer;

  // Main block ------------------------------------------------
begin

  // Populate input array. This may come from a file.
  SetLength(inputArray, INPUT_ARRAY_LENGTH);
  for index := 0 to INPUT_ARRAY_LENGTH - 1 do
    inputArray[index] := index + 1;

  // Calculate segment size for each thread
  segmentSize := Math.Ceil((Length(inputArray) + MAX_THREADS - 1) / MAX_THREADS);

  // Create and start the threads.
  SetLength(myThreads, MAX_THREADS);
  for index := 0 to MAX_THREADS - 1 do
  begin
    // Start index for a thread is i * segmentSize.
    startIndex := index * SegmentSize;
    // Ensure that each thread processes the correct portion of the array
    // without going out of bounds on last iteration.
    endIndex := Min((index + 1) * SegmentSize - 1, Length(inputArray) - 1);

    // Show user info.
    WriteLn('startIndex: ', startIndex, ' ', ' endIndex:', endIndex);

    // Create a thread.
    myThreads[index] := TMyThread.Create(False, inputArray, StartIndex, EndIndex);
    // Start this new thread.
    myThreads[index].Start;
  end;

  // Wait until a thread is done, sum up and free it.
  totalSum := 0;
  for index := 0 to MAX_THREADS - 1 do
  begin
    // Wait until thread index n finishes
    myThreads[index].WaitFor;
    // Get the partial sum from thread index n
    totalSum := totalSum + myThreads[index].partialSum;
    // Lastly, free thread index n
    myThreads[index].Free;
  end;

  // Display results
  WriteLn('Total sum of array is: ', totalSum);

  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## Assigns a student ID to each student name from a text file

This program assigns ID to each student name from a text file and sort by student ID.

This snippet features;

- `TFIleStream` and `TStreamREader` for reading lines from a text file,
- use of four threads to complete the task,
- use of rounding up division to split workload between threads, and
- use of `TRTLCriticalSection` to create a critical section ensuring only one thread can write into the output list.


### The `common.pas` of assigning a student ID to each name

This file holds the common type and variable declarations.

There are two important common variable here.

1. `startStudentID: int64 = 200000;`. This variable specify the lowest index for student ID. All threads will be reading from this variable and increase by one for other threads to read from. Hence, reading and increment of this variable **MUST** be done from within a critical section to avoid race.
2. `finalStudentList: TStudentList;`. A list of `TStudent` records containing names and student IDs. All threads will write the output here. So, writing to this variable **MUST** be done from within a critical section too.
  

```pascal linenums="1"
unit Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, Math;


type
  // A record to hold student information.
  TStudent = record
    Name: string;
    id: integer;
  end;

type
  // A list to hold student records, along with a comparer for
  // sorting the list afterwards.
  TStudentList = specialize TList<TStudent>;
  TStudentListComparer = specialize TComparer<TStudent>;

type
  // A type of string list.
  TStrList = specialize TList<string>;

const
  // Number of maximum threads to use.
  maxThreads: int64 = 4;

var
  // This variable specify the lowest index for student ID.
  // All threads will be reading from this variable and increase by one
  // for other threads to read from.
  // Hence, reading and increment of this variable MUST be done
  // from within a critical section to avoid race.
  startStudentID: int64 = 200000;

  // A list of TStudent records. All threads will write student names and
  // student IDs into this variable.
  // Hence, writing to this variable MUST be done from within
  // a critical section too.
  finalStudentList: TStudentList;

// Custom comparison function for sorting by name - ascending
function CompareID(const LeftItem, RightItem: TStudent): integer;

implementation

// Custom comparison function for sorting by student id - ascending
function CompareID(const LeftItem, RightItem: TStudent): integer;
begin
  Result := CompareValue(LeftItem.id, RightItem.id);
end;

end.
```

### The `customthread.pas` of assigning a student ID to each name

This file define the implementation of a custom thread based on `TThread`.

There are imporant things to note here.

1. The implementation includes a `destructor` to clean up the list used by the threads for the task. Lines 30, 63-69.
2. The thread receives an array, but only process a portion of it based on the rounding up division algorithm defined in the main block.
3. The `FreeOnTerminate := True` as the main thread is not collecting results from this thread. This thread writes results into a shared variable. Line 47.
4. The `Execute` method updates shared variables in a critical section. Line 79-91.

```pascal linenums="1"  hl_lines="47 30 63-69 79-91"
unit CustomThread;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Common;

  // Create a thread class deriving from TThread.
type
  // The TThread class encapsulates the native thread support of the OS.
  // To create a thread, (1) declare a child of the TThread object, ...
  TCustomThread = class(TThread)
    // (with a data to work with)
  private
    // A TStrList to store the input array for this thread, along with
    // a variable to store an instance of critical section.
    list: TStrList;
    cs:TRTLCriticalSection;
  protected
    // (2) override the Execute method, and ...
    procedure Execute; override;
  public
    // (3) include a constructor to setup variables for executing this thread.
    constructor Create(const criticalSection: TRTLCriticalSection;
                       const listToProcess: TStrList;
                       const startIndex, finishIndex: int64);
    // (4) lastly, include  destructor to free the TStrList of this thread.
    destructor Destroy; override;
  end;

implementation

// Create the Custom Thread with an input list to process.
constructor TCustomThread.Create(const criticalSection: TRTLCriticalSection;
                                 const listToProcess: TStrList;
                                 const startIndex, finishIndex: int64);
var
  index: int64;
begin
  // Call parent's constructor
  // If user pass True, thread won't start automatically
  inherited Create(True);

  // Free threads on terminate.
  FreeOnTerminate := True;

  // Assign critical section
  self.cs := criticalSection;

  // Populate the internal list for the Execute procedure
  self.list := TStrList.Create;
  for index := startIndex to finishIndex do
  begin
    self.list.Add(listToProcess[index]);
  end;

  // User feedback
  WriteLn('Thread created with id: ', ThreadID);
end;

destructor TCustomThread.Destroy;
begin
  // Free the TStrList.
  self.list.Free;
  // Call parents' Destroy.
  inherited Destroy;
end;

// Enter and leave Critical Section here.
procedure TCustomThread.Execute;
var
  index: int64;
  student: TStudent;
begin
  for index := 0 to self.list.Count - 1 do
  begin
    EnterCriticalSection(cs); // --------------------------------- enter cs
    try
      // Add student - ID pair as TStudent, then add into TStudentList
      //   1. Get the name from the list with allocated index
      student.Name := list[index];
      //   2. Get the starting student ID from
      student.id := startStudentID;
      //   3. Add TStudent into TStudentList (the main block does the init)
      finalStudentList.Add(student);
      // After a student - ID pair is added, increment the current student ID by 1
      startStudentID := startStudentID + 1
    finally
      LeaveCriticalSection(cs); // ------------------------------- leave cs
    end;
  end;
end;

end.
```

### The main program of of assigning a student ID to each name

Key features of the main snippet:

- The text file is read into `strList`.
- Threads are created and assigned specific subarrays to process.
- Critical sections ensure safe access to shared variables.
- After threads finish, results are sorted and printed.

```pascal linenums="1"
program AssignStudentIDs;

{
 This program assigns student ID to each name from a text file by
 using N number of threads.

 Pre-requisite

    - TThread for managing the threads.
    - TRTLCriticalSection for ensuring only one thread can modify a shared
      variable at one time.
    - Input text file containing a list of names. For example;

    Alyssa Morgan
    Declan Hayes
    Nora Patel
    Miles Thompson
    Sienna Larson
    Kellan Rivera
    Camille Chang
    Jensen Park
    Amara Singh
    Holden Myers
    Elise Howard
    Luca Griffin
    Reagan Patel
    Kian Gallagher
    Mara Nguyen
    ...
    ...

 Algorithm

    - Read the text file into an array.
      - All threads will read from the same array, but at differing start and
        finish indexes. This depends on the number of max threads.
    - Assign workloads to each thread.
      - Specify the start and finish indexes to each thread.
      - Will use the rounding up division method to ensure near-equal division
        of workload for each thread.
    - Wait until all task is done
    - Sort the final student list
    - Print results on screen.

 Sample Output

   $ ./AssignStudentIDs.exe student-names.txt
   No of students         : 200
   Max threads            : 4
   subArray size round up : 51
   ---------------------------------
   Thread created 25040
   Thread created 26324
   Thread created 11972
   Thread created 26028
   Starting threads ...
   Waiting for threads to finish ...
   All threads are done ...
   Printing results ...
   Alyssa Morgan, 200000
   Declan Hayes, 200001
   Nora Patel, 200002
   Miles Thompson, 200003
   Sienna Larson, 200004
   Kellan Rivera, 200005
   Camille Chang, 200006
   Jensen Park, 200007
   Amara Singh, 200008
   Holden Myers, 200009
   Elise Howard, 200010
   Luca Griffin, 200011
   Reagan Patel, 200012
   ...
   ...

}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem
  , cthreads
  {$ENDIF}
  Classes,
  SysUtils,
  Math,
  streamex,
  Common,
  CustomThread;

  // All the variables and procedures to get the job done.
var
  // Critical Section preventing threads accessing a variable at the same time.
  customCriticalSection: TRTLCriticalSection;

  // TStreams for reading files
  fileStream: TFileStream;
  streamReader: TStreamReader;

  // A temporary list to hold student names from a text file
  strList: TStrList;

  // An array of threads
  myThreads: array of TThread;

  // Variables for calculating subarray size for each thread
  subArraySize, index: int64;

  // Main block ////////////////////////////////////////////////////////////////
begin
  try
    // 1. Read input text file and populate input array from a text file
    if not FileExists(ParamStr(1)) then
    begin
      WriteLn(Format('%s does not exist.', [ParamStr(1)]));
      Exit;
    end;

    strList := TStrList.Create;
    finalStudentList := TStudentList.Create;
    try
      fileStream := TFileStream.Create(ParamStr(1), fmOpenRead);
      try
        streamReader := TStreamReader.Create(fileStream, 65536, False);
        try
          while not streamReader.EOF do
          begin
            // Add each line into a list
            strList.Add(streamReader.ReadLine);
          end;
        finally
          streamReader.Free;
        end;
      finally
        fileStream.Free
      end;

      // 2. Init Critical Section as we have threads writing to a shared variable
      InitCriticalSection(customCriticalSection);

      // 3a. set the number of threads in array of TThread
      SetLength(myThreads, maxThreads);
      try
        {
          3b. Now we add threads & assign workloads, using rounding up division --
              Ceil((totalElements + N - 1) div N).
              - When we divide totalElements by N, we get the quotient of the
                division. However, if totalElements is not evenly divisible by N,
                there might be a remainder.
              - In the context of splitting an array into subarrays, we want
                each subarray to have approximately the same number of elements.
                Therefore, we want to ensure that any remaining elements after
                dividing totalElements by N are included in the last subarray to
                avoid losing data.
              - The expression Ceil((totalElements + N - 1) div N); effectively
                rounds up the division by adding N - 1 to totalElements before
                performing the division. **This ensures that any remainder is
                accounted for in the last subarray**.
        }
        subArraySize := Math.Ceil((strList.Count + maxThreads - 1) / maxThreads);

        // Show user feedback
        WriteLn('No of students         : ', IntToStr(strList.Count));
        WriteLn('Max threads            : ', IntToStr(maxThreads));
        WriteLn('subArray size round up : ', IntToStr(subArraySize));
        WriteLn('---------------------------------');

        {
         3c. Assign workload to each thread by using the following info;
             - source list
             - start index and
             - finish index for a thread
        }
        for index := 1 to maxThreads do
        begin
          myThreads[index - 1] := TCustomThread.Create(customCriticalSection,
                                                       strList,
                                                       ((index - 1) * subArraySize),
                                                       Math.Min(index * subArraySize - 1, strList.Count - 1));
        end;

        // 4. Start all threads
        WriteLn('Starting threads ...');
        for index := 0 to High(myThreads) do
          myThreads[index].Start;

        // 5. Wait for both threads to finish
        WriteLn('Waiting for threads to finish ...');
        for index := 0 to High(myThreads) do
          myThreads[index].WaitFor;
        WriteLn('All threads are done ...');

        // 6. Sort by student ID
        finalStudentList.Sort(TStudentListComparer.construct(@CompareID));

        // 7. Show results
        WriteLn('Printing results ...');
        for index := 0 to finalStudentList.Count - 1 do
          WriteLn(finalStudentList[index].Name, ', ', finalStudentList[index].id);

        // 8. Show user feedback
        WriteLn('---------------------------------');
        WriteLn(Format('Output list contains %d items', [finalStudentList.Count]));
        WriteLn('---------------------------------');
      finally
        // 7. Free Critical Section
        DoneCriticalSection(customCriticalSection);
      end;
    finally
      finalStudentList.Free;
      strList.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;
end.
```