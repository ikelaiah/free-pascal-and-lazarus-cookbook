# File Handling

## Writing a new text file

There are many ways to do this;

1. The classic style (using the [File Handling Functions](https://www.freepascal.org/docs-html/rtl/system/filefunctions.html)) and 
2. The object style, including:

      - [`TFileStream`](https://www.freepascal.org/docs-html/rtl/classes/tfilestream.html).
      - [`TStringList`](https://www.freepascal.org/docs-html/rtl/classes/tstringlist.html) and



### New text file - Classic

See the snippet below. It uses `SysUtil` for catching errors during opening and writing file.

1. Use `AssignFile` to assign a text file to a file type `TextFile`. Line 14.
2. Use `Rewrite` to open file for writing (and create if doesn't exists). Line 19.
3. Add text using the assigned file type and `WriteLn`. Line 22.
4. Close the file with `CloseFile` in the `try..finally` block. Line 31.


> *[When saving file the classic way]* you should always strive to have a `try..finally` outside the `try..except`.
> 
> -Gustavo 'Gus' Carreno

```pascal linenums="1" hl_lines="14 19 22 31"
program ClassicNewTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

var
  textFile: System.TextFile;

begin
    try
      // Set the name of the file that will be created
      AssignFile(textFile, 'output_file.txt');

      // Enclose in try/except block to handle errors
      try
        // Create (if not found) and open the file for writing
        Rewrite(textFile);

        // Adding text
        WriteLn(textFile, 'Hello Text!');

      except
        // Catch error here
        on E: EInOutError do
          WriteLn('Error occurred. Details: ', E.ClassName, '/', E.Message);
      end;
    finally
      // Close file
      CloseFile(textFile);
    end;

  // Pause console
  ReadLn;
end.
```

Overly verbose with `try...finally`? It can be written as follows too.

```pascal linenums="1"
program ClassicNewTextFileSimple;

{$mode objfpc}{$H+}{$J-}

var
  textFile: System.TextFile;

begin
  AssignFile(textFile, 'output_file.txt');
  ReWrite(textFile);
  WriteLn(textFile, 'This is a new line');
  CloseFile(textFile);
end.
```

!!! warning

    But what will happen if the file if locked by another process? 

    What will happen when the file accidentally got deleted before closing it?

    Boom! It will crash.

I'd prefer to handle error gracefully. Hence, I like the first snippet better.


### New text file - Classic alt

You can streamline the process of writing text to a file by refactoring the lines into a procedure.

Here is an example. 

- The lines that write text is refactored into a procedure. See line 9-36.
- Now, writing a text into a file is in a line. See line 41.

```pascal linenums="1" hl_lines="9-34 39"
program ClassicNewTextFileOrganised;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

  // Write or append a text to a file
  procedure WriteTextToFile(fileName: string; stringText: string;);
  var
    textFile: System.TextFile;
  begin
    try 
      // Set the name of the file that will be created
      AssignFile(textFile, fileName);

      // Enclose in try/except block to handle errors
      try
        // Create (if not found) and open the file for writing
        Rewrite(textFile);

        // Adding text
        WriteLn(textFile, stringText);

      except
        // Catch error here
        on E: EInOutError do
          WriteLn('Error occurred. Details: ', E.ClassName, '/', E.Message);
      end;
    finally
      // Close file
      CloseFile(textFile);
    end;
  end;

begin

  // Write a text to a file
  WriteTextToFile('hello-text.txt', 'Hello There! How are you?');

  // Pause console
  ReadLn;
end.

```


### New text file - `TFileStream`


For writing text into a new file using Object style;

1. create a new file; `TFileStream.Create(fileName, fmCreate);`. Line 14.
2. set the current position in the stream as 0; `TFileStream.Position := 0;`. Line 17.
3. write using `TFileStream.Write(stringText, Length(stringText));` Line 18.
4. `Free` the `TFileStream` object from memory. Lines 22.

```pascal linenums="1" hl_lines="14 17 19 24"
program TFileStreamNewTextFile;

uses
  Classes, SysUtils;

var
  text: string = 'QILT Surveys';
  filename :String = 'hello-text.txt';
  fileStream: TFileStream;
  size: longint;

begin
  // Create a TFileStream object
  fileStream := TFileStream.Create(filename, fmCreate);
  try
    // set position at the beginning of file
    fileStream.Position := 0;
    // Write text into the file
    size := fileStream.Write(text[1], Length(text));
    // Show confirmation
    Writeln(Format('Created %s. %d bytes written.', [filename, size]));
  finally
    // Free TFileStream object
    fileStream.Free;
  end;

  // Pause console
  ReadLn;
end.
```

### New text file - `TFileStream` (alt)

This example is the previous snippet wrapped in a `procedure`.

```pascal linenums="1" hl_lines="32"
program TFileStreamNewTextFileOrganised;

uses
  Classes, SysUtils;

  // Write text into a new file
  procedure WriteStreamToFile(fileName: string; text: string);
  var
    fileStream: TFileStream;
    size: longint;
  begin
    fileStream := TFileStream.Create(fileName, fmCreate);
    try
      // set position at the beginning og file
      fileStream.Position := 0;
      // Write text into the file
      size := fileStream.Write(text[1], Length(text));
      // Show confirmation
      Writeln(Format('Created %s. %d bytes written.', [filename, size]));
    finally
      // Free TFileStream object
      fileStream.Free;
    end;
  end;

var
  myText: string = 'QILT Surveys';
  filename :String = 'hello-text.txt';

begin

  WriteStreamToFile(filename, myText);

  ReadLn;
end.
```

### New text file - `TSringList`

1. Create the `TStringList` object. Line 18.
2. Use the `Add` method to add text or lines. Line 21-22.
3. Use `SaveToFile` to save your `TStringList` into a text file. Line 25.
4. Remember to `Free` the resources. Line 28.


```pascal linenums="1" hl_lines="18 21 22 25 28"
program TStringListNewTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes;

var
  textFileName: string = 'hello-text.txt';
  stringList: TStringList;

begin

  // Create TStringList object
  stringList := TStringList.Create;
  try
    // Add lines
    stringList.Add('Hello Line 1!');
    stringList.Add('Hello Line 2!');

    // Save to a file
    stringList.SaveToFile(textFileName);
  finally
    // Free object
    stringList.Free;
  end;

  // Pause Console
  WriteLn('Press Enter key to exit ...');
  ReadLn;

end.
```

## Create a blank text file

### Blank text file - Classic

Here is an example.

1. Use `AssignFile` to assign a text file to a file type `TextFile`. Line 13.
2. Use `Rewrite` to open file for writing (and create if doesn't exists). Line 18.
3. Close the file with `CloseFile`. Line 21.

```pascal linenums="1" hl_lines="13 18 21"
program ClassicCreateBlankTextFile;

uses
  Classes,
  SysUtils;

var
  filename: string = 'hello-text.txt';
  textFile: System.TextFile;

begin
  // Set the name of the file that will be created
  AssignFile(textFile, filename);

  // Enclose in try/except block to handle errors
  try
    // Open the file for writing (it will create it file doesn't exist)
    ReWrite(textFile);

    // Close file
    CloseFile(textFile);

    // Show a confirmation
    WriteLn('Created a new blank file');

  except
    // Catch error here
    on E: EInOutError do
      WriteLn('Error occurred. Details: ', E.ClassName, '/', E.Message);
  end;

  // Pause console
  ReadLn;
end.
```

### Blank text file - `TFileStream`

Quite straightforward.

1. create a new file; `TFileStream.Create(fileName, fmCreate);`. Line 15.
2. `Free` the `TFileStream` object from memory. Lines 21.


```pascal linenums="1" hl_lines="15 21"
program TFileStreamCreateBlankTextFile;

uses
  Classes, SysUtils;

var
  fileName: String;
  fileStream: TFileStream;

begin
  fileName := 'hello-text.txt';

  try
    // Create a new file without writing anyting into it
    fileStream := TFileStream.Create(fileName, fmCreate);

    // Show a confirmation
    Writeln('Created a blank file: ', fileName);
  finally
    // Free resources
    fileStream.Free;
  end;
end.
```

### Blank text file - `TStringList`

See the snippet below.

1. Create the `TStringList` object. Line 18.
2. Use `SaveToFile` to save your `TStringList` into a text file. Line 22.
3. Remember to `Free` the resources. Line 25.

```pascal linenums="1" hl_lines="18 22 25"
program TStringListBlankFIle;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes;

var
  textFileName: string = 'hello-text.txt';
  stringList: TStringList;

begin

  // Create TStringList object
  stringList := TStringList.Create;
  try

    // Save to a file
    stringList.SaveToFile(textFileName);
  finally
    // Free object
    stringList.Free;
  end;

  // Pause Console
  WriteLn('Press Enter key to exit ...');
  ReadLn;

end.
```

## Append to an existing text file

### Append a text file - Classic

See an example below. The program will create a new file if the file for appending is not found.

1. `AssignFile` and `CloseFile` is in **outer** `try..finally` block. This ensures the file will be closed whenever an IO error occured.
   
      - Assign a string filename to a text file variable. Line 47.
      - Remember to `CloseFile` after appending. Line 66.

2. Appending text is inside the **inner** `try..except` block.

      - Use `Append` to open the file in append mode. Line 52.
      - Use `WriteLn` to add new text into the existing file. Line 55.

```pascal linenums="1" hl_lines="47 52 55 66"
program ClassicAppendTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils;

  // Create a new file, the classical way
  procedure CreateNewFile(filename: string);
  var
    textFile: System.TextFile;
  begin
    // Set the name of the file that will be created
    AssignFile(textFile, filename);

    // Enclose in try/except block to handle errors
    try
      // Open the file for writing (it will create it file doesn't exist)
      ReWrite(textFile);

      // Close file
      CloseFile(textFile);

      // Show a confirmation
      WriteLn(Format('Created a new file: ''%s''', [filename]));

    except
      // Catch error here
      on E: EInOutError do
        WriteLn('Error occurred. Details: ', E.ClassName, '/', E.Message);
    end;
  end;

var
  filename: string = 'hello-text.txt';
  textFile: System.TextFile;

begin

  // First of all, check if the input file exists.
  // If not, create a new text file
  if not FileExists(filename) then
    CreateNewFile(filename);

  try
    // Set filename to a file
    AssignFile(textFile, filename);

    // Enclose in try/except block to handle errors
    try
      // Open a file for appending.
      Append(textFile);

      // Adding text
      WriteLn(textFile, 'New Line!');
      WriteLn(textFile, 'New Line!');

    except
      // Catch error here
      on E: EInOutError do
        WriteLn('Error occurred. Details: ', E.ClassName, '/', E.Message);
    end;

  finally
    // Close file
    CloseFile(textFile);
  end;

  // Pause console
  WriteLn('Press Enter key to quit.');
  ReadLn;
end.
```

### Append a text file - `TFileStream`

Quite straightforward. Remember to free `TFileStream` when done.

See the snippet below.

1. Does the file  exist? 

      - If yes, create `TFileStream` in *append* mode using `TFileStream.Create(filename, fmOpenWrite or fmShareDenyNone)`. Line 23.
      - If not, create `TFileStream` using `fmCreate` mode to create the new file; `TFileStream.Create(filename, fmCreate);`. Line 26.
  
2. Set position to the end of the file. Line 31.
3. Add new text using `fileStream.Write`. Line 34.
4. Free `TFileStream` object. Line 39.

```pascal linenums="1" hl_lines="23 26 31 34"
program TFileStreamAppendTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

var
  filename: string = 'hello-text.txt';
  fileStream: TFileStream;
  size: longint;
  newText: string;

begin

  // First, does the file exist?
  if FileExists(fileName) then
    // If yes, open the file in append mode.
    fileStream := TFileStream.Create(filename, fmOpenWrite or fmShareDenyNone)
  else
    // If not, create a a new file.
    fileStream := TFileStream.Create(filename, fmCreate);

  // Next, start appending.
  try
    // set position at the end of the file
    fileStream.Seek(0, soFromEnd);
    // Write text into the file
    newText := LineEnding + 'A new line!';
    size := fileStream.Write(newText[1], Length(newText));
    // Show confirmation
    Writeln(Format('Appended %s. %d bytes written.', [filename, size]));
  finally
    // Free TFileStream object
    fileStream.Free;
  end;

  // Pause console
  WriteLn('Press Enter to quit.');
  ReadLn;
end.
```

### Append a text file - `TStringList`

The example below starts by checking if the file exists. If the files doesn't exists, exit program early.

1. Create a `TStringList` object if the text file exists. Line 28.
2. Load the existing text into a `TStringList` object. Line 31.
3. Add new text into the `TStringList` object. Line 34-35.
4. Save the appended `TStringList` into the existing file. Line 38.
5. Free resources. Line 43.

```pascal linenums="1" hl_lines="28 31 34 35 38 43"
program TStringListAppend;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

var
  filename: string = 'hello-text.txt';
  text: TStringList;

begin

  // First of all, check if the input file exists.
  // It not, exit program early.
  if not FileExists(fileName) then
  begin
    WriteLn(Format('File %s does not exist. Press Enter to quit.', [filename]));
    ReadLn;
    Exit;
  end;

  // If file exists, create a TStringList object
  text := TStringList.Create;
  try
    // Read an existing file into TStringList object
    text.LoadFromFile(filename);

    // Append more text
    text.Add('New line!');
    text.Add('New line!');

    // Save the appended TStringList file
    text.SaveToFile(filename);
    WriteLn(Format('Saved to %s.', [filename]));

  finally
    // Free object
    text.Free;
  end;

  // Pause Console
  WriteLn('Press Enter to exit.');
  ReadLn;

end.
```

!!! warning

    A `TStringList` is very easy to use, but I wouldn't recommend it for a log file that gets updated very often. It is slow (because the entire file needs to be rewritten when appending just a single line) and it causes unnecessary writes on a ssd drive.

    Sebastian Z, Aug 12, 2018 at 20:56
    
    Source: [https://stackoverflow.com/a/51808874/1179312](https://stackoverflow.com/a/51808874/1179312)

## Read a text file

### Read a text file - Classic

The snippet below will quit if the file to read is not found.

1. Assign a file to read into a `TextFile` variable. Line 24.
2. Within the `try..catch` do the following:

    - Open the file for reading. Line 29.
    - Use a while loop to read the file one line at a time. Line 32-36.
    - Use close the file after reading it. Line 39.

```pascal linenums="1" hl_lines="24 29 32-36 39"
program ClassicReadTextFile;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

var
  filename: string = 'cake-ipsum.txt';
  textFile: System.TextFile;
  line: string;

begin

  // Provide user feedback
  WriteLn(Format('Reading ''%s''', [filename]));
  WriteLn('--------------------');

  // Assign filename to a TextFile variable - set the name of the file for reading
  AssignFile(textFile, filename);

  // Perform the read operation in a try..except block to handle errors gracefully
  try
    // Open the file for reading
    Reset(textFile);

    // Keep reading lines until the end of the file is reached
    while not EOF(textFile) do
    begin
      ReadLn(textFile, line);
      WriteLn(line);
    end;

    // Close the file
    CloseFile(textFile);

  except
    on E: Exception do
      WriteLn('File handling error occurred. Details: ', E.Message);
  end;

  // Pause console
  WriteLn('--------------------');
  WriteLn('Press Enter to quit.');
  ReadLn;
end.
```

### Read a text file - `TFileStream`

See the snippet below as example.

1. Create a `TFileStream` to open a text file for reading. Line 17.
2. Create a `StreamReader` to read the text file. Line 19. 
3. Use the `while not TStreamReader.eof` to read the text file line by line. Line 23-28.
4. `Free` resources when done.

To gracefully handle error during open and read operations, an outer `try..except` is in place.

```pascal linenums="1" hl_lines="17 19 23-28"
program TStreamReaderReadFile;

uses
  Classes,
  SysUtils,
  streamex;

var
  reader: TStreamReader;
  fileStream: TFileStream;
  line, filename:string;
  i: integer;
begin
  // filename to read
  filename:= 'cake-ipsum-.txt';
  try
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      reader := TStreamReader.Create(fileStream);
      try
        // Set line counter to 1
        i := 1;
        while not reader.EOF do
        begin
          line := reader.ReadLine;
          WriteLn(Format('line %d is: %s', [i, line]));
          i := i + 1;
        end;
      finally
        reader.Free;
      end;
    finally
      fileStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;

  // Pause console
  ReadLn;
end.
```

#### Make the `TFileStream` and `TStreamReader` code more readable

The nested `try..free` blocks in the `try..except` might be difficult to read. But the code works, right?

Here is a better strategy.

- The inner `try..free` blocks are now in separate procedures.
- The outer `try..except` can now catch exceptions from procedures.

```pascal linenums="1" hl_lines="11 33 52"
program TStreamReaderReadFileTidy;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  streamex;

  // Read a stream of string line by line
  procedure ReadTextFile(const fileStream: TStream);
  var
    reader: TStreamReader;
    i: integer;
    line: string;
  begin
    reader := TStreamReader.Create(fileStream);
    try
      // Set line counter to 1
      i := 1;
      while not reader.EOF do
      begin
        line := reader.ReadLine;
        WriteLn(Format('line %d: %s', [i, line]));
        i := i + 1;
      end;
    finally
      reader.Free;
    end;
  end;

  // Open a file for reading, and pass the stream to TStreamReader for reading.
  procedure ReadTextFile(const filename: string);
  var
    fileStream: TFileStream;
  begin
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      ReadTextFile(fileStream);
    finally
      fileStream.Free;
    end;
  end;

var
  filename: string;

begin
  // filename to read
  filename := 'cake-ipsum.txt';
  try
    ReadTextFile(filename);
  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;

  // Pause console
  ReadLn;
end.
```

### Read a text file faster - `TBufferedFileStream`

The snippet below was adapted from one of Stephen Ball's articles, [Faster filestream with TBufferedFilestream](https://delphiaball.co.uk/2016/04/29/faster-filestream-tbufferedfilestream/).

The structure is similar to [reading a text file using TFileStream](#read-a-text-file---tfilestream), but here, we use [`TBufferedFileStream`](https://www.freepascal.org/docs-html/fcl/bufstream/tbufferedfilestream.html).

1. In the `uses` section, add `bufstream`. Line 11.
2. Create a `TBufferedFileStream` to open a text file for reading. Line 29.
3. Use the `while fStream.Read(ch, 1) = 1` to keep on reading data until there is no more data to read. Line 32-44.

      - This part sequentially reads through a text file, checking each character one by one. 
      - It combines these characters into lines by joining them together until it finds a newline character, which indicates the end of a line. 
      - Once a complete line is formed, the snippet prints it out using the `WriteLn` function.

4. `Free` resources when done. Line 46.

There is an outer `try..except` is in place to handle error during open and read operations.

```pascal linenums="1" hl_lines="11 29 32-44 46"
program TBufferedFileStreamReadFile;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  bufstream;

var
  fStream: TBufferedFileStream;
  line: string;
  ch: char;

begin

  if Length(ParamStr(1)) < 3 then
  begin
    WriteLn('Invalid input file');
    Exit;
  end;

  try

    // Create TBufferedFileStream object, read only
    fStream := TBufferedFileStream.Create(ParamStr(1), fmOpenRead);
    try
      // Keep on reading until there is no more data to read
      while fStream.Read(ch, 1) = 1 do
      begin
        if ch = #10 then
        begin
          // Display the line.
          WriteLn('Line: ', line);
          // After displaing, reset.
          line := '';
        end
        else
          // If not #10, combine the characters to make up a line.
          line := line + ch;
      end;
    finally
      fStream.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);
  end;

end.
```

### Read a text file - `TStringList`

Here is a snippet.

1. Instantiate a `TStringList` object on line 27.
2. Within the `try..finally` block, load text into a `TStringList` object on line 32.
3. Use a loop to access the contents of the `TStringList` on lines 35-36.
4. Release the `TStringList` object on line 40.
5. Enclose the algorithm in a `try..except` block to handle exceptions when opening or reading the file.


```pascal linenums="1" hl_lines="27 32 35 36 40"
program TStringListReadTextFile;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

var
  filename: string = 'cake-ipsum.txt';
  stringList: TStringList;
  line: string;

// Main block
begin

  // Provide user feedback
  WriteLn(Format('Reading ''%s''', [filename]));
  WriteLn('--------------------');

  // Start try..except
  try
    // Create the TSTringList object
    stringList := TStringList.Create;

    // Start try..finally
    try
      // Read the file into a TStringList
      stringList.LoadFromFile(filename);

      // Use for loop to read the content of the stringList
      for line in stringList do
        WriteLn(line);

    finally
      // Free object from memory
      stringList.Free;
    end;

  except
    on E: Exception do
      WriteLn('File handling error occurred. Details: ', E.Message);
  end; // end of try..except

  // Pause console
  WriteLn('--------------------');
  WriteLn('Press Enter key to quit.');
  ReadLn;

end.
```

## Counting lines in a text file 

### Counting lines - `TBufferedFileStream`

The snippet below was adapted from [Faster filestream with TBufferedFilestream](https://delphiaball.co.uk/2016/04/29/faster-filestream-tbufferedfilestream/) by Stephen Ball.

The structure is similar to [reading a text file using TFileStream](#read-a-text-file-tfilestream), but here, we use [`TBufferedFileStream`](https://www.freepascal.org/docs-html/fcl/bufstream/tbufferedfilestream.html).

1. In the `uses` section, add `bufstream`. Line 11.
2. Create a `TBufferedFileStream` to open a text file for reading. Line 33.
3. Use the `while fStream.Read(ch, 1) = 1` to keep on reading data until there is no more data to read. Line 35-39.
4. `Free` resources when done. Line 41.

```pascal linenums="1" hl_lines="11 33 35-39 41"
program TBufferedFileStreamCount;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  bufstream;

var
  fStream: TBufferedFileStream;
  total: int64;
  ch: char;

begin

  // Do we have a valid input file?
  if Length(ParamStr(1)) < 3 then
  begin
    WriteLn('Please specify a valid input file.');
    Exit;
  end;

  // Reset total
  total := 0;

  // try - except block start
  try

    fStream := TBufferedFileStream.Create(ParamStr(1), fmOpenRead);
    try
      while fStream.Read(ch, 1) = 1 do
      begin
        if ch = #10 then
          Inc(total);
      end;
    finally
      fStream.Free;
    end;

    // User feedback
    WriteLn('Total line is: ', IntToStr(total));

  except
    on E: Exception do
      WriteLn('Error: ' + E.Message);

  end; // try - except block ends

end.
```