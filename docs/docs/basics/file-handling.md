# File Handling

## How do I write strings into a new text file?

There are many ways to do this; 

1. classic style (using the [File Handling Functions](https://www.freepascal.org/docs-html/rtl/system/filefunctions.html)) and 
2. object style, including:

      - [`TStringList`](https://www.freepascal.org/docs-html/rtl/classes/tstringlist.html) and
      - [`TFileStream`](https://www.freepascal.org/docs-html/rtl/classes/tfilestream.html).

### New text file - Classic

See the snippet below. It uses `SysUtil` for catching errors during opening and writing file.

1. Use `AssignFile` to assign a text file to a file type `TextFile`. Line 14.
2. Use `Rewrite` to open file for writing (and create if doesn't exists). Line 19.
3. Add text using the assigned file type and `WriteLn`. Line 22.
4. Close the file with `CloseFile` in the `try..finally` block. Line 31.

> You should always strive to have a `try..finally` outside the `try..except`.
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
          writeln('Error occurred. Details: ', E.ClassName, '/', E.Message);
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


### New text file - Classic in a procedure

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
          writeln('Error occurred. Details: ', E.ClassName, '/', E.Message);
      end;
    finally
      // Close file
      CloseFile(textFile);
    end;
  end;

begin

  // Write a text to a file
  WriteTextToFile('hello-text.txt', 'Hello There! How are you?');

  // Wait for enter
  readln;
end.

```


### New text file - TFileStream



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

### New text file - TFileStream in a procedure

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


## How can I create a new blank text file?

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

### Blank text file - TFileStream

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

## How to read a text file line by line?

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

### Make the code more readable

The nested `try..free` blocks in the `try..except` might be difficult to read. But the code works, right?

Here is an alternative implementation.

- inner `try..free` blocks are now in separate procedures.
- the outer `try..except` can now catch exceptions from procedures.

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