# Directories, Path and Filename

## Create a directory or chain of directories

Here is a snippet of creating a sub directory called `demo/ex-01` in the program's current directory.

```pascal linenums="1"
program DirPathFileCreateDir;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils;

var
  directoryName: string;
begin
  directoryName := ConcatPaths(['demo','ex-01']);
  if ForceDirectories(directoryName) then
    writeln('Directory created successfully')
  else
    writeln('Failed to create directory');
end.
```

## Create a directory or chain of directories using UTF8

You can use `ForceDirectories` to create directories using UTF8.

Here is a snippet of creating a sub directory called `demo/胜利` in the program's current directory.

```pascal linenums="1"
program DirPathFileCreateDirUTF8;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils;

var
  directoryName: string;
begin
  directoryName := ConcatPaths(['demo','胜利']);
  if ForceDirectories(directoryName) then
    writeln('Directory created successfully')
  else
    writeln('Failed to create directory');
end.
```

## Checking if a file exists

Use `FileExists` from unit `SysUtils`.

```pascal linenums="1" hl_lines="4 12"
program CheckFileExists;

uses
  SysUtils;

var
  fileName: String;

begin
  fileName := 'hello-world.txt';

  if FileExists(fileName) then
    Writeln( fileName, ' exists.')
  else
    Writeln(fileName, ' does not exist.');
end.
```