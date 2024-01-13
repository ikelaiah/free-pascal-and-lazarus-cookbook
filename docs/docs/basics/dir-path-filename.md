# Directories, Path and Filename

## Create a directory or chain of directories

Here is a snippet of creating a sub directory called `demo/ex-01` in the program's current directory.

```pascal linenums="1"
program DirPathFileCreateDir;

{$mode objfpc}{$H+}

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

{$mode objfpc}{$H+}

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