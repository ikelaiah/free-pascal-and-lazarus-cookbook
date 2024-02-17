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

## Check if a directory exists

Use `FileExists` from unit `SysUtils`.

```Pascal
program CheckDirExists;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

begin

  if (DirectoryExists('sub-folder/')) then
    WriteLn('That folder exists!')
  else
    WriteLn('Can''t find it!');

  // Pause console
  ReadLn;

end.
```

## Check if a file exists

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

## Find files recursively (using LazUtils package)

You can use [FindAllFiles](https://lazarus-ccr.sourceforge.io/docs/lazutils/fileutil/findallfiles.html) from `FileUtil` unit.

To use this unit, you must add `LazUtils` package from the `Project Inspector -> Required Packages`.

![Add LazUtils in Project inspector](../../assets/lazutils-in-inspector.png)

Here is an example. This program looks for `csv` and `xslx` files in a sub-folder.

1. Add `FileUtil` in the unit section.
2. Invoke the `FindAllFiles` and save the output into a `TStringList` variable. You don't need to instantiate the `TStringList` object separately; `FindAllFiles` handles it automatically. When calling this function, make sure to provide the following:

      - The path to be searched.
      - The types of files to be searched.
      - Specify whether the search should be recursive.
  
3. Lastly, free the `TStringList`.

```pascal linenums="1" hl_lines="10 23 34"
program ListAllFiles;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  FileUtil, // Add LazUtils in Project Inspector -> Required Packages first
  SysUtils;

var
  searchResults: TStringList;
  path: string = './sub-folder/';
  criteria: string = '*.csv;*.xlsx';
  isRecursive: boolean = True;
  item: string;

begin

  // Call FindAllFiles, no need to create TStringList manually
  searchResults := FindAllFiles(path, criteria, isRecursive);
  try
    // Print number of files found
    WriteLn(Format('Found %d files', [searchResults.Count]));

    // Display results, if any
    if searchResults.Count > 0 then
      for item in searchResults do WriteLn(item);

  finally
    // Free the TStringList
    searchResults.Free;
  end;

  // Pause console
  WriteLn;
  WriteLn('Press Enter key to exit ...');
  ReadLn;
end.
```

**References**

- [https://lazarus-ccr.sourceforge.io/docs/lazutils/fileutil/findallfiles.html](https://lazarus-ccr.sourceforge.io/docs/lazutils/fileutil/findallfiles.html)
- [https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-find-all-files-in-a-directory-and-subdirectories-matching-criteria/](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-find-all-files-in-a-directory-and-subdirectories-matching-criteria/)