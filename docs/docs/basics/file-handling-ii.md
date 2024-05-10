# File Handling II

More snippets on handling files using Free Pascal.

## Listing specific columns in a CSV file - TCSVDataSet

Here is an example of reading first two columns in a CSV file using `TCSVDataset`.

```pascal
program TCSVDatasetGetSpecificCols;

{
 An example of listing the content of first two columns in a CSV file
 using TCSVDataset.
}

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  streamex,
  bufstream,
  csvdataset;

  // A routine to list first two columns of a CSV file
  procedure ReadCSV(filename: string;
                    delimiter: char = ',';
                    isFirstRowFieldName: boolean = False);
  var
    fileStream: TFileStream;
    buffStream: TReadBufStream;
    csvDataset: TCSVDataset;
    lineNo: int64;
  begin
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      buffStream := TReadBufStream.Create(fileStream, 65536);
      try
        csvDataset := TCSVDataset.Create(nil);
        try

          // Assign a valid delimiter
          csvDataset.CSVOptions.Delimiter := delimiter;

          // Is the first line field names?
          // If yes, first row will be excluded when listing rows
          csvDataset.CSVOptions.FirstLineAsFieldNames := isFirstRowFieldName;

          // Load CSV from the stream
          csvDataset.LoadFromCSVStream(buffStream);

          // Move to first record
          csvDataset.First;

          lineNo := 1;

          while not csvDataset.EOF do
          begin
            // Get the values of the first two fields here and list them.
            WriteLn(Format('row %d: %s, %s',
                           [lineNo,
                            csvDataset.Fields[0].AsString,
                            csvDataset.Fields[1].AsString]));

            // Move to next
            csvDataset.Next;

            // Increment line no
            lineNo := lineNo + 1;
          end;

        finally
          csvDataset.Free;
        end;
      finally
        buffStream.Free;
      end;
    finally
    end;
    fileStream.Free;
  end;

var
  filename: string;

begin

  filename := ParamStr(1);
  if not FileExists(filename) then
  begin
    WriteLn('Cannot find file.');
    Exit;
  end;

  ReadCSV(filename, ';', False);
end.
```

## Listing specific columns in a CSV file - TCSVDocument

Here is an example of reading first two columns in a CSV file using `TCSVDocument`.

```pascal
program TCSVDocumentGetSpecificCols;


{
 An example of listing the content of first two columns in a CSV file
 using TCSVDocument.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  csvdocument,
  streamex,
  bufstream;

  procedure ReadCSV(filename: string; delimiter: char);
  var
    fileStream: TFileStream;
    buffStream: TReadBufStream;
    csvReader: TCSVDocument;
    index, totalLines: int64;
  begin
    totalLines := 0;
    fileStream := TFileStream.Create(filename, fmOpenRead);
    try
      buffStream := TReadBufStream.Create(fileStream, 65536);
      try
        csvReader := TCSVDocument.Create;
        try
          // Assign a delimiter
          csvReader.Delimiter := delimiter;

          // Assign a source stream.
          csvReader.LoadFromStream(buffStream, 65536);

          // Get total lines for iteration.
          totalLines := csvReader.RowCount;

          // Print the values of first two columns from the CSV file.
          for index := 0 to totalLines-1 do
          begin
            WriteLn(Format('row %d: %s, %s', [(index + 1),
                                              csvReader.Cells[0, index],
                                              csvReader.Cells[1, index]]));
          end;

        finally
          csvReader.Free;
        end;
      finally
        buffStream.Free;
      end;
    finally
    end;
    fileStream.Free;
  end;

var
  filename: string;

begin
  filename := ParamStr(1);
  if not FileExists(filename) then
  begin
    WriteLn('Cannot find file.');
    Exit;
  end;

  ReadCSV(filename, ';');
end.
```