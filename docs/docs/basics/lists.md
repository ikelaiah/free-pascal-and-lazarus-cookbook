# Building and Processing Lists

## How can I make a list of string and sort it?

Use [`TStringList`](https://lazarus-ccr.sourceforge.io/docs/rtl/classes/tstringlist.html). See the recipe below.

```pascal
program StringList;

{$mode objfpc}{$H+}

uses
  Classes;

var
  myString: TStringList;
  i:Integer;

begin
  // Allocate a memory for this list
  myString := TStringList.Create;
  myString.Add('c');
  myString.Add('a');
  myString.Add('b');

  // You can sort
  myString.Sort;

  // Iterate through the list
  for i := 0 to myString.Count - 1 do
    Writeln(myString[i]);

  // Free the memory used by the list
  myString.Free;

  // Pause Console
  Readln;
end.
```

## Is there a `TIntegerList`? 

No, there is no built-in type `TIntegerList`.

## How do I make a list of integer? 

Make your own type. You can use [`Generics.Collections.TList`](https://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate1/EN/pdf/genericcollections.pdf) to create `TIntegerList`.

```pascal
program SimpleIntegerList;

{$mode objfpc}{$H+}

uses
  Generics.Defaults,
  Generics.Collections;

type
  TIntegerList = specialize TList<Integer>;

var
  myIntList: TIntegerList;
  i: Integer;

begin
  // Create a new generic list
  myIntList := TIntegerList.Create;

  // Add some elements to the list
  myIntList.Add(3);
  myIntList.Add(1);
  myIntList.Add(2);

  // Iterate through the list
  for i := 0 to myIntList.Count - 1 do
    Writeln(myIntList[i]);

  // Free the memory used by the list
  myIntList.Free;

  Readln;
end.
```

## Can I sort a  `TList<Integer>` list?

Absolutely!

First, add the following a comparer type and its function in the code.


```pascal hl_lines="4-14"
type
  TIntegerList = specialize TList<Integer>;
  
  TIntegerListComparer = specialize TComparer<Integer>;
  
  // Custom comparison function for sorting
  function CompareInteger(constref Item1, Item2: Integer): Integer;
  begin
    Result := CompareValue(Item1, Item2);
  end;
```

Next, use the `Generics.Collections.TList.Sort` in the main block.

```pascal
MyIntList.Sort(TIntegerListComparer.construct(@CompareItems));
```

Here is a complete example.

```pascal
program SortIntegerList;

{$mode objfpc}{$H+}

uses
  Generics.Defaults,
  Generics.Collections,
  Math;

type
  TIntegerList = specialize TList<integer>;
  TIntegerListComparer = specialize TComparer<integer>;

  // Custom comparison function for sorting
  function CompareItems(constref Item1, Item2: integer): integer;
  begin
    Result := CompareValue(Item1, Item2);
  end;

var
  myIntList: TIntegerList;
  i: integer;

begin
  // Create a new generic list
  myIntList := TIntegerList.Create;

  // Add some elements to the list
  myIntList.Add(3);
  myIntList.Add(1);
  myIntList.Add(2);

  // Sort
  myIntList.Sort(TIntegerListComparer.construct(@CompareItems));

  // Iterate through the list
  for i := 0 to myIntList.Count - 1 do
    Writeln(myIntList[i]);

  // Free the memory used by the list
  myIntList.Free;

  Readln;
end.
```


## How do you append two `TList<Integer>` lists?

You can use `Generics.Collections.TList.AddRange`.

```pascal hl_lines="28"
program AppendIntegerList;

{$mode objfpc}{$H+}

uses
  Generics.Defaults,
  Generics.Collections;

type
  TIntegerList = specialize TList<Integer>;

var
  myIntList1, myIntList2: TIntegerList;
  i: Integer;

begin
  // Create a new generic list
  myIntList1 := TIntegerList.Create;
  myIntList2 := TIntegerList.Create;

  // Add some elements to the list
  myIntList1.Add(4);
  myIntList1.Add(1);
  myIntList2.Add(3);
  myIntList2.Add(2);

  // Append myIntList2 at the back of myIntList1
  myIntList1.AddRange(myIntList2);

  // Iterate through the list
  for i := 0 to myIntList1.Count - 1 do
    Writeln(myIntList1[i]);

  // Free the memory used by the list
  myIntList2.Free;
  myIntList1.Free;

  Readln;
end.
```

## When building an integer list (or others), which unit is preferable to use: `fgl` or `Generics.Collection`?

Consider the following answer from PascalDragon, March 22, 2020, 12:55 pm.

> Just to clear these up as well:
> 
> - `fgl` is a unit of generic types distributed with FPC that is smaller than Generics.Collections and has some restrictions and the performance might be worse; however it can be more easily be used where size of the binary is a concern
> - `Generics.Collections` is the name of the Delphi-compatible generic collection types (list, dictionary, etc.) which is rather powerful and performant; this is part of FPC 3.2.0 and newer ...
> 
> Source: https://forum.lazarus.freepascal.org/index.php?topic=48988.0


## So, how do I create an integer list using `fgl`?

See the recipe below.

```pascal
program FGLIntegerList;

{$mode objfpc}{$H+}

uses
  fgl, Math;

type
  TIntegerList = specialize TFPGList<integer>;

function CompareItems(const item1, item2: Integer): Integer;
begin
  Result := CompareValue(item1, item2);
end;

var
  myIntList: TIntegerList;
  i: integer;

begin
  myIntList := TIntegerList.Create;

  // Adding integers to the list
  myIntList.Add(444);
  myIntList.Add(222);
  myIntList.Add(333);

  // Sorting the list
  myIntList.Sort(@CompareItems);

  // Printing list
  for i := 0 to myIntList.Count - 1 do
  begin
    WriteLn(myIntList[i]);
  end;

  // Freeing the list when done
  myIntList.Free;

  ReadLn;
end.
```