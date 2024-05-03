# Arrays in Free Pascal

## Official docs

- [https://www.freepascal.org/docs-html/ref/refsu14.html](https://www.freepascal.org/docs-html/ref/refsu14.html)
- [https://wiki.freepascal.org/Array](https://wiki.freepascal.org/Array)

## What is an array?

> An array is a ==linear data structure concept== that ==groups elements of the same type==, stores them in ==contiguous and adjacent memory locations== and provides ==random access== to all of said elements by way of a linear index.
> 
> Each element can be uniquely identified by one or more scalar values, called indices, along those dimensions.
> 
> Quoted from [Array | Free Pascal Wiki](https://wiki.freepascal.org/Array).

## What is a static array?

A static array is an array with its range included in the array declaration. In other words, a static array has a size known or decided in advance or at compile time.

Since the size is decided on compile, you cannot change the size of the array. Hence, static array.

### Declaring a static array

You can declare a static array type in the `type` section.

```pascal linenums="1"
type
  TArray = array [ordinal..ordinal] of aType;

var
  anArray:TArray;
```

Alternatively, you can declare your static array variable in the `var` section.

```pascal linenums="1"
var
  anArray : array [ordinal..ordinal] of aType;
```

Which approach should you use? See [Should I declare arrays in the `type` or in the `var` section?](#do-i-declare-arrays-in-the-type-or-in-the-var-section)

### Declaring and initialising a static array

You can declare and init static array variable in the `var` section.

```pascal linenums="1"
var
  anArray : array [ordinal..ordinal] of aType = (val_lowest_ordinal, ... , val_highest_ordinal);
```

### Accessing an element in a static array

```pascal linenums="1"
// Accessing an element
anArray[valid_index];

// Assigning value to an element
anArray[valid_index] := value;
```

### Printing the content of a static array var

For simplicity, use either  `for..to..do` or `for..in..do` loop.

```pascal linenums="1"
// Option 1 - for..to..do loop
for index := low(anArray) to high(anArray) do
  WriteLn(anArray[index]);

// Option 2 - for..in..do loop
for tempVar in anArray do
  WriteLn(tempvar);
```

!!! Important

    The functions High and Low return the high and low bounds of the leftmost index type of the array. 
    
    You should use them whenever possible, since it improves maintainability of your code. The use of both functions is just as efficient as using constants, because they are evaluated at compile time.

    Source: [https://www.freepascal.org/docs-html/ref/refsu14.html](https://www.freepascal.org/docs-html/ref/refsu14.html)

### Example 01 - declare a type of a static array and create a var with that type

```pascal linenums="1"
program StaticArrayDemo01;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes;

type
  // Example: storing student grades.
  TStudentGrades = array [1..5] of integer; // size is 5

var
  grades: TStudentGrades;
  index: integer;   // An index variable for loops.

begin
  // Assign a value to an array's element by using a valid index value
  // enclosed in square brackets.
  // Populate student grades
  grades[1] := 95;
  grades[2] := 85;
  grades[3] := 75;
  grades[4] := 55;
  grades[5] := 85;

  // Print an element
  WriteLn('Grade of student 3 is ', grades[3]);

  WriteLn('-- Student grades array');
  // Print an array of student grades
  for index := low(grades) to high(grades) do
    WriteLn('Student ', index, ' scored ', grades[index]);

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

### Example 02 - declare types of static array vars and init values

```pascal linenums="1"
program StaticArrayDemo02;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes;

type
  TSmallStrArray = array[0..2] of string; // Size is 3

var
  // Create an array var based on a static array type
  osList: TSmallStrArray = ('Linux', 'MacOS', 'Windows');

  // Declare a static array and init values
  browsers: array[0..5] of string = ('Chrome', 'Safari', 'Edge',
                                     'Firefox', 'Opera', 'Vivaldi');

  // An index variable for loops.
  index: integer;

begin

  // Print an element from an array
  WriteLn('First choice of OS is ', osList[0]);
  WriteLn('First choice of browser is ', browsers[0]);

  WriteLn('-- array of operating systems');

  // Print an array of operating systems
  for index := low(osList) to high(osList) do
    WriteLn(osList[index]);

  WriteLn('-- array of browser names');

  // Print an array of operating systems
  for index := low(browsers) to high(browsers) do
    WriteLn(browsers[index]);

  // Pause console
  WriteLn('Press enter key to quit');
  ReadLn;
end.
```

## What is a dynamic array?

In Free Pascal, a dynamic array is a ==data structure== that allows for ==flexible sizing of arrays at runtime==. It is defined as an array whose size can be adjusted dynamically during program execution. Dynamic arrays are allocated on the heap, allowing them to grow or shrink as needed, unlike static arrays whose size is fixed at compile time.

!!! Important

    Dynamic arrays' indices are always non-negative integers starting at zero for the first element. It is not possible to use an enumerative type or any other ordinal type as an index. The first element is always specified by an index of 0 – this cannot be changed.

Source: [https://wiki.freepascal.org/Dynamic_array](https://wiki.freepascal.org/Dynamic_array)

### Declaring a dynamic array

You can declare a dynamic array type in the `type` section.

```pascal linenums="1"
type
  TDynArray = array of aType;

var
  aDynArray:TDynArray;
```

Alternatively, you can declare your static array variable in the `var` section.

```pascal linenums="1"
var
  aDynArray : array of aType;
```

Which approach should you use? See [Should I declare arrays in the `type` or in the `var` section?](#do-i-declare-arrays-in-the-type-or-in-the-var-section)

### Declaring and initialising a dynamic array

You can declare and init a dynamic array variable in the `var` section.

```pascal linenums="1"
var
  aDynArray : array of aType = (value_1, value_2, ... , val_n);
```

### Setting length of a dynamic array

Use [`SetLength`](https://www.freepascal.org/docs-html/rtl/system/setlength.html) to set the length of a dynamic array at runtime.

```pascal linenums="1"
SetLength(aDynArray, newLength);
```

### Accessing an element in a dynamic array

Similar to the static array's one, use a valid index value in enclosed in square brackets.

```pascal linenums="1"
// Accessing an element
aDynArray[valid_index];

// Assigning value to an element
aDynArray[valid_index] := value;
```

### Printing the content of a dynamic array var

For simplicity, use either  `for..to..do` or `for..in..do` loop.

```pascal linenums="1"
// Option 1 - for..to..do loop
for index := low(aDynArray) to high(aDynArray) do
  WriteLn(aDynArray[index]);

// Option 2 - for..in..do loop
for tempVar in aDynArray do
  WriteLn(tempvar);
```

### Example 01 - declare dynamic array types and create vars with the types

```pascal linenums="1"
program DynArrayDemo01;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes;

type
  TIntArray = array of integer;
  TStringArray = array of string;

var
  multipleTen: TIntArray;       // size is unknown at compile time
  defenceForces: TStringArray;  // size is unknown at compile time
  tempInt, index: integer;      // variables for loops.
  tempStr: string;              // a string placeholder for loops.

begin

  // Setting length of dynamic arrays
  SetLength(multipleTen, 10);   // This array's size is 10, index 0..9
  SetLength(defenceForces, 3);  // This array's size is 2, index 0..1

  // Populate the int array with multiples of 10
  for index := low(multipleTen) to high(multipleTen) do
    multipleTen[index] := index * 10;

  // Populate the string array with strings
  defenceForces[0] := 'Navy';
  defenceForces[1] := 'Army';
  defenceForces[2] := 'Air Force';

  WriteLn('-- Printing the integer array');

  // Print the integer array
  for tempInt in multipleTen do
    WriteLn(tempInt);

  WriteLn('-- Printing the string array');

  // Print the string array
  for tempStr in defenceForces do
    WriteLn(tempStr);

  // Pause console
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

### Example 02 - declare and init values of dynamic arrays

```pascal linenums="1"
program DynArrayDemo02;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes;

type
  TStringArray = array of string;

var
  // Create an array var based on a dynamic array type
  shoppingList: TStringArray = ('Corn Flakes', 'Eggs', 'Tea', 'Milk',
                                'Cheese', 'Tzatziki', 'Sausages', 'Olives',
                                'Bread', 'Garlic');

  // Declare a dynamic array and initialise values
  tofuList: array of string = ('Kinu-dofu', 'Momen-dofu',
                               'Iburi-dofu', 'Yuba');

  tempStr: string; // a string placeholder for loops.

begin

  WriteLn('-- array of shopping items');

  // Print the string array
  for tempStr in shoppingList do
    WriteLn(tempStr);

  WriteLn('-- array of japanese tofu names');

  // Print the string array
  for tempStr in tofuList do
    WriteLn(tempStr);

  // Pause console
  WriteLn('-------------------');
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

## Should I declare arrays in the `type` or in the `var` section?

While it is possible to declare arrays in the `var` section for simplicity or quick one-time use, leveraging the `type` section for array declarations offers benefits in terms of Readability ==code structure==, ==maintainability==, and ==readability== in larger and more complex Pascal programs.

### Pros of declaring in `var` section

- **Simplicity and Convenience**: Declaring dynamic arrays directly in the var section can be more convenient for quick one-time use or when you need a simple array without the need for a custom type.

- **Less Overhead**: For smaller programs or cases where the array is used only within a limited scope, declaring dynamic arrays in the var section can reduce the overhead of creating custom types.

### Pros of declaring in the `type` section

- **Code Readability**: Placing dynamic array declarations in the type section enhances code readability by clearly defining custom array types for reuse throughout the program.

- **Reusability**: Defining dynamic array types in the type section promotes code reusability and consistency, allowing you to use the same array type in multiple parts of your program.

- **Type Safety**: By declaring dynamic array types in the type section, you enforce type safety and ensure that variables declared using these types are checked for compatibility at compile time.

- **Encapsulation**: Encapsulating dynamic array definitions within named types offers a level of abstraction, hiding implementation details and preventing unintended modifications to the array structure.

### Recommendations

- **For larger programs**: In larger programs or when working with complex data structures, it is beneficial to declare dynamic arrays in the `type` section to promote code organization, maintainability, and consistency.

- **For quick prototyping or small programs**: In smaller programs or for quick prototyping where simplicity is priority, declaring dynamic arrays in the `var` section may be more practical.