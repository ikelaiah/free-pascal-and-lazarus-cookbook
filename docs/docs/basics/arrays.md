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

### Examples of static arrays

```pascal linenums="1"
program StaticArrayDemo01;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes;

type
  // Declaring a static array in the type-section.
  TByteArray = array [1..5] of byte; // size is 5

var
  // Creating an array var based on a type.
  // You can assign initial values here too.
  studentGrades: TByteArray;

  // Declaring an array type in the var section.
  multipleTen: array [0..9] of integer; // size is 10

  // Declaring static array and init values in the var-section.
  osChoices: array [1..3] of string = ('Linux', 'MacOS', 'Windows');

  index: integer; // a var for loops

begin
  // Assign a value to an array's element by using a valid index value
  // enclosed in square brackets.
  // Populate student grades
  studentGrades[1] := 95;
  studentGrades[2] := 85;
  studentGrades[3] := 75;
  studentGrades[4] := 55;
  studentGrades[5] := 85;

  // Populate multiple ten
  for index := low(multipleTen) to high(multipleTen) do
    multipleTen[index] := index * 10;

  // Print the length of the arrays
  WriteLn('The length of grades array     : ', Length(studentGrades));
  WriteLn('The length of osChoices array  : ', Length(osChoices));
  WriteLn('The length of multipleTen array: ', Length(multipleTen));

  WriteLn('-------------------');

  // Print an element from each array
  WriteLn('Grade of student 3 in the array : ', studentGrades[3]);
  WriteLn('First choice of OS the array    : ', osChoices[1]);
  WriteLn('The Last multiple of 10 in array: ', high(multipleTen));

  WriteLn('-------------------');

  // Print all elements from each array
  WriteLn('-- Student grades array');
  for index := low(studentGrades) to high(studentGrades) do
    WriteLn('Student ', index, ' scored ', studentGrades[index]);

  WriteLn('-- Multiple of ten array');
  for index := low(multipleTen) to high(multipleTen) do
    WriteLn('Index  ', index, ' contains ', multipleTen[index]);

  WriteLn('-- OS choices array');
  for index := low(osChoices) to high(osChoices) do
    WriteLn('OS choice no ', index, ' is ', osChoices[index]);

  // Pause console
  WriteLn('-------------------');
  WriteLn('Press enter to quit');
  ReadLn;
end.
```

The output will be as follows.

```bash
The length of grades array     : 5
The length of osChoices array  : 3
The length of multipleTen array: 10
-------------------
Grade of student 3 in the array : 75
First choice of OS the array    : Linux
The Last multiple of 10 in array: 9
-------------------
-- Student grades array
Student 1 scored 95
Student 2 scored 85
Student 3 scored 75
Student 4 scored 55
Student 5 scored 85
-- Multiple of ten array
Index  0 contains 0
Index  1 contains 10
Index  2 contains 20
Index  3 contains 30
Index  4 contains 40
Index  5 contains 50
Index  6 contains 60
Index  7 contains 70
Index  8 contains 80
Index  9 contains 90
-- OS choices array
OS choice no 1 is Linux
OS choice no 2 is MacOS
OS choice no 3 is Windows
-------------------
Press enter to quit
```

!!! Important

    The functions High and Low return the high and low bounds of the leftmost index type of the array. 
    
    You should use them whenever possible, since it improves maintainability of your code. The use of both functions is just as efficient as using constants, because they are evaluated at compile time.

    Source: [https://www.freepascal.org/docs-html/ref/refsu14.html](https://www.freepascal.org/docs-html/ref/refsu14.html)

## What is a dynamic array?

In Free Pascal, a dynamic array is a ==data structure== that allows for ==flexible sizing of arrays at runtime==. It is defined as an array whose size can be adjusted dynamically during program execution. Dynamic arrays are allocated on the heap, allowing them to grow or shrink as needed, unlike static arrays whose size is fixed at compile time.

!!! Important

    Dynamic arrays' indices are always non-negative integers starting at zero for the first element. It is not possible to use an enumerative type or any other ordinal type as an index. The first element is always specified by an index of 0 – this cannot be changed.

Source: [https://wiki.freepascal.org/Dynamic_array](https://wiki.freepascal.org/Dynamic_array)

Before using a dynamic array, you must set the length at runtime using [`SetLength`](https://www.freepascal.org/docs-html/rtl/system/setlength.html) procedure.

### Example of dynamic arrays

```pascal linenums="1"
program DynArrayDemo01;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Classes,
  SysUtils;

type
  // Declaring a static array in the type-section.
  TRealArray = array of real;

var
  // Creating an array var based on a type.
  // You can assign initial values here too.
  dailyTemp: TRealArray;

  // Declaring an array type in the var section.
  multipleTwo: array of integer;

  // Declaring an array type in the var section.
  defenceForces: array of string = ('Navy', 'Army', 'Air Force');

  tempInt, index: integer;      // variables for loops.
  tempReal: real;               // variable for loops.
  tempStr: string;              // a string placeholder for loops.

begin

  // Setting length of dynamic arrays
  SetLength(dailyTemp, 7);   // This array's size is 7, index 0..6
  SetLength(multipleTwo, 5);  // This array's size is 8, index 0..7

  // Populate the daily temp array
  dailyTemp[0] := 30.1;
  dailyTemp[1] := 25.5;
  dailyTemp[2] := 28.7;
  dailyTemp[3] := 29.1;
  dailyTemp[4] := 28.8;
  dailyTemp[5] := 28.5;
  dailyTemp[6] := 27.2;

  // Populate the int array with multiples of two
  for index := low(multipleTwo) to high(multipleTwo) do
    multipleTwo[index] := index * 2;

  // Print the length of the arrays
  WriteLn('The length of dailyTemp array    : ', Length(dailyTemp));
  WriteLn('The length of multipleTwo array  : ', Length(multipleTwo));
  WriteLn('The length of defenceForces array: ', Length(defenceForces));

  WriteLn('-------------------');

  // Print an element from each array
  WriteLn('Last temp recorded in the array           : ', high(dailyTemp));
  WriteLn('First number in multipleTwo array         : ', low(multipleTwo));
  WriteLn('The second item in the defenceForces array: ', defenceForces[1]);

  WriteLn('-------------------');

  WriteLn('-- Printing the real array');

  // Print the real array
  for index := 0 to high(dailyTemp) do
  begin
    // Option 1
    // WriteLn('Temp day ', (index + 1), ' is ', dailyTemp[index]:0:2);
    // Option 2
    WriteLn(Format('The temp for day %d is %2f.', [index, dailyTemp[index]]));
  end;

  WriteLn('-- Printing the integer array');

  // Print the integer array
  for tempInt in multipleTwo do
    WriteLn(tempInt);

  WriteLn('-- Printing the string array');

  // Print the string array
  for tempStr in defenceForces do
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