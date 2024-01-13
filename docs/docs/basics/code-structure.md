# Structuring Your Codes

## How do I Structure of a Pascal Program?

Here is a common structure of a Pascal program.

```pascal linenums="1"
program ProgramStructure;

{ Global compiler directives at the top. }

uses
  { Add necesasry units here. }

const
  { Declare consts.  }

resourcestring
  { Declare resourcestrings. }

type
  { Declare types. }

var
  { Declare variables. }

threadvar
  { Declare threadvars.
    Variables in this section have unique values for each thread} 

{ Define procedures and functions before the MAIN entry of the program. }

begin
   { This is the MAIN entry of your program. } 
end.             
```

## An example of a Pascal Program

Here is a program example that stores student information in a record, and print it on the console.

```pascal linenums="1"
program SimpleProgram;

{$mode objFPC}{$H+}{$J-}

uses
  SysUtils;

const
  student_id_prefix: string = 'ua-';

type
  TStudent = record
    studentId: string;
    firstname: string;
    lastname: string;
  end;

procedure PrintStudentInfo(student: TStudent);
begin
  WriteLn(student.studentId);
  WriteLn(student.firstname, ' ', student.lastname);
end;

var
  myStudent: TStudent;

begin
  WriteLn('Now : ', DateToStr(Now));

  myStudent.firstname := 'John';
  myStudent.lastname := 'Costco';
  myStudent.studentId := student_id_prefix + '2227209';
  PrintStudentInfo(myStudent);

  WriteLn('Press Enter key to quit ...');
  readln();
end.
```

The output as follows.

```text
Now : 16/12/2023
ua-2227209
John Costco
Press Enter key to quit ...
```

## How do I Structure a Unit?

```pascal linenums="1"
unit AnEmptyUnit;

interface

  { This is the Public section. 
    Variables, functions and procedures declared in this section 
    will be accessible from the unit's caller. }

implementation

  { This is the Private section.
    Anything declared in this section will only be available to the unit. }

initialization

	{ Optional. Code that runs when the unit gets loaded. }

finalization

	{ Optional. Code that runs when the program ends normally.
      The finalization part of the units are executed in the 
      reverse order of the initialization execution. 
      - Reference Guide, 16.2 (Official Doc). }

end.
```

## An Example of a Unit

Here is an example of a simple unit for calculating the areas of a square and a circle.

1. the unit has a private variable called `short_pi`, which is not available outside the unit itself.
2. the unit has two public functions.

```pascal linenums="1"
unit Areas;

interface

function CalcAreaSquare(side: real): real;
function CalcAreaCircle(radius: real): real;

implementation

const
  shortPI: real = 3.14;

function CalcAreaSquare(side: real): real;
begin
  Result := side * side;
end;

function CalcAreaCircle(radius: real): real;
begin
  Result :=  shortPI * radius * radius;
end;

end.
```

We can use this unit as follows.

```pascal linenums="1"
program TestUnit;

{$mode ObjFPC}{$H+}{$J-}

uses
  { Call your unit in the `uses` section. }
  Areas;

begin
  { Calculate area of a square. }
  WriteLn('Area of 2.5cm square is ', Areas.CalcAreaSquare(2.5): 0: 2, ' cm².');

  { Calculate area of a circle. }
  WriteLn('Area of a circle with r=2.5cm is ', Areas.CalcAreaCircle(2.5): 0: 2, ' cm².');

  { The following line will not compile }
  // WriteLn('shortPI is ', Areas.shortPI);

  WriteLn('Press Enter key to exit ...');
  readln;
end.
```