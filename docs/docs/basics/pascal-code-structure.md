# Structuring Your Codes

## Structure of a Pascal Program

```pascal
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

```pascal
program simple_program;

{$mode objFPC}{$H+}{$J-}

uses
  SysUtils;

const
  student_id_prefix: string = 'ua-';

type
  TStudent = record
    student_id: string;
    first_name: string;
    last_name: string;
  end;

procedure print_student_info(student: TStudent);
begin
  writeln(student.student_id);
  writeln(student.first_name, ' ', student.last_name);
end;

var
  my_student: TStudent;

begin
  writeln('Now : ', DateToStr(Now));

  my_student.first_name := 'John';
  my_student.last_name := 'Costco';
  my_student.student_id := student_id_prefix + '2227209';
  print_student_info(my_student);

  writeln('Press Enter key to quit ...');
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

## Structure of a Unit

```pascal
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

```pascal
unit areas;

interface

function calc_area_square(side: real): real;
function calc_area_circle(radius: real): real;

implementation

const
  short_pi: real = 3.14;

function calc_area_square(side: real): real;
begin
  Result := side * side;
end;

function calc_area_circle(radius: real): real;
begin
  Result :=  short_pi * radius * radius;
end;

end.
```

We can use this unit as follows.

```pascal
program test_unit;

{$mode ObjFPC}{$H+}{$J-}

uses
  { Call your unit in the `uses` section. }
  areas;

begin
  { Calculate area of a square. }
  writeln('Area of 2.5cm square is ', areas.calc_area_square(2.5): 0: 2, ' cm².');

  { Calculate area of a circle. }
  writeln('Area of a circle with r=2.5cm is ', areas.calc_area_circle(2.5): 0: 2, ' cm².');

  { The following line will not compile }
  // writeln('short_pi is ', areas.short_pi);

  writeln('Press Enter key to exit ...');
  readln;
end.
```