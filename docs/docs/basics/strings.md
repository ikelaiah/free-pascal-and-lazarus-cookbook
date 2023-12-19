# Strings

## What is a `String`?

A `String` in FPC is an alias for;

- `ShortString` (fixed 255 length),
- `AnsiString` (variable length) or
- `Unicodestring` (UTF16) depending on a compiler setting.

When `{$H+}` is not specified, or `{$H-}`, `String` is an alias for `ShortString`.

Any `ShortString` have a maximum length of 255 characters with the implicit codepage `CP_ACP`. Short strings are always assumed to use the system code page.

When `{$H+}` is not specified, or `{$H-}`, `String` is an alias for `ShortString`.

When `{$H+}` is specified, `String` is an alias for `AnsiString`.

Any `String` is essentially an `AnsiString` with the `DefaultSystemCodePage` declared in it; `AnsiString(CP_ACP)`. And if the default system code page is `65001`, then any `String` is `UTF-8`. 

Commonly on Windows, the system code page is `1252`. If the system code page is `1252`, then any `String` is `1252`.

   Refs:

   - [What is a `String`?](https://wiki.freepascal.org/String)
   - [https://forum.lazarus.freepascal.org/index.php?topic=58131.0](https://forum.lazarus.freepascal.org/index.php?topic=58131.0)


## Display UTF-8 on a console

```pascal
begin
  writeln('勤奋,勤勉になる,부지런하다!👍');
  writeln('Press Enter key to exit');
  readln;
end.                                 
```

Alternatively, you can assign your UTF-8 test to a `string`.

```pascal
var
  s: string = '勤奋,勤勉になる,부지런하다!👍';

begin
  writeln(s);
end.                        
```

!!! Note for Windows Users

    If you see garbage characters on console;
    
    1. your console might not support code page 65001, or
    2. your windows does not support UTF on API level (only read/write file in UTF-8)
   
   See this [answer from StackOverflow](https://stackoverflow.com/a/57134096) on how to enable code page 65001 on your console or on Windows (system-wide). **DO NOT MISS** the caveat section in that answer.

   Refs:

   - [https://wiki.freepascal.org/FPC_Unicode_support#Code_pages](https://wiki.freepascal.org/FPC_Unicode_support#Code_pages)
   - [https://stackoverflow.com/a/57134096](https://stackoverflow.com/a/57134096)
   - [https://superuser.com/a/1435645](https://superuser.com/a/1435645)


## What is my system's default codepage?

See [https://www.freepascal.org/docs-html/rtl/system/defaultsystemcodepage.html](https://www.freepascal.org/docs-html/rtl/system/defaultsystemcodepage.html)

```pascal
begin
  writeln(DefaultSystemCodePage); 
end.                                 
```

If it says `65001`, then you should be able to see UTF-8 characters on the console.