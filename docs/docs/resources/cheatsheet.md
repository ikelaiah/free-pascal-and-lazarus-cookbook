# Cheatsheet (FPC)

## Types

### Built-in Integer Types

| Type     |                    Range                    | Size in bytes |
| -------- | :-----------------------------------------: | ------------: |
| Byte     |                  0 .. 255                   |             1 |
| Shortint |                 -128 .. 127                 |             1 |
| Smallint |               -32768 .. 32767               |             2 |
| Word     |                 0 .. 65535                  |             2 |
| Integer  |         either smallint or longint          |   size 2 or 4 |
| Cardinal |                  longword                   |             4 |
| Longint  |          -2147483648 .. 2147483647          |             4 |
| Longword |               0 .. 4294967295               |             4 |
| Int64    | -9223372036854775808 .. 9223372036854775807 |             8 |
| QWord    |          0 .. 18446744073709551615          |             8 |

### Built-in Real Types

| Type     |                     Range                     | Significant digits |   Size |
| -------- | :-------------------------------------------: | :----------------: | -----: |
| Real     |              platform dependant               |        ???         | 4 or 8 |
| Single   |               1.5E-45 .. 3.4E38               |        7–8         |      4 |
| Double   |              5.0E-324 .. 1.7E308              |       15–16        |      8 |
| Extended |             1.9E-4932 .. 1.1E4932             |       19–20        |     10 |
| Comp     |               -2E64+1 .. 2E63-1               |       19–20        |      8 |
| Currency | -922337203685477.5808 .. 922337203685477.5807 |       19–20        |      8 |