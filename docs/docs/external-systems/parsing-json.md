# Parsing JSON

Since many services use JSON for exchanging information, wouldn't it be nice to know how to do this in Free Pascal and Lazarus?


## How do I make a GET request?

!!! Note

    The snippet below is based on a fine tutorial by [Marcus Fernström](https://medium.com/@marcusfernstrm/freepascal-and-json-337c04cad489).

    Watch the detailed explanation by Marcus on YouTube; [Learn how to consume JSON data in Free Pascal](https://www.youtube.com/watch?v=Gy-OcEPgTHg)

Here is an example on making a GET request of JSON data and displaying it on console.

1. In `uses` add `opensslsockets` and `fphttpclient` for making the GET request. Line 11-12.
2. Set two variables. Line 16-17.
      - One for the URL. Here I use `https://dummyjson.com/users?limit=3`.
      - One for the response of the GET request.
3. Make the GET request by using `TFPHTTPClient.SimpleGet(url)`. Line 22.
4. Print result on console. Line 25.

```pascal linenums="1"  hl_lines="11-12 16 17 22 25"
program GetRequest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  // For making web requests
  opensslsockets,
  fphttpclient;


var
  url: string = 'https://dummyjson.com/users?limit=3'; // endpoint to get JSON mock data
  rawJson: string; // a var to store raw JSON data

begin
  // Get the raw JSON data
  WriteLn('Contacting ', url, ' ...');
  rawJson := TFPHTTPClient.SimpleGet(url);

  // Display JSON on console
  WriteLn(rawJson);

  // Pause console
  WriteLn('Press enter key to exit...');
  ReadLn;
end.
```


## How do I parse JSON from `dummyjson.org`?

!!! Note

    Again, the snippet below is based on a fine tutorial by [Marcus Fernström](https://medium.com/@marcusfernstrm/freepascal-and-json-337c04cad489).

    Watch the detailed explanation by Marcus on YouTube; [Learn how to consume JSON data in Free Pascal](https://www.youtube.com/watch?v=Gy-OcEPgTHg)

!!! Note

    WIP

```pascal
program ParseJSON;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  // For making web requests
  opensslsockets,
  fphttpclient,
  // For parsing JSON data
  fpjson,
  jsonparser;

var
  url: string = 'https://dummyjson.com/users?limit=3'; // endpoint to get JSON mock data
  rawJson: string;       // a var to store raw JSON data
  arrayJson: TJSONArray; // a var for processing an array of JSON objects
  enumJson: TJSONEnum;   // an enum type for looping JSON arrays
  objJson: TJSONObject;  // a var for manipulating a JSON object

begin
  // Get the raw JSON data
  WriteLn('Contacting ', url, ' ...');
  rawJson := TFPHTTPClient.SimpleGet(url);


  // Next, get the users array as TJSONArray;
  // 1. convert the raw JSON data to TJSONData,
  // 2. find data called "users" (a JSON array as per dummyjson's structure) and
  // 3. cast as TJSONArray
  arrayJson := TJSONArray(GetJSON(rawJson).FindPath('users'));

  // Loop using the TJSONEnum
  for enumJson in arrayJson do
  begin
    // Cast the enum value (TJSONData) to TJSONObject
    objJson := TJSONObject(enumJson.Value);

    // Output a few pieces of data as example.
    WriteLn('id   : ', objJson.FindPath('id').AsString);
    WriteLn('name : ', objJson.FindPath('firstName').AsString, ' ', objJson.FindPath('lastName').AsString);
    WriteLn('phone: ', objJson.FindPath('phone').AsString);
    WriteLn('city : ', objJson.FindPath('address.city').AsString);
    WriteLn('state: ', objJson.FindPath('address.state').AsString);
    WriteLn('---');
  end;

  // Pause console
  WriteLn('Press enter key to exit...');
  ReadLn;
end.
```