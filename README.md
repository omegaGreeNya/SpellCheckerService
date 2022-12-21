# SpellCheckerService

Web server with api to check text misspells.
## API Reference

#### CheckText

```http
  POST /api/CheckText
```

Text should be sended in request body as json object with `text` field.
Example:
```json
{"text":"Hellllo, mister cucucumber!"}
```
Will return:
```json
{"errors":["Hellllo","cucucumber"], "mark":3}
```
Where `errors` contains all finded misspelled words, and `mark` is resulted mark in range of 0..5. Each error decreases resulted mark by one.
## Run Locally

Clone the project

```bash
  git clone https://github.com/omegaGreeNya/SpellCheckerService
  
```

Go to project directory and build with stack

```bash
  stack build
```

Start the app with this command

```bash
  stack exec SpellCheckerService-exe
```

Alternatevly, you can install with `stack install` command and run just by `SpellCheckerService-exe`.


## Usage

You can specify server port by with first command line argument, if no port supplied, would be used default port `8081`.

## TO-DO
#### Usage related:
More verbose logging.\
External configuration (througth config file).
#### Usage unrelated:
Proper app top-layer.
