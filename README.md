Compiled with GHC version 9.4.7

## Running

Setup
```
stack install && stack build
```

Example Run Cmd
```
stack run './network.txt' 6
```

Unit tests can be run with
```
stack test artificial-script:unit-tests  
```

## Known Issues

The optional expressions statement allows any content after the percentage, but prevents further parsing. For example
```
I1: 50% gibberish
I2: 60% if I1
```
will only parse `I1: 50%`.

## Outstanding options
- Add to parser test suite
- Parser is currently strict on whitespace. This could be relaxed.
- Add validation of parsed network for logical consistency
  - Check all Expression Insurers exist
  - Check for impossible Expressions such as <5 with >5 or =4 with =5

## Notes

I had never used a parsing library (other than aeson) prior to this task. As such I created a manual parser first to ensure I had something to submit. Then I wrote a method using megaparsec to try a more elegant and robust solution.