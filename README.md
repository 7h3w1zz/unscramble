# unscramble

unscramble words on the command line

```
Usage: unscramble [-d|--dictionary-file <file>] CHARACTERS 
                  [(-u|--unlimited) 
                    [(-l|--length <int>) | [-m|--min-length <int>] 
                      [-M|--max-length <int>]] [-c|--contains <string>]]
  Unscrambles CHARACTERS into a list of WORDS

Available options:
  -d,--dictionary-file <file>
                           Whitespace-separated word list to solve
                           from (default: "/usr/share/dict/words")
  CHARACTERS               Characters to unscramble
  -u,--unlimited           WORDS can contain any number of CHARACTERS
  -l,--length <int>        Exact length of WORDS
  -m,--min-length <int>    Minimum length of WORDS
  -M,--max-length <int>    Maximum length of WORDS
  -c,--contains <string>   Characters that WORDS must contain at least once
  -h,--help                Show this help text
```
