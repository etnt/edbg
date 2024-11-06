# Custom Colors

Custom colors may be set using the environment variable `EDBG_COLOR`.

It should contain a SPACE separated string with ITEM=COLOR entries.

Valid ITEMs are:

```
att    - attention color    (default: whiteb)
warn   - warning color      (default: yellow)
err    - error color        (default: red)
cur    - current line color (default: green)
```

Valid COLORs are:

```
black,   blackb
red,     redb
green,   greenb
yellow,  yellowb
blue,    blueb
magenta, magentab
cyan,    cyanb
white,   whiteb
```

Colors ending in 'b' are the bright variants.

Example:

```
export EDBG_COLOR="att=yellowb warn=magenta"
```

Colors not specified in `EDBG_COLOR` will keep their defaults.
