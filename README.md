# textadept-vi
textadept-vi is my humble attempt to bring vi-style modal editing to the (irresistibly fun-to-hack) Textadept editor. This project is not an attempt to recreate vi/vim via Textadept. Rather, it tries to bring the essential vi key bindings in a minimal and extensible way.

# Installation
```bash
git clone https://github.com/chadvoegele/textadept-vi.git ~/.textadept/modules/textadept-vi
echo "tavi = require('textadept-vi')" >> ~/.textadept/init.lua
```

In `$TEXTADEPT_HOME/src/scintilla`, apply the patches, `scintilla_*.patch`, and rebuild.
```bash
cd $TEXTADEPT_HOME/src/scintilla
patch -p1 < scintilla_block_cursor_selection.patch
patch -p1 < scintilla_set_rect_range.patch
cd $TEXTADEPT_HOME/src
make
```

# Status
## Working
* Motions: h, j, k, l, G, gg, f, F, t, T, $, 0, ^, w, b, i{char}, iw, a{char}, aw
* Count: all of above work with count prefix i.e. c3fa, d5l, ...
* Find: /, n, N
* Change Case: ~
* Replace: r
* Undo/Redo: u, cr
* Indentation: <, >
* Cut/Copy/Paste: x, d, dd,  y, yy, p, P
* Mode Switching: i, I, o, O, a, A, c, C, v, V, cv
* Command Mode (enters 'lua_command'): :

## Needs Some Work
* Find: ?
* Automatic Indentation: =
* Repeat Last Change: .

# Tests
In `$TEXTADEPT_HOME`, apply the patch, `test_textadept.patch`, and build textadept_headless.so.
```bash
cd $TEXTADEPT_HOME
patch -p1 < test_textadept.patch
cd $TEXTADEPT_HOME/src
make headless
```

Then run the tests.
```bash
cd $TEXTADEPTVI_HOME
lua test.lua -v
```

# Contributions
All contributions are welcome!

# Acknowledgements
Thanks to Mitchell for creating Textadept!
* https://foicica.com/textadept

Several others have developed similar textadept-vi implementations which have been immensely helpful as references. Many thanks to them!
* https://github.com/jugglerchris/textadept-vi
* https://github.com/pauldub/textadept-vim
* https://github.com/lammermann/ta-vim
