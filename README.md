MemEdit Memory Editor for Color Maximite 2 by Epsilon
-----------------------------------------------------
Current Version: 0.1

ChangeLog
---------
0.1:
- Initial version.

Description
-----------
MemEdit presents you with a view into CMM2's (virtual) memory and allows you to make changes. Essentially, it's a peek/poke front-end. This means that the tool is limited to the virtual address space exposed by peek and poke.  

The memory contents are shown as hexadecimal byte values and as ASCII code in two side-by-side blocks. Changes can be made both in the hex table section and in the ASCII section.

The usual file navigation with cursors keys, page up&down, home, end, etc. should work as expected.

MemEdit is a full-screen console-only editor. I does not work over serial, sorry.

The starting address to view can be passed in as a command line argument, e.g. run memedit MM.INFO(PROGRAM)

Commands such as goto and fill accept MMBasic syntax for address specifications, e.g. Go To Address (MMBasic syntax): MM.INFO(PAGE ADDRESS 0)

Press F1 to get help on key bindings.

Current limitations
-------------------
Console only.

