OPTION EXPLICIT
OPTION DEFAULT NONE
OPTION BASE 0
OPTION CONSOLE SCREEN

CONST VERSION$ = "0.2"

'--> Mapped address ranges: Can be referenced in goto command.
CONST SRAM% = &H38000000
CONST SRAM_END% = &H38010000 - 256

CONST RAM1% = &H30000000
CONST RAM1_END% = &H30040000 - 256

CONST RAM2% = &H40000000
CONST RAM2_END% = &H59000000

CONST AXI_RAM% =     &H24000000
CONST AXI_RAM_END% = &H24080000

CONST ROM1% = &H08000000
CONST ROM1_END% = &H08200000
CONST ROM2% = &H10000000
CONST ROM2_END% = &H20000000
'<--


CONST NUM_BYTES_PER_ROW% = 16

CONST NUM_ROWS% = 45
CONST START_ROW% = 2
CONST START_COL% = 13
CONST START_COL_ASC% = (START_COL% + NUM_BYTES_PER_ROW%*3 + 1)
CONST NUM_ADDR_DIGITS% = 8
CONST CURSOR_BLINK_PERIOD% = 500
CONST NON_PRINTABLE_CHAR_INDICATOR$ = "?" 'For non-printable characters this character is shown 
                                          'instead.

'--> The table below is used to convert a byte's column value to a screen X position.
'Bytes are clumped together or space out depending on selected word size. E.g. 12 34, or 1234, etc.
'The rows in the table correspond to the 4 different word sizes, the columns to column values.
CONST NUM_WORD_SIZES% = 4
DIM COL_TO_X%(NUM_BYTES_PER_ROW%-1, NUM_WORD_SIZES% - 1)
COL_TO_X_DATA:
  DATA 0, 1*3, 2*3, 3*3, 4*3, 5*3, 6*3, 7*3, 8*3, 9*3, 10*3, 11*3, 12*3, 13*3, 14*3, 15*3
  DATA 0,   2, 2*3,   8, 4*3,  14, 6*3,  20, 8*3,  26, 10*3,   32, 12*3,   38, 14*3,   44
  DATA 0,   2,   4,   5, 4*3,  14,  16,  18, 8*3,  26,   28,   30, 12*3,   38,   40,   42
  DATA 0,   2,   4,   6,   8,  10,  12,  14, 8*3,  26,   28,   30,   32,   34,   36,   38

SUB initColToX 'This is a workaround for 2D array pre-initialization.
  LOCAL ii%,jj%

  RESTORE COL_TO_X_DATA
  FOR jj%=0 TO NUM_WORD_SIZES%-1
    FOR ii%=0 TO NUM_BYTES_PER_ROW%-1
      READ COL_TO_X%(ii%, jj%) 
    NEXT ii%
  NEXT jj%
END SUB

initColToX
'<--

'The address corresponding to the byte in row 0, column 0. We're preinitializing it to PAGE 0 because
'it's mapped and it's an interesting place to start.
DIM topLeftAddress% = MM.INFO(PAGE ADDRESS 0) 
DIM exitRequested% = 0

'--> These variables are inputs to positionCursorInTable/ASCblock.
DIM crsrRow% = 0, crsrCol% = 0, crsrNibbleOffset% = 0
'<--

'--> The following variables can only be modified by positionCursorInTable/ASCblock:
DIM crsrScrnXpos% = 0, crsrScrnYpos% = 0
DIM crsrAddress%=topLeftAddress% 'Address corresponding to element at cursor
'These are booleans. If the cursor is in the ASCII block, both are 0.
DIM crsrOnLeftNibble%, crsrOnRightNibble%
'<--

'A semaphore from pageRefresh to positionCursorInTable/ASCblock.
DIM pageRefreshed%=0

'Word size (in bytes) in hex table: 1, 2, 4 or 8
'Note that this editor fundamentally remains a byte level editor. Word size just affect how the
'data is formatted in the hex table.
'Also note that because this is an editor for CMM2, little endianness is assumed.
DIM wordSize% = 1
DIM log2WordSize% = 0 '0-3 corrspondening to word sizes 1, 2, 4 and 8. 

IF MM.CMDLINE$ <> "" THEN
  DIM cmdLineAddr% = -1 
  ON ERROR IGNORE 1
    cmdLineAddr% = EVAL(MM.CMDLINE$)
  IF cmdLineAddr% <> -1 THEN
    topLeftAddress% = cmdLineAddr%
  ENDIF
ENDIF

MODE 1, 8
FONT 1, 1

PAGE WRITE 1
COLOUR RGB(WHITE), RGB(BLUE)
CLS
PAGE WRITE 0
COLOUR RGB(WHITE), RGB(BLUE)
CLS

printHeader
LINE 0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6,MM.HRES-1,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6,,RGB(WHITE)
printFooter

refreshPage
crsrRow% = 0 : crsrCol% = 0 : crsrNibbleOffset% = 0: positionCursorInTable

DIM blinkCursorFlag% = 0
settick CURSOR_BLINK_PERIOD%, blinkCursorInt, 1

'--> The main loop:
DO WHILE exitRequested% = 0
  IF blinkCursorFlag% THEN
    blinkCursor
    blinkCursorFlag% = 0
  ENDIF

  checkKey
  printFooter 'Reprint footer because it contains status info.
LOOP
'<--

CLS RGB(BLACK)

EndOfProg:
CLEAR
END

'--> Memory buffer accessor functions:
'Write byte specified by val$ in memory given address.
SUB writeByte(addr%, val$)
  ON ERROR IGNORE 1
    POKE BYTE addr%, ASC(val$)
END SUB

'Read byte from memory at given address.
FUNCTION readByte$(addr%)
  LOCAL res$ = ""
  ON ERROR IGNORE 1
    res$ = CHR$(PEEK (BYTE addr%))
  readByte$ = res$
END FUNCTION
'<--

'Returns true if the cursor is in the table, not the ASCII block
FUNCTION cursorIsInTable%()
  cursorIsInTable% = (crsrOnLeftNibble%=1) OR (crsrOnRightNibble%=1)
END FUNCTION

'Not to be confused with refresRow below. This function is used by exportTxt and prints one line 
'to file #1.
SUB printRow(address%)
  LOCAL addrl% = address%
  LOCAL col%, x%, prevX% = START_COL%
  LOCAL elem$
  LOCAL byteOffset%, wordOffset%

  'address in Hex
  PRINT #1, "&H" HEX$(addrl%,NUM_ADDR_DIGITS%) ":  ";

  'The Hex byte section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    x% = (START_COL% + COL_TO_X%(col%, log2WordSize%))

    'Offset monotonically increments across the row. This means that for word sizes other than 1,
    'offset no longer directly corresponds to a byte address. Endianness needs to be taken into 
    'account. E.g. for word size 4, the address offsets across the row go like this:
    '03020100 07060504 ...
    'The following two statements make the conversion:
    'Offset of the word we're on.
    wordOffset% = (addrl%>>log2WordSize%)<<log2WordSize% 
    'From there we find the offset of the byte we're on.
    byteOffset% = wordSize% - 1 - addrl% + 2*wordOffset% 

    elems$ = readByte$(byteOffset%)
    IF elem$ <> "" THEN
      elem$ = HEX$(ASC(elem$),2)
    ELSE
      elem$ = "--"
    ENDIF

    addrl% = addrl% + 1

    PRINT #1, SPACE$(x%-prevX%-2) elem$;
    prevX% = x%
  NEXT col%

  addrl% = address%
  col%=0

  PRINT #1, SPACE$(START_COL_ASC%-prevX%-2);

  'The ASCII block section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    elem$ = readByte$(addrl%)
    IF elem$ = "" THEN
      elem$ = "."
    ELSE IF NOT isPrintable%(elem$) THEN
      elem$ = NON_PRINTABLE_CHAR_INDICATOR$
    ENDIF

    addrl% = addrl%+1

    PRINT #1, elem$;
  NEXT col%

  PRINT #1, "" 'newline
END SUB

'Export from startAddr to endAddr as text to exportFilename.
'We actually start from the row holding startAddr so we maintain
'the same row/address alignment as shown in the editor.
SUB exportTxt(startAddr%, endAddr%, exportFilename$)
  LOCAL location%
  LOCAL addr% = rowStartAddr%(startAddr%)

  OPEN exportFilename$ FOR OUTPUT AS #1

  DO WHILE addr% <= endAddr%
    printRow addr%
    addr% = addr% + NUM_BYTES_PER_ROW%
  LOOP

  CLOSE #1
END SUB

'Export from startAddr to endAddr as binary to exportFilename.
'Returns true if succesful, false if failed.
FUNCTION exportBin%(startAddr%, endAddr%, exportFilename$)
  LOCAL location% = startAddr%
  LOCAL elem$
  LOCAL res% = 1

  OPEN exportFilename$ FOR OUTPUT AS #1

  DO WHILE (location% <= endAddr%) AND (res%=1)
    elem$ = readByte$(location%)
    IF elen$ <> "" THEN
      PRINT #1, elem$;
    ELSE
      promptMsg "Address range contains unmapped addresses. Aborting export.", 1
      res% = 0
    ENDIF

    location% = location% + 1
  LOOP

  CLOSE #1
  exportBin% = res%
END FUNCTION

'Refresh the whole page on the screen
SUB refreshPage
  LOCAL row%
  LOCAL offset% = topLeftAddress%

  FOR row%=0 TO NUM_ROWS%-1
    refreshRow offset%, row%, 1
  NEXT row%

  BLIT 0, START_ROW%*MM.INFO(FONTHEIGHT), 0, START_ROW%*MM.INFO(FONTHEIGHT), MM.HRES, NUM_ROWS%*MM.INFO(FONTHEIGHT), 1

  'This is a semaphore to signal to positionCursorInTable/ASCblock that the page has been refreshed.
  pageRefreshed% = 1
END SUB

'Refreshes given row number on the screen with the contents at address.
'End offset is passed back up to the caller.
SUB refreshRow(address%, row%, skipBlit%)
  LOCAL addrl% = address%
  LOCAL row_l% = row% + START_ROW%
  LOCAL col%, x%, y%
  LOCAL elem$
  LOCAL invert%
  LOCAL byteOffset%, wordOffset%

  y% = row_l%*MM.INFO(FONTHEIGHT)
  x% = 0

  PAGE WRITE 1

  'Address in Hex
  PRINT @(x%,y%,0) "&H" HEX$(addrl%,NUM_ADDR_DIGITS%) ":";

  'The Hex byte section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    invert% = 0
    x% = (START_COL% + COL_TO_X%(col%, log2WordSize%))*MM.INFO(FONTWIDTH)

    'Offset monotonically increments across the row. This means that for word sizes other than 1,
    'offset no longer directly corresponds to an address offset. Endianness needs to be taken into 
    'account. E.g. for word size 4, the address offsets across the row go like this:
    '03020100 07060504 ...
    'The following two statements make the conversion:
    'Offset of the word we're on
    wordOffset% = (addrl%>>log2WordSize%)<<log2WordSize%
    'From there find the offset of the byte we're on. 
    byteOffset% = wordSize% - 1 - addrl% + 2*wordOffset% 

    elem$ = readByte$(byteOffset%)
    IF elem$ <> "" THEN
      elem$ = HEX$(ASC(elem$),2)
    ELSE
      elem$ = "--"
    ENDIF

    addrl% = addrl%+1

    PRINT @(x%,y%,invert%) elem$;: PRINT "        ";
  NEXT col%

  addrl% = address%
  col%=0
  
  'The ASCII block section.
  FOR col% = 0 TO (NUM_BYTES_PER_ROW%-1)
    x% = (START_COL_ASC% + col%)*MM.INFO(FONTWIDTH)

    elem$ = readByte$(addrl%)
    IF elem$ = "" THEN
      elem$ = "."
    else IF NOT isPrintable%(elem$) THEN
      elem$ = NON_PRINTABLE_CHAR_INDICATOR$
    ENDIF

    addrl% = addrl%+1

    PRINT @(x%,y%,0) elem$;
  NEXT col%

  PAGE WRITE 0
  IF NOT skipBlit% THEN
    BLIT 0, y%, 0, y%, MM.HRES, MM.INFO(FONTHEIGHT), 1
  ENDIF

  'Return end offset to caller
  address% = addrl%
END SUB

SUB printHeader
 LOCAL header$ = "MemEdit V"+VERSION$+" by Epsilon.";
 LOCAL byteNr$ = "             00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F  0123456789ABCDEF"

 'Print inverted
 PRINT @(0,0,2) header$ + SPACE$(MM.HRES/MM.INFO(FONTWIDTH) - LEN(header$))
 color rgb(CYAN)
 PRINT @(0,MM.INFO(FONTHEIGHT),2) byteNr$ + SPACE$(MM.HRES/MM.INFO(FONTWIDTH) - LEN(byteNr$))
 color rgb(WHITE)
END SUB

SUB printFooter
  LOCAL footer$ = "Mode: " + STR$(wordSize%*8) +"-bit"
  
  'Print inverted.
  PRINT @(0,(NUM_ROWS%+4)*MM.INFO(FONTHEIGHT),2) footer$ + SPACE$(MM.HRES/MM.INFO(FONTWIDTH) - LEN(footer$) - 11) + "F1 = Help  ";
END SUB

'Prints the given text on the prompt line, then waits for input. 
'The input string is returned to the caller.
FUNCTION promptForText$(text$)
  LOCAL inputStr$
  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) text$;
  INPUT "", inputStr$
  emptyInputBuffer
  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) SPACE$(MM.HRES/MM.INFO(FONTWIDTH));
  promptForText$ = inputStr$  
END FUNCTION

'Prints the given text on the prompt line, then waits for the user to press any key. 
'The pressed key is returned to the caller.
FUNCTION promptForAnyKey$(text$)
  LOCAL pressedKey$
  LOCAL latchedTime% = INT(TIMER)

  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) text$;
  LOCAL crsrPos% = (LEN(text$)+1)*MM.INFO(FONTWIDTH)
  LOCAL invert% = 0

  emptyInputBuffer

  'An overly complex way of getting a blinking cursor at the prompt...
  DO: 
    pressedKey$ = INKEY$ 
    PRINT @(crsrPos%, (NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4, invert%) " ";
    IF (INT(TIMER) > latchedTime% + CURSOR_BLINK_PERIOD%) THEN
      invert% = invert% XOR 2
      latchedTime% = INT(TIMER)
    ENDIF
  LOOP UNTIL pressedKey$ <> ""

  PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) SPACE$(MM.HRES/MM.INFO(FONTWIDTH));

  promptForAnyKey$ = pressedKey$
END FUNCTION

'If on%=1, prompt text is shown. If on%=0 prompt text is removed.
SUB promptMsg(text$, on%)
  IF on%=1 THEN
    'LINE 0, (NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6, MM.HRES-1, (NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6,, RGB(WHITE)
    PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) text$;
    emptyInputBuffer
  ELSE
    PRINT @(0,(NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-4) SPACE$(MM.HRES/MM.INFO(FONTWIDTH));
    'LINE 0, (NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6, MM.HRES-1, (NUM_ROWS%+3)*MM.INFO(FONTHEIGHT)-6,, RGB(BLUE)
  ENDIF
END SUB

'Return true if given character is printable
FUNCTION isPrintable%(char$)
  isPrintable% = (char$ >= CHR$(32))
END FUNCTION

'Returns true if the given address is currently shown on the screen. 
'If includeLastRow% = 0, pretend that the last row is not shown on the screen.
FUNCTION addrIsOnScreen%(addr%, includeLastRow%)
  addrIsOnScreen% = (addr% >= topLeftAddress%) AND (addr% <= topLeftAddress% + NUM_BYTES_PER_ROW%*(NUM_ROWS%-1+includeLastRow%) - 1)
END FUNCTION

'Return the address of the 1st byte in the row containing given address.
FUNCTION rowStartAddr%(addr%)
  rowStartAddr% = (addr%\NUM_BYTES_PER_ROW%)*NUM_BYTES_PER_ROW%
END FUNCTION

'Convert hex table column position to address offset (rel. to start of column) taking word size into 
'account.
FUNCTION tblColToAddrOffset%(col%)
  tblColToAddrOffset% = (col%\wordSize% + 1)*wordSize% - 1 - (col% MOD wordSize%)
END FUNCTION

'Convert address to hex table column position taking word size into account
FUNCTION addressToTblCol%(address%)
  LOCAL byteOffsetInRow% = address% - rowStartAddr%(address%)
  LOCAL wordOffsetInRow% = (byteOffsetInRow%\wordSize%)*wordSize%
  addressToTblCol% = 2*wordOffsetInRow% + wordSize% - 1 - byteOffsetInRow%
END FUNCTION

'Convert address to ASCII block column
FUNCTION adressToASCcol%(address%)
  adressToASCcol% = (address% - topLeftAddress%) MOD NUM_BYTES_PER_ROW%
END FUNCTION

'Position the cursor at the given address. Scroll if necessary.
'Set allowCursorOnLastRow%=1 if you want to allow positioning of cursor on last row rather than scrolling.
SUB positionCursorAtAddr(addr%, allowCursorOnLastRow%)
  LOCAL addrl% = addr%

  IF addrIsOnScreen%(addrl%, allowCursorOnLastRow%) = 0 THEN
    IF addrl% > topLeftAddress% THEN 'Move forward to middle of screen.
      topLeftAddress% = (addrl%\NUM_BYTES_PER_ROW% - NUM_ROWS%\2)*NUM_BYTES_PER_ROW% 
    ELSE 'Move backward to top of screen.
      topLeftAddress% = rowStartAddr%(addrl%)
    ENDIF
    refreshPage
  ENDIF

  crsrRow% = (addrl% - topLeftAddress%)\NUM_BYTES_PER_ROW%
  crsrNibbleOffset% = 0

  IF cursorIsInTable%() THEN
    crsrCol% = addressToTblCol%(addrl%)
    positionCursorInTable
  ELSE
    crsrCol% = adressToASCcol%(addrl%)
    positionCursorInASCblock
  ENDIF
END SUB

'Position cursor in the hex table at crsrRow/crsrCol/crsrNibbleOffset
'crsrRow/Col are byte element positions, not screen coordinates. crsrNibbleOffset is 0 or 1 for 
'left or right nibble.
'crsrRow increments from left to right. crsCol increments from top to bottom.
'This sub won't scroll the page. We're positioning the cursor somewhere on the current page.
SUB positionCursorInTable
  'Un-Reverse previous char position, unless we just had a page refresh.
  IF NOT pageRefreshed% THEN
    drawCharAtCursor 0
  ELSE
    pageRefreshed% = 0
  ENDIF

  'Start column + word offset including spacing between words + byte offset in word + nibble offset 
  'in bytes.
  crsrScrnXpos% = START_COL% + COL_TO_X%(crsrCol%, log2WordSize%) + crsrNibbleOffset%
  'crsrScrnXpos% = START_COL% + (crsrCol%\wordSize%)*(wordSize%*3) + (crsrCol% MOD wordSize%)*2 + crsrNibbleOffset%
  crsrScrnYpos% = crsrRow% + START_ROW%
  crsrOnLeftNibble% = (crsrNibbleOffset%=0)
  crsrOnRightNibble% = (crsrNibbleOffset%=1)
  crsrAddress% = topLeftAddress% + crsrRow%*NUM_BYTES_PER_ROW% + tblColToAddrOffset%(crsrCol%)

  drawCharAtCursor 2 '2 indicates print reversed.
END SUB

'Position cursor in the ASCII block at crsrRow/crsrCol.
'crsrRow/Col are element positions, not screen coordinates.
SUB positionCursorInASCblock
  'Un-Reverse previous char position, unless we just had a page refresh.
  IF NOT pageRefreshed% THEN
    drawCharAtCursor 0
  ELSE
    pageRefreshed% = 0
  ENDIF

  crsrScrnXpos% = START_COL_ASC% + crsrCol%
  crsrScrnYpos% = crsrRow% + START_ROW%
  crsrOnLeftNibble% = 0
  crsrOnRightNibble% = 0
  crsrAddress% = topLeftAddress% + crsrRow%*NUM_BYTES_PER_ROW% + crsrCol%

  drawCharAtCursor 2 '2 indicates print reversed.
END SUB

'The IRQ just sets the flag, which'll cause blinkCursor below to be called from the main loop.
SUB blinkCursorInt
  blinkCursorFlag% = 1
END SUB

SUB blinkCursor
  STATIC invert%=2
  'A cursor is emulated by alternating between regular and reverse print of the character at the 
  'cursor position.
  drawCharAtCursor invert% 
  invert% = invert% XOR 2
END SUB

'Print the character at the cursor position. The actual inputs to this function are globals:
'crsrAddress (indicating at which memory address the character to be printed is located)
'crsrScreenXpos/crsrScrnYpos and crsrOnLeft/RightNibble.
'The invert flag indicates if the character should be printed in reversed-font.
SUB drawCharAtCursor(invert%)
  LOCAL char$
  LOCAL invertl% = invert%

  
  char$ = readByte$(crsrAddress%)
  IF char$ <> "" THEN
    IF crsrOnLeftNibble% = 1 THEN
      char$ = HEX$(ASC(char$)\16, 1)
    ELSEIF crsrOnRightNibble% = 1 THEN
      char$ = HEX$(ASC(char$) AND 15, 1)
    ENDIF
  ELSE  
    IF cursorIsInTable%() THEN
      char$ = "-"
    ELSE
      char$ = "."
    ENDIF
  ENDIF

  PRINT @(crsrScrnXpos%*MM.INFO(FONTWIDTH), crsrScrnYpos%*MM.INFO(FONTHEIGHT), invertl%) char$;
END SUB

'Helper for showHelpPopup below
SUB newline(y%)
  y% = y% + MM.INFO(FONTHEIGHT)
END SUB

'help popup is prepared on a separate page in a Box, then shown on page 0 using a sprite.
SUB showHelpPopup
  LOCAL longestStringLen% = LEN("Any MMBasic expression evaluating to a valid address should work.")
  LOCAL numLines% = 25
  LOCAL boxWidth% = (longestStringLen%+4)*MM.INFO(FONTWIDTH)
  LOCAL boxHeight% = (numLines%+4)*MM.INFO(FONTHEIGHT)

  PAGE WRITE 2
  BOX 0, 0, boxWidth%, boxHeight%, 4, RGB(RED), RGB(WHITE)
  
  LOCAL x% = 2*MM.INFO(FONTWIDTH)
  LOCAL y% = 2*MM.INFO(FONTHEIGHT)

  PRINT @(x%,y%,2) "Help - Key Bindings:"; : newline y%
  newline y%                  
  PRINT @(x%,y%,2) "F1 = Help"; : newline y%
  PRINT @(x%,y%,2) "Ctrl-Q = Quit"; : newline y%
  PRINT @(x%,y%,2) "Ctrl-E = Export"; : newline y%
  PRINT @(x%,y%,2) "Ctrl-F = Fill"; : newline y%
  PRINT @(x%,y%,2) "Ctrl-G = Goto"; : newline y%
  PRINT @(x%,y%,2) "Ctrl-P = Screenshot"; : newline y%
  PRINT @(x%,y%,2) "Ctrl-T = Toggle Word Size"; : newline y%
  PRINT @(x%,y%,2) "Home = Go To Top of Page"; : newline y%
  PRINT @(x%,y%,2) "End = Go To End of Page"; : newline y%
  PRINT @(x%,y%,2) "PgUp = Move one Page Up"; : newline y%
  PRINT @(x%,y%,2) "PgDn = Move one Page Down"; : newline y%
  newline y%
  PRINT @(x%,y%,2) "Any MMBasic expression evaluating to a valid address should work."; : newline y%
  PRINT @(x%,y%,2) "E.g. Ctrl-G -> MM.INFO(PAGE ADDRESS 0)"; : newline y%
  newline y%

  PRINT @(x%,y%,2) "The following address ranges are mapped and can be accessed using"; : newline y% 
  PRINT @(x%,y%,2) "constants below:"; : newline y%
  newline y%
  PRINT @(x%,y%,2) "SRAM% =     &H" + HEX$(SRAM%,8)        + "  SRAM_END% =    &H" + HEX$(SRAM_END%,8) : newline y%
  PRINT @(x%,y%,2) "RAM1% =     &H" + HEX$(RAM1%,8)        + "  RAM1_END% =    &H" + HEX$(RAM1_END%,8) : newline y%
  PRINT @(x%,y%,2) "RAM2% =     &H" + HEX$(RAM2%,8)        + "  RAM2_END% =    &H" + HEX$(RAM2_END%,8) : newline y%
  PRINT @(x%,y%,2) "AXI_SRAM% = &H" + HEX$(AXI_RAM%,8)     + "  AXI_RAM_END% = &H" + HEX$(AXI_RAM_END%,8) : newline y%
  PRINT @(x%,y%,2) "ROM1% =     &H" + HEX$(ROM1%,8)        + "  ROM1_END% =    &H" + HEX$(ROM1_END%,8) : newline y%
  PRINT @(x%,y%,2) "ROM2% =     &H" + HEX$(ROM2%,8)        + "  ROM2_END% =    &H" + HEX$(ROM2_END%,8) : newline y%

  PAGE WRITE 0

  SPRITE READ 1, 0 , 0, boxWidth%, boxHeight%, 2
  SPRITE SHOW 1, MM.HRES\2 - boxWidth%\2, MM.VRES\2 - boxHeight%\2, 1
END SUB

SUB removeHelpPopup
  SPRITE CLOSE 1
END SUB

'Scroll down one line by moving the address corresponding to the top left of the screen,
'then calling a full page refresh.
SUB scrollLineDown
  IF topLeftAddress% >= NUM_BYTES_PER_ROW% THEN
    topLeftAddress% = topLeftAddress% - NUM_BYTES_PER_ROW%
    refreshPage
  ENDIF
END SUB

'Scroll up one line by moving the address corresponding to the top left of the screen,
'then calling a full page refresh,
SUB scrollLineUp
  topLeftAddress% = MIN(topLeftAddress% + NUM_BYTES_PER_ROW%, &HFFFFFFFF - NUM_BYTES_PER_ROW%*NUM_ROWS%)
  refreshPage
END SUB

'Scroll one page by moving the address corresponding to the top left of the screen,
'then calling a full page refresh.
SUB scrollPageDown
  IF topLeftAddress% >= NUM_BYTES_PER_ROW% THEN
    topLeftAddress% = MAX(topLeftAddress% - NUM_BYTES_PER_ROW%*NUM_ROWS%, 0)
    refreshPage
  ENDIF
END SUB

'Scrollone page by moving the address corresponding to the top left of the screen,
'then calling a full page refresh.
SUB scrollPageUp
  topLeftAddress% = MIN(topLeftAddress% + NUM_BYTES_PER_ROW%*NUM_ROWS%, &HFFFFFFFF - NUM_BYTES_PER_ROW%*NUM_ROWS%)
  refreshPage
END SUB

'Move the cursor up on the screen, scroll if needed.
SUB cursorUp
  IF crsrRow% > 0 THEN
    crsrRow% = crsrRow% - 1
  ELSE
    scrollLineDown
  ENDIF
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'Move the cursor down on the screen, scroll if needed.
SUB cursorDown
  IF crsrRow% < NUM_ROWS%-1 THEN
    crsrRow% = crsrRow% + 1
  ELSE
    scrollLineUp
  ENDIF
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'Move the cursor left on the screen. There are many cases here...
SUB cursorLeft
  LOCAL posInTableNotASC%

  IF cursorIsInTable%() <> 0 THEN
    IF crsrNibbleOffset%=1 THEN 'We're on a right nibble in the hex table. Move to the left nibble.
      crsrNibbleOffset% = 0
      posInTableNotAsc% = 1
    ELSE
      IF crsrCol% > 0 THEN 'We're on the left nibble in the hex table, not in the leftmost column.
        crsrNibbleOffset% = 1 'We move to the right nibble of the previous byte.
        crsrCol% = crsrCol% - 1
        posInTableNotAsc% = 1
      ELSE
        IF crsrRow% > 0 THEN 'We're on the left nibble of the leftmost column, but not on the top row.
          crsrRow% = crsrRow% - 1 'We move one row up, to the last column in the ASCII block.
          crsrCol% = NUM_BYTES_PER_ROW% - 1
          posInTableNotASC% = 0
        ELSE 'crsrRow = 0
          IF topLeftAddress% > 0 THEN 'Leftmost column, top row, not address 0.
            scrollLineDown          'scroll one line, then move to last column in the ASCII block.
            crsrCol% = NUM_BYTES_PER_ROW% - 1
            posInTableNotASC% = 0
          ELSE
            posInTableNotASC% = 1 'Leftmost column, top row, address 0. Do nothing.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ELSE 'In ASC block
    IF crsrCol% > 0 THEN 'We 're in the ASCII block, not first column. Just move left.
      crsrCol% = crsrCol% - 1
      posInTableNotASC% = 0
    ELSE
      posInTableNotASC% = 1 'We're in the ASCII block, first colument. 
      crsrCol% = NUM_BYTES_PER_ROW% - 1 'Jump to last column of hex table on same row
      crsrNibbleOffset% = 1               'Rightmost nibble.
    ENDIF
  ENDIF

  IF posInTableNotASC% THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'stayInBlock indicates whether cursor at the end of the block (table or ASC block)
'should advance to the other block (stayInBlock=0) or go to the next line in the current block 
'(stayInBlock=1). Again, many cases, by similar logic as in cursorLeft above.
SUB cursorRight(stayInBlock%)
  LOCAL posInTableNotASC%

  IF cursorIsInTable%() THEN
    IF crsrNibbleOffset%=0 THEN 'Cursor is on the left digit
      crsrNibbleOffset% = 1
      posInTableNotASC% = 1
    ELSE
      IF crsrCol% < NUM_BYTES_PER_ROW% - 1 THEN 'Cursor is on the right digit but not at end of line
        crsrNibbleOffset% = 0
        crsrCol% = crsrCol% + 1
        posInTableNotASC% = 1
      ELSE IF stayInBlock% = 0 THEN 'At the end of the line, move to the ASCII block
        crsrCol% = 0
        posInTableNotASC% = 0
      ELSE 'At the of the line, Stay in Block, so go to start of next line
        IF crsrRow% < NUM_ROWS% - 1 THEN 'If not at the end of the page, just go down one row
          crsrRow% = crsrRow% + 1
          crsrCol% = 0
          crsrNibbleOffset% = 0
          posInTableNotASC% = 1
        ELSE 'If at the end of the page scroll up one line
          crsrCol% = 0
          crsrNibbleOffset% = 0
          posInTableNotASC% = 1
          scrollLineUp
        ENDIF
      ENDIF
    ENDIF
  ELSE 'In ASC block
    IF crsrCol% < NUM_BYTES_PER_ROW% - 1 THEN 'Not at the end of the line
      crsrCol% = crsrCol% + 1
      posInTableNotASC% = 0
    ELSE 'Last column:
      IF crsrRow% < NUM_ROWS% - 1 THEN 'Not last row:
        crsrRow% = crsrRow% + 1
        crsrCol% = 0
        crsrNibbleOffset% = 0
      ELSE 'Last row:
        scrollLineUp
        crsrCol% = 0
        crsrNibbleOffset% = 0
      ENDIF

      posInTableNotASC% = NOT stayInBlock% 
    ENDIF
  ENDIF

  IF posInTableNotASC% THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'pageUp key handler
SUB pageUp
  scrollPageDown
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

'pageDown key handler
SUB pageDown
  scrollPageUp
  IF cursorIsInTable%() <> 0 THEN
    positionCursorInTable
  ELSE
    positionCursorInASCblock
  ENDIF
END SUB

SUB homeKeyHandler
  positionCursorAtAddr topLeftAddress%, 1
END SUB

SUB endKeyHandler
  positionCursorAtAddr topLeftAddress% + NUM_ROWS%*NUM_BYTES_PER_ROW% - 1, 1
END SUB

'Ctrl-Q (Quit) key handler.
SUB quitKeyHandler
    exitRequested% = 1
END SUB

'Ctrl-G (Go To addr) key handler.
SUB gotoKeyHandler
  LOCAL addrStr$ = promptForText$("Go To Address (MMBasic Syntax): ")
  LOCAL addrInt% = -1
  ON ERROR IGNORE 1
    addrInt% = EVAL(addrStr$)
  IF addrInt% <> -1 THEN
    positionCursorAtAddr addrInt%, 0
  ELSE
    promptMsg "Invalid address", 1
  ENDIF
END SUB

SUB toggleKeyHandler
  'Increment with wraparound
  log2wordSize% = (log2wordSize% + 1) AND 3
  wordSize% = 1<<log2wordSize%  'Sync wordSize to log2WordSize
  refreshPage
  positionCursorAtAddr crsrAddress%, 1
END SUB

SUB exportKeyHandler
  LOCAL startAddrStr$ = promptForText$("Export from Address (MMBasic Syntax): ")
  LOCAL startAddrInt% = -1
  ON ERROR IGNORE 1
    startAddrInt% = EVAL(startAddrStr$)

  IF startAddrInt% < 0 THEN
    promptMsg "Start address invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL endAddrStr$ = promptForText$("To Address (MMBasic Syntax): ")
  LOCAL endAddrInt% = -1
  ON ERROR IGNORE 1
    endAddrInt% = EVAL(endAddrStr$)

  IF endAddrInt% < startAddrInt% THEN
    promptMsg "End address invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL binOrTxt$ = ""
  DO WHILE binOrTxt$<>"B" AND binOrTxt$<>"T" 
    binOrTxt$=UCASE$(promptForAnyKey$("Binary or Text? (B/T)")) 
  LOOP

  LOCAL exportFilename$ = promptForText$("Export to file: ")
  IF exportFilename$ = "" THEN
    promptMsg "Export aborted.", 1
    EXIT SUB
  ENDIF

  IF MM.INFO(FILESIZE exportFilename$) <> -1 THEN 
    IF UCASE$(promptForAnyKey$("File exists. Overwrite? (Y/N)")) <> "Y" THEN
      promptMsg "Export aborted.", 1
      EXIT SUB
    ENDIF
  ENDIF

  promptMsg "Exporting...", 1

  LOCAL exportRes%
  IF binOrTxt$ = "B" THEN
    exportRes% = exportBin%(startAddrInt%, endAddrInt%, exportFilename$)
  ELSE
    exportTxt startAddrInt%, endAddrInt%, exportFilename$
    exportRes% = 1
  ENDIF

  IF exportRes% = 1 THEN 'Only print done if successful.
    promptMsg "Done.       ", 1
  ENDIF
END SUB

'Empty the keyboard input buffer
SUB emptyInputBuffer
  DO WHILE INKEY$ <> ""
  LOOP
END SUB

SUB paasei
  PAGE COPY 0 TO 4
  PAGE WRITE 3  
  CLS RGB(BLACK)
  CIRCLE MM.HRES/2, MM.VRES/2, (MM.VRES/2)-10, 10, 1, RGB(BLACK), RGB(YELLOW)
  CIRCLE MM.HRES/2-100, MM.VRES/2-100, 40, 1, 1/2, RGB(BLACK), RGB(BLACK)
  CIRCLE MM.HRES/2+100, MM.VRES/2-100, 40, 1, 1/2, RGB(BLUE), RGB(BLUE)
  ARC MM.HRES/2, MM.VRES/2, (MM.VRES/4)-5, (MM.VRES/4)+5, 100, 260, RGB(BLACK)
  PAGE WRITE 5
  LOCAL scale!=0.1
  LOCAL newx%, newy%
  DO WHILE scale! < 1
    newx% = MM.HRES*(1 - scale!)/2
    newy% = MM.VRES*(1 - scale!)/2
    PAGE COPY 4 TO 5
    IMAGE RESIZE_FAST 0, 0, MM.HRES, MM.VRES, newx%, newy%, MM.HRES*scale!, MM.VRES*scale!, 3, 1
    PAGE COPY 5 TO 0, B
    scale! = scale! * 1.1
  LOOP
  PAGE COPY 4 TO 0, B
  PAGE WRITE 0
END SUB

SUB helpKeyHandler
  showHelpPopup
  LOCAL dummy$ = promptForAnyKey$("")
  removeHelpPopup
END SUB

SUB fillKeyHandler
  LOCAL startAddrStr$ = promptForText$("Fill from Address (MMBasic syntax): ")
  LOCAL startAddrInt% = -1
  ON ERROR IGNORE 1
    startAddrInt% = EVAL(startAddrStr$)

  IF startAddrInt% < 0 THEN
    promptMsg "Start address invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL endAddrStr$ = promptForText$("To Address (MMBasic syntax): ")
  LOCAL endAddrInt% = -1
  ON ERROR IGNORE 1
    endAddrInt% = EVAL(endAddrStr$)

  IF endAddrInt% < startAddrInt% THEN
    promptMsg "End address invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL valueStr$ = promptForText$("Byte (MMBasic syntax): ")
  LOCAL valueInt% = -1 
  ON ERROR IGNORE 1
    valueInt% = EVAL(valueStr$)
  
  IF (valueInt% < 0) OR (valueInt% > 255) THEN
    promptMsg "Fill value invalid.", 1
    EXIT SUB
  ENDIF

  LOCAL index%

  promptMsg "Filling...", 1

  FOR index% = startAddrInt% TO endAddrInt%
    writeByte index%, CHR$(valueInt%)
  NEXT index%

  promptMsg "Done.     ", 1

  positionCursorAtAddr startAddrInt%, 0 'Move the cursor to the 1st byte of the fill block.
  refreshPage
END SUB

'CtrlP screenshot key handler.
SUB screenshot
  LOCAL screenshotFileName$ = promptForText$("Screenshot Filename: ")
  promptMsg "Saving...", 1
  SAVE IMAGE screenshotFileName$
  promptMsg "Screenshot saved.", 1
END SUB

'A modification is requested somewhere inside the hex table. The new nibble value is passed in as argument.
SUB editTable(nibble%)
  'Make the change
  LOCAL elem$ = readByte$(crsrAddress%)
  IF elem$ <> "" THEN
    LOCAL oldVal% = ASC(elem$)
    LOCAL newVal%
    IF crsrOnLeftNibble% <> 0 THEN
      newVal% = nibble%*16 OR (oldVal% AND 15)
    ELSE
      newVal% = (oldVal% AND &HF0) OR nibble%
    ENDIF

    writeByte crsrAddress%, CHR$(newVal%)
    refreshRow rowStartAddr%(crsrAddress%), crsrRow%, 0
  ENDIF
  cursorRight 1 '1 indicates stay in block.
END SUB

'A modification is requested somewhere in the ASCII block.
SUB editASCblock(char$)
  writeByte crsrAddress%, char$
  refreshRow rowStartAddr%(crsrAddress%), crsrRow%, 0
  cursorRight 1 '1 indicates stay in block.
END SUB

'Check for key presses
SUB checkKey
  LOCAL pressedKey$ = INKEY$

  IF pressedKey$ <> "" THEN
    'Remove any messages on the prompt line
    promptMsg "", 0

    SELECT CASE ASC(pressedKey$)
      CASE 128 'Up Arrow
        cursorUp
      CASE 129 'Down Arrow
        cursorDown
      CASE 130 'Left Arrow
        cursorLeft
      CASE 131 'Right Arrow
        '0 indicates don't stay in block
        cursorRight 0
      CASE 134 'home
        homeKeyHandler
      CASE 135 'End
        endKeyHandler 
      CASE 136 'Page Up
        pageUp
      CASE 137 'Page Down
        pageDown
      CASE 145 'F1
        helpKeyHandler
      CASE 17 'ctrlQ
        quitKeyHandler
      CASE 5 'ctrlE
        exportKeyHandler
      CASE 15
        paasei
      CASE 6 'ctrlF
        fillKeyHandler
      CASE 7 'ctrlG
        gotoKeyHandler
      CASE 16 'CtrlP
        screenshot
      CASE 20 'CtrlT
        toggleKeyHandler
      CASE ELSE
        'This is for the non-ctrl keys, i.e. the edits.
        IF cursorIsInTable%() <> 0 THEN
          LOCAL ucaseKey% = ASC(UCASE$(pressedKey$))
          'Convert characters A-F to values 10-15.
          IF ((ucaseKey%>=ASC("A")) AND (ucaseKey%<=ASC("F"))) THEN
            editTable ucaseKey% - ASC("A") + 10
          'Convert characters 0-9 to values.
          ELSEIF ((ucaseKey%>=ASC("0")) AND (ucaseKey%<=ASC("9"))) THEN
            editTable ucaseKey% - ASC("0")
          ENDIF
        ELSE 'ASC block
          editASCblock pressedKey$
        ENDIF
    END SELECT
  ENDIF
END SUB
