import Data.Char
data TextEditor = TextEditor String String String String deriving (Show)
--initialises text editor
initialise :: TextEditor -> TextEditor
initialise (TextEditor b s a m) =
 TextEditor "Text Editor Initialised" [] [' '] []
 
--inserts character where cursor is
insertCharacter :: TextEditor -> Char -> TextEditor
insertCharacter (TextEditor b s a m) characterSelected = (TextEditor (b++[characterSelected]) s a m)

--selects everything in the string
selectAll :: TextEditor -> TextEditor
selectAll (TextEditor b s a m) = (TextEditor [] (b++a) [] m)

--moves cursor right one space
moveCursorRight :: TextEditor -> TextEditor
moveCursorRight (TextEditor b s a m) =
 (TextEditor (b++[head a]) [] (tail a) m)
 
--moves cursor left one space
moveCursorLeft :: TextEditor -> TextEditor
moveCursorLeft (TextEditor b s a m) =
 (TextEditor (reverse(tail(reverse b))) [] ([(head (reverse b))]++a) m)
 
--moves cursor to the word on the right 
moveCursorRightWord :: TextEditor -> TextEditor
moveCursorRightWord (TextEditor [] s a m) = (TextEditor [] s a m)
moveCursorRightWord (TextEditor b s a m) =
 if (((head a) /= ' ') && (head(reverse(b++s)) == ' '))
then (TextEditor b s a m)
else moveCursorRightWord(moveCursorRight(TextEditor b s a m))

--moves cursor to the word on the left
moveCursorLeftWord :: TextEditor -> TextEditor
moveCursorLeftWord (TextEditor [] s a m) = (TextEditor [] s a m)
moveCursorLeftWord (TextEditor b s a m) =
 if (head(reverse b) == ' ' && head(s++a) /= ' ')
then (TextEditor b s a m)
else moveCursorLeftWord(moveCursorLeft(TextEditor b s a m))

--moves cursor to beginning of the string
moveCursorToLineBegin :: TextEditor -> TextEditor
moveCursorToLineBegin (TextEditor b s a m) = (TextEditor [] [] (b++s++a) m)

--moves cursor to end of the line
moveCursorToLineEnd :: TextEditor -> TextEditor
moveCursorToLineEnd (TextEditor b s a m) = (TextEditor (b++s++a) [] [] m)

--selects character to the left
selectCharacterLeft :: TextEditor -> TextEditor
selectCharacterLeft (TextEditor [] s a m) = (TextEditor [] s a m)
selectCharacterLeft (TextEditor b s a m) = (TextEditor (reverse(tail(reverse b))) ([head(reverse b)]++s) a m)

-- selects character to the right
selectCharacterRight :: TextEditor -> TextEditor
selectCharacterRight (TextEditor b s a m) = (TextEditor b (s++[head a]) (tail a) m)

--selects word to the right
selectWordRight :: TextEditor -> TextEditor
selectWordRight (TextEditor b s [] m) = (TextEditor b s [] m)
selectWordRight (TextEditor b s a m) =
 if(head(a) == ' ')
 then (TextEditor b s a m)
else selectWordRight(selectCharacterRight(TextEditor b s a m))

--selects word on the left
selectWordLeft :: TextEditor -> TextEditor
selectWordLeft (TextEditor [] s a m) = (TextEditor [] s a m)
selectWordLeft (TextEditor b s a m) =
 if((head(reverse b) == ' ') && (head(s++a) /= ' '))
 then (TextEditor b s a m)
else selectWordLeft(selectCharacterLeft(TextEditor b s a m))

--selects word at beginning of the line
selectLineBegin :: TextEditor -> TextEditor
selectLineBegin (TextEditor b s a m) = (TextEditor [] (b++s) a m)

-- selects word at end of the line
selectLineEnd :: TextEditor -> TextEditor
selectLineEnd (TextEditor b s a m) = (TextEditor b (s++a) [] m)

--backspace where cursor is / delete to left
backspaceCharacter :: TextEditor -> TextEditor
backspaceCharacter (TextEditor b s a m) =
 (TextEditor (reverse(tail (reverse b))) [] a m)

--deletes character where cursor is / delete to right 
deleteCharacter :: TextEditor -> TextEditor
deleteCharacter(TextEditor b s a m) =
 if s == []
 then (TextEditor b [] (tail a) m )
 else (TextEditor b [] a m)
 
--cut selection
cut :: TextEditor -> TextEditor
cut (TextEditor b s a m) = (TextEditor b [] a s)

--copy selection
copy :: TextEditor -> TextEditor
copy (TextEditor b s a m) = (TextEditor b s a s)

--paste selection
paste :: TextEditor -> TextEditor
paste (TextEditor b s a m) = (TextEditor (b++m) s a [])