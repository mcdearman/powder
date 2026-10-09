-- Haskell 2010 Report 6.1.2 (and 2.6): ASCII mnemonic escapes (\NUL .. \DEL), control escapes (\^A), \SO\&H vs \SOH
allNames :: String
allNames = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\BS\HT\LF\VT\FF\CR\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US\SP\DEL"

main :: IO ()
main = do
  print ("all names", map fromEnum allNames)
  print ("all names ok", map fromEnum allNames == [0 .. 32] ++ [127])
  print ("count", length allNames)
  print ("SOH is one char", map fromEnum "\SOH", length "\SOH")
  print ("SO then H", map fromEnum "\SO\&H", length "\SO\&H")
  print ("SO then space H", map fromEnum "\SO H")
  print ("SOH vs SO H", "\SOH" == "\SO\&H", "\SOH" < "\SO\&H")
  print ("name then letter", map fromEnum "\DELa\ESCb\NULc")
  print ("name then digit", map fromEnum "\DEL1\EM2")
  print ("control chars", map fromEnum ['\^@', '\^A', '\^B', '\^I', '\^J', '\^M', '\^Z'])
  print ("control punctuation", map fromEnum ['\^[', '\^\', '\^]', '\^^', '\^_'])
  print ("control string", map fromEnum "\^A\^B\^C\^Z")
  print ("control equals name", '\^A' == '\SOH', '\^G' == '\BEL', '\^[' == '\ESC', '\^@' == '\NUL')
  print ("control equals simple", '\^H' == '\b', '\^I' == '\t', '\^J' == '\n', '\^M' == '\r')
  print ("char literals", fromEnum '\NUL', fromEnum '\SOH', fromEnum '\SO', fromEnum '\DEL')
