10 v$="*** easyasm v0.1, by dddaaannn ***"
10 print chr$(13);v$;chr$(13);chr$(13);"loading...";
20 bload "easyasm-e",p($1e00)
30 bload "easyasm",p($8700000)
40 ht$=chr$(141)+v$+chr$(141)
50 ht$=ht$+"https://github.com/dansanderson/easyasm65  "+chr$(141)+chr$(141)
50 ht$=ht$+"sys $1e00 : rem: assemble to memory  "+chr$(141)
60 ht$=ht$+"sys $1e03 : rem: assemble to disk  "+chr$(141)
70 ht$=ht$+"sys $1e06 : rem: restore source  "+chr$(141)+chr$(141)
80 key 15,ht$
90 print "done";chr$(13);chr$(13);"easyasm is now in memory. press help for help.";chr$(13)
