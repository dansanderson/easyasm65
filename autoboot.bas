10 v$="*** easyasm v0.1, by dddaaannn ***"
10 print chr$(13);v$;chr$(13);chr$(13);"loading...";
20 bload "easyasm-e",p($1e00)
30 bload "easyasm",p($8700000)
40 key 15,chr$(141)+"sys $1e00 : rem "+chr$(13)
50 print "done";chr$(13);chr$(13);"easyasm is now in memory. press <help> for menu.";chr$(13)
60 print "to switch to edit mode:"
60 print "  edit on";chr$(13)
70 new
