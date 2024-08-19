10 v$="easyasm v0.1"
20 print chr$(13);chr$(172);chr$(172);chr$(172);chr$(32);v$;
30 print ", by dddaaannn";chr$(32);chr$(187);chr$(187);chr$(187);chr$(13)
40 print "loading...";
50 bload "easyasm-e",p($1e00)
60 bload "easyasm",p($8700000)
70 key 15,chr$(141)+"sys $1e00 : rem "+chr$(13)
80 print "done";chr$(13);chr$(13);"easyasm is now in memory. press <help> for menu."
90 print chr$(17);chr$(17);chr$(17);"edit on";chr$(145);chr$(145);chr$(145)
100 sys $ff4a,13 : rem enqueue a return key
110 new
