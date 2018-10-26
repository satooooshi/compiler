% yacc -d ch3-01.y # makes y.tab.c and "y.tab.h % lex ch3-01.l # makes lex.yy.c
% cc -o ch3-01 y.tab.c lex.yy.c -ly -ll % ch3-01
