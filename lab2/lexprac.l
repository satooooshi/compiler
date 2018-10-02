%{
	#include<stdio.h>
int comments, code, whiteSpace;


char *getstr(const char *str)
{
	char alphabet[]={'a','b','c','d','e'};

	int result=atoi(str);
	
	int charItr=result-65;
	char converted=alphabet[charItr];
	
	printf("im%c\n",converted);
	//optional: implement this function if you need it
	return converted;
}

%}

%x str

%%

<str>{

	hello {
    printf("imhello\n");
  }
  

  [\\065-\\090\\097-\\122]+/\\n {
    printf("imhtml::%s\n",yytext);
  } /*equals (\\([0-9]{3}))+/\\n {
    printf("imhtml::%s\n",yytext);
  }  */

  ((\\\^)[\101-\132\141-\172])+/\\n   {
    //adjuststr();
    printf("imEscape: %s\n",yytext);
  } /* \\\^ means "\^" OCT:\101-\132\141-\172 *string_buf_ptr++ = yytext[2];*/

  \\n {printf("EOL\n",yytext);}

  \"  {
    printf("string ends here\n");
    BEGIN(INITIAL);
  }

[ \t\n] {printf("imInvisible\n");}
. { printf("imAnythingElse\n");}
}
<INITIAL>{
\"   {
    //adjust();
    //string_buf_ptr = string_buf;
	 printf("string starts here\n");
    BEGIN(str);
  } /* Beginning of STRING \" */
}
.	 {/*adjust(); EM_error(EM_tokPos,"illegal token")*/;}
%%
int
main()
{
	yylex();
	return 0;
}


((\\\^)[\x41-\x5A\x61-\x7A])+ {
    adjuststr();
    printf("chtocc:%u",chtocc('C'));
   *string_buf_ptr++ = '\x03';
   *string_buf_ptr++ = '\x0F';
   *string_buf_ptr++ = '\x0D';
   *string_buf_ptr++ = '\x10';
   *string_buf_ptr++ = '\x09';
   *string_buf_ptr++ = '\x0C';
   *string_buf_ptr++ = '\x05';
   *string_buf_ptr++ = '\x12';
   *string_buf_ptr++ = '\x13';
  } /* \\\^ means "\^", OCT:\101-\132\141-\172, Hex:\x41-\x5A\x61-\x7A means A~Za~z */

  