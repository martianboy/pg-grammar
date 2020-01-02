%option reentrant
%option bison-bridge
%option bison-locations
%option never-interactive
%option nodefault
%option noinput
%option nounput
%option noyywrap
%option noyyalloc
%option noyyrealloc
%option noyyfree
%option warn
%option prefix="core_yy"

%{

const _ = require('./src');

/*
 * GUC variables.  This is a DIRECT violation of the warning given at the
 * head of gram.y, ie flex/bison code must not depend on any GUC variables;
 * as such, changing their values can induce very unintuitive behavior.
 * But we shall have to live with it until we can remove these variables.
 */
const		backslash_quote = 2; // BACKSLASH_QUOTE_SAFE_ENCODING
const		escape_string_warning = true;
const		standard_conforming_strings = true;

function process_integer_literal(token, lval) {
	let val = parseInt(token);

	if (val.toString().length < token.length) {
		lval.str = token;
		return 'FCONST';
	}

	lval.ival = val;
	return 'ICONST';
}

%}

/*
 * OK, here is a short description of lex/flex rules behavior.
 * The longest pattern which matches an input string is always chosen.
 * For equal-length patterns, the first occurring in the rules list is chosen.
 * INITIAL is the starting state, to which all non-conditional rules apply.
 * Exclusive states change parsing rules while the state is active.  When in
 * an exclusive state, only those rules defined for that state apply.
 *
 * We use exclusive states for quoted strings, extended comments,
 * and to eliminate parsing troubles for numeric strings.
 * Exclusive states:
 *  <xb> bit string literal
 *  <xc> extended C-style comments
 *  <xd> delimited identifiers (double-quoted identifiers)
 *  <xh> hexadecimal numeric string
 *  <xq> standard quoted strings
 *  <xe> extended quoted strings (support backslash escape sequences)
 *  <xdolq> $foo$ quoted strings
 *  <xui> quoted identifier with Unicode escapes
 *  <xuiend> end of a quoted identifier with Unicode escapes, UESCAPE can follow
 *  <xus> quoted string with Unicode escapes
 *  <xusend> end of a quoted string with Unicode escapes, UESCAPE can follow
 *  <xeu> Unicode surrogate pair in extended quoted string
 *
 * Remember to add an <<EOF>> case whenever you add a new exclusive state!
 * The default one is probably not the right thing.
 */

%x xb
%x xc
%x xd
%x xh
%x xq
%x xe
%x xdolq
%x xui
%x xuiend
%x xus
%x xusend
%x xeu

/*
 * In order to make the world safe for Windows and Mac clients as well as
 * Unix ones, we accept either \n or \r as a newline.  A DOS-style \r\n
 * sequence will be seen as two successive newlines, but that does not cause
 * any problems.  Comments that start with -- and extend to the next
 * newline are treated as equivalent to a single whitespace character.
 *
 * NOTE a fine point: if there is no newline following --, we will absorb
 * everything to the end of the input as a comment.  This is correct.  Older
 * versions of Postgres failed to recognize -- as a comment if the input
 * did not end with a newline.
 *
 * XXX perhaps \f (formfeed) should be treated as a newline as well?
 *
 * XXX if you change the set of whitespace characters, fix scanner_isspace()
 * to agree.
 */

space			[ \t\n\r\f]
horiz_space		[ \t\f]
newline			[\n\r]
non_newline		[^\n\r]

comment			("--"{non_newline}*)

whitespace		({space}+|{comment})

/*
 * SQL requires at least one newline in the whitespace separating
 * string literals that are to be concatenated.  Silly, but who are we
 * to argue?  Note that {whitespace_with_newline} should not have * after
 * it, whereas {whitespace} should generally have a * after it...
 */

special_whitespace		({space}+|{comment}{newline})
horiz_whitespace		({horiz_space}|{comment})
whitespace_with_newline	({horiz_whitespace}*{newline}{special_whitespace}*)

/*
 * To ensure that {quotecontinue} can be scanned without having to back up
 * if the full pattern isn't matched, we include trailing whitespace in
 * {quotestop}.  This matches all cases where {quotecontinue} fails to match,
 * except for {quote} followed by whitespace and just one "-" (not two,
 * which would start a {comment}).  To cover that we have {quotefail}.
 * The actions for {quotestop} and {quotefail} must throw back characters
 * beyond the quote proper.
 */
quote			\'
quotestop		{quote}{whitespace}*
quotecontinue	{quote}{whitespace_with_newline}{quote}
quotefail		{quote}{whitespace}*"-"

xbstart			[bB]{quote}
xbinside		[^']*

/* Hexadecimal number */
xhstart			[xX]{quote}
xhinside		[^']*

/* National character */
xnstart			[nN]{quote}

/* Quoted string that allows backslash escapes */
xestart			[eE]{quote}
xeinside		[^\\']+
xeescape		[\\][^0-7]
xeoctesc		[\\][0-7]{1,3}
xehexesc		[\\]x[0-9A-Fa-f]{1,2}
xeunicode		[\\]("u"[0-9A-Fa-f]{4}|"U"[0-9A-Fa-f]{8})
xeunicodefail	[\\]("u"[0-9A-Fa-f]{0,3}|"U"[0-9A-Fa-f]{0,7})

xqstart			{quote}
xqdouble		{quote}{quote}
xqinside		[^']+

dolq_start		[A-Za-z\200-\377_]
dolq_cont		[A-Za-z\200-\377_0-9]
dolqdelim		\$({dolq_start}{dolq_cont}*)?\$
dolqfailed		\${dolq_start}{dolq_cont}*
dolqinside		[^$]+

dquote			\"
xdstart			{dquote}
xdstop			{dquote}
xddouble		{dquote}{dquote}
xdinside		[^"]+

uescape			[uU][eE][sS][cC][aA][pP][eE]{whitespace}*{quote}[^']{quote}
uescapefail		[uU][eE][sS][cC][aA][pP][eE]{whitespace}*"-"|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*{quote}[^']|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*{quote}|[uU][eE][sS][cC][aA][pP][eE]{whitespace}*|[uU][eE][sS][cC][aA][pP]|[uU][eE][sS][cC][aA]|[uU][eE][sS][cC]|[uU][eE][sS]|[uU][eE]|[uU]

xuistart		[uU]&{dquote}

xusstart		[uU]&{quote}

xustop1		{uescapefail}?
xustop2		{uescape}

xufailed		[uU]&


xcstart			\/\*{op_chars}*
xcstop			\*+\/
xcinside		[^\*\/]+

digit			[0-9]
ident_start		[A-Za-z\200-\377_]
ident_cont		[A-Za-z\200-\377_0-9\$]

identifier		{ident_start}{ident_cont}*

typecast		"::"
dot_dot			\.\.
colon_equals	":="		/* " */

equals_greater	"=>"
less_equals		"<="
greater_equals	">="
less_greater	"<>"
not_equals		"!="

/*
 * "self" is the set of chars that should be returned as single-character
 * tokens.  "op_chars" is the set of chars that can make up "Op" tokens,
 * which can be one or more characters long (but if a single-char token
 * appears in the "self" set, it is not to be returned as an Op).  Note
 * that the sets overlap, but each has some chars that are not in the other.
 *
 * If you change either set, adjust the character lists appearing in the
 * rule for "operator"!
 */
self			[,()\[\].;\:\+\-\*\/\%\^\<\>\=]
op_chars		[\~\!\@\#\^\&\|\`\?\+\-\*\/\%\<\>\=]
operator		{op_chars}+

integer			{digit}+
decimal			(({digit}*\.{digit}+)|({digit}+\.{digit}*))
decimalfail		{digit}+\.\.
real			({integer}|{decimal})[Ee][-+]?{digit}+
realfail1		({integer}|{decimal})[Ee]
realfail2		({integer}|{decimal})[Ee][-+]

param			\${integer}

other			.

%%

{whitespace}	{
					/* ignore */
				}

{xcstart}		{
					yy.extra.xcdepth = 0;
					this.begin('xc');
					this.less(2);
				}
<xc>{xcstart}	{
					yy.extra.xcdepth++;
					/* Put back any characters past slash-star; see above */
					this.less(2);
				}
<xc>{xcstop}	{
					if (yy.extra.xcdepth <= 0) {
						this.popState();
					} else {
						yy.extra.xcdepth--;
					}
				}

<xc>{xcinside}	{
					/* ignore */
				}

<xc>{op_chars}	{
					/* ignore */
				}

<xc>\*+			{
					/* ignore */
				}

<xc><<EOF>>		{
					throw new Error('unterminated /* comment');
				}

{xqstart}		{
					yy.extra.warn_on_first_escape = true;
					yy.extra.saw_non_ascii = false;
					// SET_YYLLOC();
					if (yy.extra.standard_conforming_strings)
						this.begin('xq');
					else
						this.begin('xe');

					yy.extra.literalbuf = '';
					yy.extra.literallen = 0;
				}
{xestart}		{
					yy.extra.warn_on_first_escape = false;
					yy.extra.saw_non_ascii = false;
					// SET_YYLLOC();
					this.begin('xe');
					yy.extra.literalbuf = '';
					yy.extra.literallen = 0;
				}
{xusstart}		{
					// SET_YYLLOC();
					if (!yy.extra.standard_conforming_strings)
						ereport(ERROR,
								(errcode(ERRCODE_FEATURE_NOT_SUPPORTED),
								 errmsg("unsafe use of string constant with Unicode escapes"),
								 errdetail("String constants with Unicode escapes cannot be used when standard_conforming_strings is off."),
								 lexer_errposition()));
					this.begin('xus');
					yy.extra.literallen = 0;
					yy.extra.literalbuf = '';
				}
<xq,xe>{quotestop} {
					this.less(1);
					this.popState();
					/*
					 * check that the data remains valid if it might have been
					 * made invalid by unescaping any chars.
					 */
					// if (yy.extra.saw_non_ascii)
					// 	pg_verifymbstr(yy.extra.literalbuf,
					// 				   yy.extra.literallen,
					// 				   false);
					yy.lval.str = yy.extra.literalbuf;
					return 'SCONST';
				}
<xq,xe>{quotefail} {
					this.less(1);
					this.popState();
					/*
					 * check that the data remains valid if it might have been
					 * made invalid by unescaping any chars.
					 */
					// if (yy.extra.saw_non_ascii)
					// 	pg_verifymbstr(yy.extra.literalbuf,
					// 				   yy.extra.literallen,
					// 				   false);
					yy.lval.str = yy.extra.literalbuf;
					return 'SCONST';
				}
<xus>{quotestop} {
					/* throw back all but the quote */
					this.less(1);
					/* xusend state looks for possible UESCAPE */
					this.begin('xusend');
				}
<xus>{quotefail} {
					/* throw back all but the quote */
					this.less(1);
					/* xusend state looks for possible UESCAPE */
					this.begin('xusend');
				}
<xusend>{whitespace} {
					/* stay in xusend state over whitespace */
				}

<xq,xe,xus>{xqdouble} {
					yy.extra.literalbuf += "'";
				}
<xq,xus>{xqinside}  {
					yy.extra.literalbuf += yytext;
				}

<xq,xe,xus>{quotecontinue} {
					/* ignore */
				}

<xq,xe,xus><<EOF>>		{ throw new Error("unterminated quoted string"); }

{typecast}		{
					return 'TYPECAST';
				}

{dot_dot}		{
					return 'DOT_DOT';
				}

{colon_equals}	{
					return 'COLON_EQUALS';
				}

{equals_greater} {
					return 'EQUALS_GREATER';
				}

{less_equals}	{
					return 'LESS_EQUALS';
				}

{greater_equals} {
					return 'GREATER_EQUALS';
				}

{less_greater}	{
					/* We accept both "<>" and "!=" as meaning NOT_EQUALS */
					return 'NOT_EQUALS';
				}

{not_equals}	{
					/* We accept both "<>" and "!=" as meaning NOT_EQUALS */
					return 'NOT_EQUALS';
				}

{self}          {
					// SET_YYLLOC();
					return yytext[0];
				}

{operator}		{
					/*
					* Check for embedded slash-star or dash-dash; those
					* are comment starts, so operator must stop there.
					* Note that slash-star or dash-dash at the first
					* character will match a prior rule, not this one.
					*/
					let	nchars = yyleng;
					let slashstar = yytext.indexOf("/*");
					let dashdash = yytext.indexOf("--");

					if (slashstar > -1 && dashdash > -1)
					{
						/* if both appear, take the first one */
						if (slashstar > dashdash)
							slashstar = dashdash;
					}
					else if (slashstar < 0)
						slashstar = dashdash;
					if (slashstar > -1)
						nchars = slashstar;

					/*
					* For SQL compatibility, '+' and '-' cannot be the
					* last char of a multi-char operator unless the operator
					* contains chars that are not in SQL operators.
					* The idea is to lex '=-' as two operators, but not
					* to forbid operator names like '?-' that could not be
					* sequences of SQL operators.
					*/
					if (nchars > 1 &&
						(yytext[nchars - 1] == '+' ||
						yytext[nchars - 1] == '-'))
					{
						let	ic;

						for (ic = nchars - 2; ic >= 0; ic--)
						{
							let c = yytext[ic];
							if (c == '~' || c == '!' || c == '@' ||
								c == '#' || c == '^' || c == '&' ||
								c == '|' || c == '`' || c == '?' ||
								c == '%')
								break;
						}
						if (ic < 0)
						{
							/*
							* didn't find a qualifying character, so remove
							* all trailing [+-]
							*/
							do {
								nchars--;
							} while (nchars > 1 &&
								(yytext[nchars - 1] == '+' ||
								yytext[nchars - 1] == '-'));
						}
					}

					// SET_YYLLOC();

					if (nchars < yyleng)
					{
						/* Strip the unwanted chars from the token */
						this.less(nchars);
						/*
						* If what we have left is only one char, and it's
						* one of the characters matching "self", then
						* return it as a character token the same way
						* that the "self" rule would have.
						*/
						if (nchars == 1 &&
							",()[].;:+-*/%^<>=".includes(yytext[0]))
							return yytext[0];
						/*
						* Likewise, if what we have left is two chars, and
						* those match the tokens ">=", "<=", "=>", "<>" or
						* "!=", then we must return the appropriate token
						* rather than the generic Op.
						*/
						if (nchars == 2)
						{
							if (yytext[0] == '=' && yytext[1] == '>')
								return 'EQUALS_GREATER';
							if (yytext[0] == '>' && yytext[1] == '=')
								return 'GREATER_EQUALS';
							if (yytext[0] == '<' && yytext[1] == '=')
								return 'LESS_EQUALS';
							if (yytext[0] == '<' && yytext[1] == '>')
								return 'NOT_EQUALS';
							if (yytext[0] == '!' && yytext[1] == '=')
								return 'NOT_EQUALS';
						}
					}

					/*
					* Complain if operator is too long.  Unlike the case
					* for identifiers, we make this an error not a notice-
					* and-truncate, because the odds are we are looking at
					* a syntactic mistake anyway.
					*/
					if (nchars >= 64)
						throw new Error("operator too long");

					yy.lval.str = yytext;
					return 'Op';
				}


{param}			{
					return 'PARAM';
				}

{integer}		{
					return process_integer_literal(yytext, yy.lval);
				}
{decimal}		{
					yy.lval.str = pstrdup(yytext);
					return 'FCONST';
				}
{decimalfail}	{
					/* throw back the .., and treat as integer */
					this.less(yyleng - 2);
					return process_integer_literal(yytext, yy.lval);
				}
{real}			{
					yy.lval.str = yytext;
				}

{identifier}	{
					/* Is it a keyword? */
					let kwnum = _.ScanKeywordLookup(yytext);
					if (kwnum >= 0)
					{
						yy.lval.keyword = _.GetScanKeyword(kwnum,
														 yy.extra.keywordlist);
						return yy.lval.keyword;
					}

					/*
					 * No.  Convert the identifier to lower case, and truncate
					 * if necessary.
					 */
					yytext = yytext.toLowerCase().trim()
					yy.lval.str = yytext;
					return 'IDENT';
				}

{other}			{
					// SET_YYLLOC();
					return yytext[0];
				}

<<EOF>>         {
	// return 'EOF'
}
