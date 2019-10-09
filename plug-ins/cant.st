/**
 * Name: Cant
 * Description: Cant programming language. Derived from scheme.st which is by Markku Rossi <mtr@iki.fi>
 */

state cant extends HighlightEntry
{
  BEGIN {
    /*
     * Modify regexp character syntax so that we can distinguish all
     * symbols.
     */
    extras = list ('!', '$', '%', '&', '*', '/', ':', '<',
		   '=', '>', '?', '~', '^', '.', '+', '-');
    for (i = 0; i < length (extras); i = i + 1)
      regexp_syntax (extras[i], 'w');
  }

  /* Comments. */
  /;/ {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Keywords.
   */
  /@|:|->|~|_|\b(and|begin|do|else|if|let|or|quasiquote|quote|unquote|unquote-splicing|optional|view|be|call|export|for|given|hide|hm|import|make|may|make-trait|on|to|unless|when)\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }
}


/*
Local variables:
mode: c
End:
*/
