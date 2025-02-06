// Comment example 1.

// In next line, the two strings "/*" and "*/" are regular characters within the comment
// start of comment, /* still in comment */ and still in comment which ends with newline

/* line 1 of example 2, a single comment 4 lines long.
line 2 of the comment
// line 3 of the comment, the "//" at start of this line are just regular characters
// line 4 of the comment, this 4 line comment ends with these two characters -->*/

/* L1 Comment example 3, shows you cannot nest or mix comment styles.
/* L2 Note the declaration of the storage FOO on L6, is outside of the comment.
/* L3 Note the first two characters on L6 do NOT start a nested comment.
/* L4 However, the two chars '*' and '/' following the line number L6, terminate
/* L5 the comment started on L1.
/* L6 */ var FOO : integer = 1; // The declaration of FOO is not within any comment */

/* L7 The last two characters on line L6 have no special meaning, */
/* L8 they are just characters within the comment that started with the "//". */
