Explanation of used regular expressions
=======================================

titles a converted to all lower case before trying to match

[a-z]+[^a-z]+    an incomplete word followed by punctuation and/or white space
[^a-z]*          an optionally incomplete word
ar(ch|g)a?eo     spelling variants: archeo argeo archaeo argaeo
(..|..)          alternatives, usually reordered terms
