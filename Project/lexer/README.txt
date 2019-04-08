To deal with nested comments using <COMMENT> state and for specific handling of string 
using <STRING> state. By counting nested depth we implement nested commenting.

The string is read with the help of a buffer.

At EOF we check for two cases, an unclosed comment and an unclosed string. 

We also reset some references (such as line number, comment depth) in the
EOF function so that if we parse additional files the values will be correct 
and will not carry previous state.
