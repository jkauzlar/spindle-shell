# Spindle Shell

A language and run-time used for running one-off commands against various resource types in a shared environment. 

Spindle is intended to simplify support and diagnostic tasks by providing quick command-line access to databases 
and messaging systems. Most command line shells are syntactically optimized for file system and process interaction, as
well as text manipulation. Non-text data is not well-supported, particularly by pipes, which view the stdout and stdin 
data as lines of text rather than as structured (or binary) data. Microsoft's Powershell solves this, but verbosely and
not platform-independently.

