Genesis: dawn of a new day FULL source code release
---------------------------------------------------

So here it is, the full source code release of the game by Retroworks. Full means:

- All source code is included
- All sprites/screen$ are included
- Full music is included
- Full makefile is included, so you can just type 'make' and get your own TZX

Requirements
------------

You need the following components to get a compiled game:

- z88dk official release 1.6, from www.z88dk.org
- Pasmo assembler, from http://pasmo.speccy.org/
- APACK compressor from http://www.smspower.org/maxim/smssoftware/aplib.html 
  or http://www.ibsensoftware.com/download.html  
- The utoloader TZX builder, from https://sites.google.com/site/oxobits/dowloads/utoloader0.4.zip
- And some kind of make utility (use GNU Make, for example)
- For the DSK version, you will need the TAPTOOLS from http://www.seasip.demon.co.uk/ZX/unix.html

The DSK version is not fully generated with the makefile, some manual work
is required. Just play around and see ;).


Compiling your own version
--------------------------

There are many hardcoded addresses in the code, to overcome the limitations of z88dk and pasmo. 
So you will need to be very careful when modifying something, to avoid breaking something else.
In short, after executing "make", check the symbol/map files and modify the following files
according to the new addresses:

- constants.h  (info from levels.sym, wyzplayer.sym, menu.sym and ram3.sym)
- behavior.c (info from behav.sym)
- engine.h (info from engine.sym)
- maindefs.asm (info from test.map)
- movement.c (info from move.sym)


Warnings, disclaimers and licensing
-----------------------------------

First of all, some licensing words: 

"The world would be a happier place if we all shared our source code"

That said, some more warnings:

- The code is barely readable in some areas. Sorry, I had to make it that way.
- Most of the code is in English with reasonable comments... but there is some
  Spanish here and there. If you do not understand, do not worry, just ask me and
  I will try to help.
- If you have any trouble while playing around with the code, just let me know.
  I will try to help if I have some free time.
  
  
Contacts:
---------

- Coder & main responsible for this mess: utopian@retroworks.es
- Released by: http://www.retroworks.es
