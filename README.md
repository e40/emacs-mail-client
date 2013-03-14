# emacs-mail-client

This repo contains my hacks to read email in GNU Emacs.

The first question is: why?  The short answer: I'm much more
productive with this system than anything I've seen other people use.

The long answer:

Back in the 90's I was using GNU Emacs + MH-E and I needed a way to
manage the volume of email I was getting.  Chris Richardson and I
cooked up a filtering system based on Perl.  It worked by sitting in
front of **inc** (the MH way of reading new mail) that would sort new
mail into different *inboxes*.  The rules were written in Perl and
were fairly concise.

I used the Perl-based filter for some time.  Then later in the 90's I
had a promising young programming working for me and, being at a
Common Lisp company, I wanted a CL version of the Perl filter.  So,
Ahmon Dancy wrote the CL version of the Perl script, and
**mailfilter** was born.  The CL version uses rules in Common Lisp,
obviously, and that was nice for me because it had the power of CL
(macros and more).

I used the system in this state for another 15 years, when I really
needed something better to manage the large number of balls I was
keeping in the air.  The problem was: my **+inbox** tended to grow
large over time and I'd lose track of all the messages in my various
other inboxes.

The first thing I did was write an Emacs mode for looking at the
inboxes.  Instead of looking at my **inbox**, I could look at a
summary of my inboxes:

    +inbox            1 new                 170 old  
    +inbox-gmail                34 unread    35 old  
    +inbox-spam       1 new    233 unread   237 old  
    +inbox-junk       1 new    151 unread   151 old  

    ; *********** TODO inboxes ********************
    +inbox-q                                 35 old  
    +inbox-fin                               41 old  
    +inbox-todo                              37 old  
    +inbox-sa                                47 old  
    +inbox-pbx                               11 old  
    +inbox-someday                           77 old  
    +inbox-pend                               1 old  

    ; *********** Hobby inboxes********************
    +inbox-music                             26 old  
    +inbox-photo                             26 old  
    +inbox-fun                               21 old  
    +inbox-travel                             1 old  

    ; *********** Mailing list inboxes ************
    +inbox-emacs      1 new    298 unread   307 old  
    +inbox-git                 178 unread   178 old  
    +inbox-blink                10 unread    10 old  
    +inbox-cygwin              104 unread   131 old  
    +inbox-mh                                 2 old  
    +inbox-nmh                                4 old  
    +inbox-ps                    1 unread     1 old  

A single key would *go into* a folder and I could process those new,
unread or old emails.

The new problem: it was hard to move messages to various inboxes
because new email on those conversations kept coming back to my
**+inbox**.   So, I wrote support to *move conversations* to a folder
so that new emails would automatically show up there.

Lastly, some time before this I had written an MH-E hack that would
allow me to highlight messages.  I could mark them as *important* and
they would be highlighted in yellow in whatever folder they were in.
I used nmh sequences for this, but, unfortunately, when a message was
refiled from one folder to another, the sequence information was lost.
The *nmh-workers* mailing list to the rescue, and I was able to write
a *refile hook* to take care of that problem.

## Dependencies

This repo depends on these parts:

 * nmh 1.5
 * MH-E 8.5
 * mailfilter 2.x

The rest is included in this repo.

TODO:
 * .mh_profile additions for ref-hook, rmmproc, send (add -msgid)
