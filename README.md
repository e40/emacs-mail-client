# emacs-mail-client

This repo contains my hacks to read email in GNU Emacs.

The first question is: why?  The short answer: I'm much more
productive with this system than anything I've seen other people use.
That doesn't mean there isn't something out there more powerful, but I
just haven't seen it.

The long answer:

Back in the 90's I was using GNU Emacs + MH-E and I needed a way to
manage the volume of email I was getting.  Chris Richardson and I
cooked up a filtering system based on Perl.  It worked by sitting in
front of **inc** (the MH way of reading new mail) that would sort new
mail into different *inboxes*.  The rules were written in Perl and
were fairly concise.

I used the Perl-based filter for some time.  Then later in the 90's
Ahmon Dancy wrote a Common Lisp version of the Perl script, and
**mailfilter** was born.  The Common Lisp version features rules in
Common Lisp, obviously.  Look at *mailfilter.cl* in the repo to see
what the rules look like.  This is the actual rules I use for my
personal email.  The ones for work are a lot more complex, but contain
things which I don't want to make public.

I used the system in this state for another 15 years, when I really
needed something better to manage the large number of balls I was
keeping in the air.  The problem: my **+inbox** tended to grow large
over short periods of time and I'd lose track of all the messages in
my various other inboxes.  Not only that, I would have to periodically
spend several hours cleaning up my **+inbox**.  It was really painful.

The first thing I did was write an Emacs mode for looking at the
inboxes.  Instead of looking at my **+inbox**, I could look at a
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

Notice I said *could* above.  The problem was, I didn't make it the
default mode.  It was a big step moving to making the *inboxes mode*
the default, but it was an important one.

Once I started using the *inboxes mode*, I ran into another problem:
it was hard to move messages to various inboxes because new email on
those conversations kept coming back to my **+inbox**.  So, I wrote
support to *move conversations* to a folder so that new emails would
automatically show up there.

Lastly, some time before this I had written an MH-E hack that would
allow me to highlight messages.  I could mark them as *important* and
they would be highlighted in yellow in whatever folder they were in.
I used nmh sequences for this, but, unfortunately, when a message is
refiled from one folder to another, the sequence information is lost.
The *nmh-workers* mailing list was helpful in finding a solution:
a *refile hook* to make sure the newly moved message was in the same
sequence.

## Dependencies

First, this doesn't work on Windows.  I've tested it on many versions
of Fedora up to version 17.  It will very likely work on CentOS, RHEL,
Ubuntu, and many other flavors of UNIX.  The software originally ran
on Solaris.

This repo depends on these parts:

 * [nmh](http://www.nongnu.org/nmh/) 1.5 :: this is usually provided by the operating system
 * [MH-E](http://mh-e.sourceforge.net/) 8.5 :: the Emacs interface to MH/nmh
 * [mailfilter](https://github.com/franzinc/mailfilter) 2.x :: the mail filter that sits on top of MH/nmh
 * [Allegro Common Lisp](http://www.franz.com) :: because *mailfilter*
   is written in Common Lisp, you need a Common Lisp.  Another Common
   Lisp might do, but I've never tested anything but ACL.  I will be
   providing binaries for *mailfilter* at some point in the future.

The rest is included in this repo.

## Installation

### MH/nmh

I won't talk about nmh installation.  Get that installed and come back
here.

### $HOME/.mh_profile

You need these additions to your `~/.mh_profile`:

    rmmproc: /home/layer/src/emacs/emacs-mail-client/nmh-rmmproc.sh
    send: -msgid
    #: so messages in the `important' sequence stay in it after being moved
    ref-hook: /home/layer/src/emacs/emacs-mail-client/nmh-ref-hook.sh

Of course, use the actual path to your repo.

### $HOME/.mailfilter.cl

Create your own from what's in this repo.  You can see what I have in
mine.  Start out simple and grow it.

### GNU Emacs

First, you need to load `dkl-boot.el`.  This contains a bunch of
functions which are later used.  This is how I load it from my
`~/.emacs`:

    (require 'bytecomp)
    ;; Must load this first because it contains various support functions
    (let* ((el (format "%semacs-mail-client/dkl-boot.el"
                       (file-name-directory load-file-name)))
           (elc (byte-compile-dest-file el)))
      (cond ((file-newer-than-file-p elc el) (load elc))
            (t (byte-compile-file el t))))

Then, load `dkl-mh-e.el` from this repo.  I do it this way:

    (load (format "%s/emacs-mail-client/dkl-mh-e.el" *my-elib*))

`*my-elib*` is defined to point to `~/src/emacs/`.  You need to define
keybindings for the entry point functions, and this is what I do:

    ;; visit a specific inbox:
    (define-key ctl-x-map "a" 'dkl:mh-scan-inboxes)

    ;; compose an email:
    (define-key ctl-x-map "c" 'mh-smail)

    ;; read email:
    (define-key ctl-x-map "i" 'dkl:mh-rmail)

    ;; visit a specific MH/nmh folder:
    ;;  (I call it with an argument so it asks which MH folder to visit)
    (define-key ctl-x-map "s" (lambda ()
                                (interactive)
                                (mh-rmail 1))))

    ;; Move conversations with a single binding.  Like
    ;; mh-thread-refile.
    (define-key mh-folder-mode-map "\e^" 'dkl:mh-move-conversation)

    ;; Mark messages importand/unimportant:
    (define-key mh-folder-mode-map "*" 'my-mark-important)
    (define-key mh-folder-mode-map "_" 'my-mark-unimportant)

The above are the basic key bindings.  

In summary:

| Keybinding | Action                   | Comments                       |
|:----------:|:-------------------------|:-------------------------------|
|`C-x a`     | visit one of the inboxes |folder read from minibuffer     |
|`C-x c`     | compose a message        |creates a new draft             |
|`C-x i`     | show `*Inboxes*` buffer  |w/prefix arg, inc to +inbox     |
|`C-x s`     | visit a folder           |folder read from minibuffer     |
|`ESC ^`     | move conversation        |folder read from minibuffer     |
|`*`         | mark message important   |                                |
|`_`         | mark message unimportant |                                |

*Inboxes* are defined to be folders named with the prefix `inbox-`.
You can create an inbox by just refiling a message to it, and creating
it in the process, or having `incfilter` create the message there.
(Will `incfilter` create a folder or does it need to already exist?)

