This is the very core of EMMS.  It provides ways to play a track
using `emms-start', to go through the playlist using the commands
`emms-next' and `emms-previous', to stop the playback using
`emms-stop', and to see what's currently playing using `emms-show'.

But in itself, this core is useless, because it doesn't know how to
play any tracks --- you need players for this.  In fact, it doesn't
even know how to find any tracks to consider playing --- for this,
you need sources.

A sample configuration is offered in emms-setup.el, so you might
just want to use that file.
