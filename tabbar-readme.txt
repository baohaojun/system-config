This library provides the Tabbar global minor mode to display a tab
bar in the header line of Emacs 21 and later versions.  You can use
the mouse to click on a tab and select it.  Also, three buttons are
displayed on the left side of the tab bar in this order: the
"home", "scroll left", and "scroll right" buttons.  The "home"
button is a general purpose button used to change something on the
tab bar.  The scroll left and scroll right buttons are used to
scroll tabs horizontally.  Tabs can be divided up into groups to
maintain several sets of tabs at the same time (see also the
chapter "Core" below for more details on tab grouping).  Only one
group is displayed on the tab bar, and the "home" button, for
example, can be used to navigate through the different groups, to
show different tab bars.

In a graphic environment, using the mouse is probably the preferred
way to work with the tab bar.  However, you can also use the tab
bar when Emacs is running on a terminal, so it is possible to use
commands to press special buttons, or to navigate cyclically
through tabs.

These commands, and default keyboard shortcuts, are provided:

`tabbar-mode'
    Toggle the Tabbar global minor mode.  When enabled a tab bar is
    displayed in the header line.

`tabbar-local-mode'         (C-c <C-f10>)
    Toggle the Tabbar-Local minor mode.  Provided the global minor
    mode is turned on, the tab bar becomes local in the current
    buffer when the local minor mode is enabled.  This permits to
    see the tab bar in a buffer where the header line is already
    used by another mode (like `Info-mode' for example).

`tabbar-mwheel-mode'
    Toggle the Tabbar-Mwheel global minor mode.  When enabled you
    can use the mouse wheel to navigate through tabs of groups.

`tabbar-press-home'         (C-c <C-home>)
`tabbar-press-scroll-left'  (C-c <C-prior>)
`tabbar-press-scroll-right' (C-c <C-next>)
    Simulate a mouse-1 click on respectively the "home", "scroll
    left", and "scroll right" buttons.  A numeric prefix argument
    value of 2, or 3, respectively simulates a mouse-2, or mouse-3
    click.

`tabbar-backward'           (C-c <C-left>)
`tabbar-forward'            (C-c <C-right>)
    Are the basic commands to navigate cyclically through tabs or
    groups of tabs.  The cycle is controlled by the
    `tabbar-cycle-scope' option.  The default is to navigate
    through all tabs across all existing groups of tabs.  You can
    change the default behavior to navigate only through the tabs
    visible on the tab bar, or through groups of tabs only.  Or use
    the more specialized commands below.

`tabbar-backward-tab'
`tabbar-forward-tab'
    Navigate through the tabs visible on the tab bar.

`tabbar-backward-group'     (C-c <C-up>)
`tabbar-forward-group'      (C-c <C-down>)
    Navigate through existing groups of tabs.


Core
----

The content of the tab bar is represented by an internal data
structure: a tab set.  A tab set is a collection (group) of tabs,
identified by an unique name.  In a tab set, at any time, one and
only one tab is designated as selected within the tab set.

A tab is a simple data structure giving the value of the tab, and a
reference to its tab set container.  A tab value can be any Lisp
object.  Each tab object is guaranteed to be unique.

A tab set is displayed on the tab bar through a "view" defined by
the index of the leftmost tab shown.  Thus, it is possible to
scroll the tab bar horizontally by changing the start index of the
tab set view.

The visual representation of a tab bar is a list of valid
`header-line-format' template elements, one for each special
button, and for each tab found into a tab set "view".  When the
visual representation of a tab is required, the function specified
in the variable `tabbar-tab-label-function' is called to obtain it.
The visual representation of a special button is obtained by
calling the function specified in `tabbar-button-label-function',
which is passed a button name among `home', `scroll-left', or
`scroll-right'.  There are also options and faces to customize the
appearance of buttons and tabs (see the code for more details).

When the mouse is over a tab, the function specified in
`tabbar-help-on-tab-function' is called, which is passed the tab
and should return a help string to display.  When a tab is
selected, the function specified in `tabbar-select-tab-function' is
called, which is passed the tab and the event received.

Similarly, to control the behavior of the special buttons, the
following variables are available, for respectively the `home',
`scroll-left' and `scroll-right' value of `<button>':

`tabbar-<button>-function'
   Function called when <button> is selected.  The function is
   passed the mouse event received.

`tabbar-<button>-help-function'
   Function called with no arguments to obtain a help string
   displayed when the mouse is over <button>.

To increase performance, each tab set automatically maintains its
visual representation in a cache.  As far as possible, the cache is
used to display the tab set, and refreshed only when necessary.

Several tab sets can be maintained at the same time.  Only one is
displayed on the tab bar, it is obtained by calling the function
specified in the variable `tabbar-current-tabset-function'.

A special tab set is maintained, that contains the list of the
currently selected tabs in the existing tab sets.  This tab set is
useful to show the existing tab sets in a tab bar, and switch
between them easily.  The function `tabbar-get-tabsets-tabset'
returns this special tab set.


Buffer tabs
-----------

The default tab bar implementation provided displays buffers in
dedicated tabs.  Selecting a tab, switch (mouse-1), or pop
(mouse-2), to the buffer it contains.

The list of buffers put in tabs is provided by the function
specified in the variable `tabbar-buffer-list-function'.  The
default function: `tabbar-buffer-list', excludes buffers whose name
starts with a space, when they are not visiting a file.

Buffers are organized in groups, each one represented by a tab set.
A buffer can have no group, or belong to more than one group.  The
function specified by the variable `tabbar-buffer-groups-function'
is called for each buffer to obtain the groups it belongs to.  The
default function provided: `tabbar-buffer-groups' organizes buffers
depending on their major mode (see that function for details).

The "home" button toggles display of buffer groups on the tab bar,
allowing to easily show another buffer group by clicking on the
associated tab.

Known problems:

Bug item #858306 at <http://sf.net/tracker/?group_id=79309>:
tabbar-mode crashes GNU Emacs 21.3 on MS-Windows 98/95.
