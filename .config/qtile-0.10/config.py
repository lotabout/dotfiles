# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
import os
import subprocess

mod = "mod1"

keys = [
    # Switch between windows in current stack pane
    Key([mod], "k", lazy.layout.down()),
    Key([mod], "j", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),

    # Move windows up or down in current stack
    Key([mod, "shift"], "k", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),

    # increase/decrease
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),

    Key([mod], "n", lazy.layout.normalize()),

    # Toggle between split and unsplit sides of stack.
    Key([mod], "space", lazy.layout.toggle_split()),

    # next layout
    Key([mod], "s", lazy.next_layout()),

    # increase ratio
    Key([mod], "Right", lazy.layout.increase_ratio()),
    Key([mod], "Left", lazy.layout.decrease_ratio()),

    Key([mod, "shift"], "Return", lazy.spawn("xterm")),

    # Toggle between different layouts as defined below
    #Key([mod], "space", lazy.next_layout()),
    Key([mod], "F4", lazy.window.kill()),

    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "q", lazy.shutdown()),
    Key([mod], "p", lazy.spawncmd()),

    # Utiflities

    Key([mod], "BackSpace", lazy.spawn('amixer -q -D pulse sset Master toggle')),
    Key([mod], "equal", lazy.spawn('amixer -q sset Master 5%+ unmute')),
    Key([mod], "minus", lazy.spawn('amixer -q sset Master 5%- unmute')),
    Key([mod, "control"], "Return", lazy.spawn("xscreensaver-command -lock")),
]


groups = [Group(str(i)) for i in range(1,10)]

for idx, grp in enumerate(groups):
    # mod1 + letter of grp = switch to grp
    keys.append(
        Key([mod], str(idx+1), lazy.screen.togglegroup(str(idx+1)))
    )

    # mod1 + shift + letter of grp = switch to & move focused window to grp
    keys.append(
        Key([mod, "shift"], str(idx+1), lazy.window.togroup(str(idx+1)))
    )

layouts = [
    layout.Max(),
    layout.Columns(),
]

widget_defaults = dict(
    font='Dejavu Sans Mono',
    fontsize=14,
    padding=1,
)

#-----------------------------------------------------------------------------
# Custom widgets

class DGroupBox(widget.GroupBox):
    """GroupBox widget that mimic i3 wm's tag which will hide empty groups"""
    def __init__(self, **config):
        super(DGroupBox, self).__init__(**config)

    @property
    def groups(self):
        return [g for g in super(DGroupBox, self).groups if g.windows or self.qtile.currentGroup.name == g.name]

screens = [
    Screen(
        bottom=bar.Bar(
            [
                DGroupBox(highlight_method="block"),
                widget.Sep(),
                widget.Prompt(),
                widget.Sep(),
                widget.TaskList(foreground="#AAAAAA", highlight_method="block"),
                widget.Sep(),
                widget.Net(interface='eth0'),
                widget.Sep(),
                widget.TextBox('CPU:'),
                widget.CPUGraph(),
                widget.TextBox('MEM:'),
                widget.MemoryGraph(),
                widget.Sep(),
                widget.TextBox('VOL:'),
                widget.Volume(),
                widget.Sep(),
                widget.Systray(),
                widget.Sep(),
                widget.Clock(format='%b %d, %Y %H:%M'),
            ],
            22,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating()
auto_fullscreen = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, github issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"

# Autostart script
@hook.subscribe.startup_once
def autostart():
    """call autostart scripts"""
    home = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([home])
