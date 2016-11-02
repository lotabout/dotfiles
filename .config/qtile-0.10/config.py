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
from urllib.request import urlopen
import re
import json


mod = "mod1"
script_path = os.path.dirname(os.path.realpath(__file__))
path_window_switcher = os.path.join(script_path, './utils/dmenu-qtile-windowlist.py')

keys = [
    # Switch between windows in current stack pane
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
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

    Key([mod], "f", lazy.window.toggle_floating()),
    Key([mod, "shift"], "f", lazy.window.toggle_fullscreen()),

    # increase ratio
    Key([mod], "Right", lazy.layout.increase_ratio()),
    Key([mod], "Left", lazy.layout.decrease_ratio()),

    Key([mod, "shift"], "Return", lazy.spawn("urxvt")),

    # Toggle between different layouts as defined below
    #Key([mod], "space", lazy.next_layout()),
    Key([mod], "F4", lazy.window.kill()),

    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "q", lazy.shutdown()),
    Key([mod], "p", lazy.spawncmd()),

    # Utilities

    Key([mod], "BackSpace", lazy.spawn('amixer -q -D pulse sset Master toggle')),
    Key([mod], "equal", lazy.spawn('amixer -q sset Master 5%+ unmute')),
    Key([mod], "minus", lazy.spawn('amixer -q sset Master 5%- unmute')),
    Key([mod, "control"], "Return", lazy.spawn("xscreensaver-command -lock")),
    Key([mod], "b", lazy.hide_show_bar()),
    Key([mod], "e", lazy.spawn(path_window_switcher)),
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


#=============================================================================
# Local configuration file

config_file = 'config.ini'
g_config = {}

if os.path.exists(os.path.join(script_path, config_file)):
    import configparser
    g_config = configparser.ConfigParser()
    g_config.read(os.path.join(script_path, config_file))

#=============================================================================
# Custom widgets

#-----------------------------------------------------------------------------
class DGroupBox(widget.GroupBox):
    """GroupBox widget that mimic i3 wm's tag which will hide empty groups"""
    def __init__(self, **config):
        super(DGroupBox, self).__init__(**config)

    @property
    def groups(self):
        return [g for g in super(DGroupBox, self).groups if g.windows or self.qtile.currentGroup.name == g.name]

#-----------------------------------------------------------------------------
# Disk IO

class DiskIO(widget.Net):
    """Displays disk IO down and up speed"""
    defaults = [
        ('interface', 'sda', 'The interface to monitor'),
        ('update_interval', 3, 'The update interval.'),
    ]

    columns_disk = ['m', 'mm', 'dev', 'reads', 'rd_mrg', 'rd_sectors',
                    'ms_reading', 'writes', 'wr_mrg', 'wr_sectors',
                    'ms_writing', 'cur_ios', 'ms_doing_io', 'ms_weighted']

    def __init__(self, **config):
        # fetch sector size
        try:
            with open(os.path.join('/sys/block/', self.interface, 'queue/hw_sector_size')) as f:
                self.sector_size = int(f.read().strip())
        except:
            self.sector_size = 512

        super(DiskIO, self).__init__(**config)
        self.add_defaults(DiskIO.defaults)


    def get_stats(self):
        lines = []  # type: List[str]
        with open('/proc/diskstats', 'r') as f:
            lines = f.readlines()
        interfaces = {}
        for s in lines:
            int_s = s.split()
            name = int_s[2]
            down = float(int_s[5])*self.sector_size
            up = float(int_s[9])*self.sector_size
            interfaces[name] = {'down': down, 'up': up}
        return interfaces

#-----------------------------------------------------------------------------
# Fetch stock price
def format_stock(stock):
    return '%s: %6.2f, %.2f%%' % (stock['symbol'], stock['price'], stock['percent']*100)

class StockBox(widget.base.ThreadedPollText):
    """Fetch stock price, data from 'http://quotes.money.163.com/stock'
       Stock code: SH, add prefix '0', SZ, add prefix '1'"""
    defaults = [
        ('format_func', format_stock, 'Format Function'),
        ('update_interval', 3, 'The update interval.'),
        ('foreground', '#AAAAAA', 'Foreground  color.'),
    ]

    def __init__(self, stocks=['0000001', '1399001'], **config):
        super(StockBox, self).__init__(**config)
        self.add_defaults(StockBox.defaults)
        self.stocks = stocks

    def _retrieve_stock_data(self):
        url = 'http://api.money.126.net/data/feed/' + ','.join(self.stocks) + ',money.api'
        response = urlopen(url).read()
        return json.loads(re.sub(r'_ntes_quote_callback\((.*)\);', r'\1', response.decode('utf-8')))

    def poll(self):
        texts = []

        try:
            for stock, stock_data in sorted(self._retrieve_stock_data().items()):
                texts.append(self.format_func(stock_data))
        except:
            pass

        return '|'.join(texts)

#=============================================================================

# read stock list from config file
# Example Config File:
# [stocks]
# codes = ["0000001", "1399001"]
stocks = ['0000001'] if 'stocks' not in g_config else json.loads(g_config.get('stocks', 'codes'))

screens = [
    Screen(
        top=bar.Bar([
            widget.TaskList(foreground="#AAAAAA", highlight_method="block",
                fontsize=12, rounded=False, margin_y=0, max_title_width=400)
            ], 20),
        bottom=bar.Bar(
            [
                DGroupBox(highlight_method="block"),
                widget.Sep(),
                widget.Prompt(),
                widget.Spacer(),
                widget.Sep(),
                StockBox(stocks),
                widget.Sep(),
                widget.TextBox('Net:'),
                widget.Net(interface='eth0', update_interval=3),
                widget.Sep(),
                widget.TextBox('Disk:'),
                DiskIO(interface='sda'),
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
                widget.Clock(format='%a, %b %d, %H:%M'),
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
