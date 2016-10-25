#!/usr/bin/env python3

# Original version: https://github.com/qtile/qtile-examples/blob/master/zordsdavini/bin/dmenu-qtile-windowlist.py

from libqtile.command import Client
import subprocess
import re

# connect to Qtile
c = Client()


# get info of windows
wins = []
id_map = {}
id = 0
for win in c.windows():
    # select only current group
    if win["group"] and win['group'] == c.group.info()['name']:

        wins.append(bytes("%i: %s | %s " % (id, c.window[win['id']].inspect()['wm_class'][-1],
            win["name"]), 'utf-8'))
        id_map[id] = {
                'id' : win['id'],
                'group' : win['group']
                }
        id = id +1

# call dmenu
screen = c.screen.info()
width = 800
height = 200
x = (screen['width'] - width) / 2
y = (screen['height'] - height) / 2

DMENU = []
DMENU.extend(['dmenu', '-z', '-l', '10'])
DMENU.extend(['-x', str(x), '-y', str(y), '-w', str(width)])
#DMENU.extend(['-nb', '#000', '-nf', '#fff', '-sb', '#00BF32', '-sf', '#FFF'])
DMENU.extend(['-fn', 'WenQuanYi Micro Hei'])

p = subprocess.Popen(DMENU, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
out = p.communicate(b"\n".join(wins))[0]

if not out:
    exit()

# get selected window info
id = int(re.match(b"^\d+", out).group())
win = id_map[id]

# focusing selected window
g = c.group[win["group"]]
g.toscreen()
w = g.window[win["id"]]
for i in range(len(g.info()["windows"])):
    insp = w.inspect()
    if insp['attributes']['map_state']:
        break

    g.next_window()

# The above code will not correctly focus the selected window on the columns layout
# Thus we iterate throught all windows on current group.

def focus_on_column(win_id):
    window = c.window.info()['id']
    c.layout.next()
    while c.window.info()['id'] != window:
        print(c.window.info()['id'])
        if c.window.info()['id'] == win_id:
            return True
        c.layout.next()
    if c.window.info()['id'] == win_id:
        return True

def focus_columns(win_id):
    # focus the select window on layout columns
    start = layout_info['current']
    c.layout.right()
    while c.layout.info()['current'] != start:
        if focus_on_column(win_id):
            return
        c.layout.right()
    focus_on_column(win_id)

layout_info = c.layout.info()
if layout_info['name'] == 'columns':
    focus_columns(win['id'])
