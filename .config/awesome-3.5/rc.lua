-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- User Library
local vicious = require("vicious")
local revelation = require('revelation')

-- wmii like tagging(auto hide)
require("eminent")
-- i3 like tagging(back_and_forth)
require("back_and_forth")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
HOME = os.getenv("HOME")
-- Themes define colours, icons, and wallpapers
--beautiful.init("/usr/share/awesome/themes/default/theme.lua")
beautiful.init(HOME .. "/.config/awesome/zenburn/theme.lua")

revelation.init()

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod1"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    --awful.layout.suit.floating,
    awful.layout.suit.tile,
    --awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal,
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    --awful.layout.suit.max.fullscreen,
    --awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, awful.layout.suit.tile.bottom)

    -- set nmaster to 2 by default for all tags
    for index, val in ipairs(tags[s]) do
        awful.tag.setnmaster(2, val)
    end
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Functions
local function translate(cin_word)
    if cin_word == "" then return end
    local f  = io.popen("google-translate.py '"..cin_word.."'")
    local ret = "";
    for line in f:lines() do
        ret = ret .. line .. '\n'
    end
    f:close()
    return ret
    -- body
end

local function pop_translate(cin_word)
    naughty.destroy(frame)

    local translation = translate(cin_word)

    local function listen()
        os.execute("google-translate.py -v '"..cin_word.."' | mplayer -cache 1024 -")
    end

    frame = naughty.notify({ text = translation, timeout = 10, run = listen})
end
--}}}


-- {{{ Wibox

-- A reusable seperator
seperator = wibox.widget.imagebox();
seperator:set_image(beautiful.widget_sep);

-- CPU usage and temperature
cpuicon = wibox.widget.imagebox();
cpuicon:set_image(beautiful.widget_cpu);
cpuwidget=wibox.widget.textbox()
cpuwidget_t = awful.tooltip({objects = {cpuwidget},})
--vicious.register(cpuwidget, vicious.widgets.cpu, "$1%", 2)
vicious.register(cpuwidget, vicious.widgets.cpu, function(widget, args)
    local cpu_text = ""
    for i = 2, #args-1 do
        if cpu_text ~= "" then
            cpu_text = cpu_text .. "\n"
        end
        cpu_text = cpu_text .. "Core " .. i-1 .. ": " .. args[i] .. "%"
    end
    cpuwidget_t:set_text(cpu_text)
    return args[1] .. '%'
end, 3)

thermalwidgit = wibox.widget.textbox()
vicious.register(thermalwidgit, vicious.widgets.thermal, " ($1 ℃)", 10, "thermal_zone0")

--{{ Battery state
baticon = wibox.widget.imagebox()
baticon:set_image(beautiful.widget_bat)
-- Initialize widget
batwidget = wibox.widget.textbox()
-- Register widget
vicious.register(batwidget, vicious.widgets.bat, "$1$2%", 61, "BAT0")
--}}

--{{ Memory usage
memicon = wibox.widget.imagebox()
memicon:set_image(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_height(17):set_width(8):set_ticks_size(2)
membar:set_background_color(beautiful.fg_off_widget)
membar:set_color({type = "linear", from = {0,0}, to = {8, 14}, stops = {{0, beautiful.fg_end_widget}, {0.5, beautiful.fg_center_widget}, {1, beautiful.fg_widget}}})
-- RAM usage tooltip
membar_t = awful.tooltip({objects = {membar}, })
-- Register widget
vicious.cache(vicious.widgets.mem)
-- vicious.register(membar, vicious.widgets.mem, "$1", 13)
vicious.register(membar, vicious.widgets.mem,
    function (widget, args)
        membar_t:set_text("RAM:\t" .. args[2] .. "MB / " .. args[3] .. "MB ")
        return args[1]
    end, 13)
--}}

netwidget = wibox.widget.textbox()

-- Register widget
local function format_interface(table, interface)
    --print(interface, " CARRIER: ", table["{" .. interface .. " carrier}"])
    local netstr = ""
    if table["{" .. interface .. " carrier}"] == 1 then
        netstr = string.upper(string.sub(interface, 0, -2)) .. ' '
        .. string.sub(interface, -1) .. '[<span color="'
        .. beautiful.fg_netdn_widget ..'">↓'
        ..  table["{" .. interface .. " down_kb}"] .. 'K</span> <span color="'
        .. beautiful.fg_netup_widget ..'">'
        .. table["{" .. interface .. " up_kb}"] .. 'K↑</span>]'
    end
    return netstr
end

local function get_network_string(widget, args)
    local netstr = ""
    local interfaces = {"eth0", "eth1", "wlan0", "wlan1", "ppp0"}
    for i, interface in ipairs(interfaces) do
        local str = format_interface(args, interface)
        if str ~= "" then
            if netstr ~= "" then
                netstr = netstr .. " "
            end
            netstr = netstr .. str
        end
    end
    return netstr
end
vicious.register(netwidget, vicious.widgets.net, get_network_string , 3)
--}}

--{{
-- disk read write
diskicon = wibox.widget.imagebox()
diskicon:set_image(beautiful.widget_fs)
diskwidget = wibox.widget.textbox()
vicious.register(diskwidget, vicious.widgets.dio,
    "↓(${sda read_mb}M,${sda write_mb}M)↑", 3)
--}}

-- Volume level

-- Initialize widgets
volicon = wibox.widget.imagebox()
volicon:set_image(beautiful.widget_vol)
volwidget = wibox.widget.textbox()
vicious.register(volwidget, vicious.widgets.volume,
    function (widget, args)
    local label = { ["♫"] = "%", ["♩"] = "M" }
    local vol = args[1]
    if args[2] ~= "♫" then
        vol = '<span color="' .. beautiful.fg_end_widget .. '">' .. args[1] .. '</span>'
    end
    return " " .. vol .. "%"
end, 1800, "Master")
-- Register buttons
volwidget:buttons(awful.util.table.join(
    awful.button({ }, 1, function () awful.util.spawn_with_shell("amixer -q -D pulse sset Master toggle") vicious.force({volwidget}) end),
    awful.button({ }, 4, function () awful.util.spawn_with_shell("amixer -q sset Master 5%+ unmute") vicious.force({volwidget}) end),
    awful.button({ }, 5, function () awful.util.spawn_with_shell("amixer -q sset Master 5%- unmute") vicious.force({volwidget}) end)
))

-- Weather
weatherwidget = wibox.widget.textbox();
weather_t = awful.tooltip({objects = {weatherwidget}, })
-- Register widget
vicious.register(weatherwidget, vicious.widgets.weather,
    function (widget, args)
        weather_t:set_text("City:\t\t" .. args["{city}"] .. "\nWind:\t" .. args["{windkmh}"] .. "km/h " .. args["{wind}"] .. "\nSky:\t\t" .. args["{sky}"] .. "\nHumidity:\t" .. args["{humid}"] .. "%")
        return args["{tempc}"] .. "℃"
    end, 1801, "ZSSS"
    -- 1800: check every 30 minutes.
    -- "ZSNJ" the Montreal ICAO code.
)

-- {{{ dictionary everyday

function word_of_day_widget(format, warg)
    local url = "http://www.dictionary.com/wordoftheday/"
    local f = io.popen("curl --connect-timeout 1 -fsm 3 '"..url.."'")
    local html = f:read("*all")
    f:close()
    local word = string.match(html, 'data%-word="(.-)"')

    return {["word"] = word,
            ["translation"] = translate(word)}
end

word_of_day = wibox.widget.textbox()
word_of_day_t = awful.tooltip({objects = {word_of_day}, })
vicious.register(word_of_day, word_of_day_widget,
    function (widget, args)
        word_of_day_t:set_text(args["translation"])
        widget:buttons(awful.button({}, 1, function() os.execute("google-translate.py -v '"..args['word'].."' | mplayer -cache 1024 -") end))
        return 'WoD: '..args["word"]
    end, 1801
)
-- }}}

-- {{{ Show stock data real time

local _stocks = {}

local function split(s, delimiter)
    result = {};
    for match in (s..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, match);
    end
    return result;
end

local function parse_stock_line(line)
    local data = split(line, ',')
    _stocks["code"]   = string.match(data[1], '"(.-)"')
    _stocks["start"]  = data[2]
    _stocks["change"] = string.match(data[3], '"(.-)"')
    _stocks["low"]    = data[4]
    _stocks["high"]   = data[5]
    _stocks["close"]  = data[6]
    return _stocks
end

local function stock(format, warg)
    if not warg then return end

    local codes = 's='..table.concat(warg, "+")
    local fmt = "&f=socghl1"
    local url = "http://finance.yahoo.com/d/quotes.csv?"..codes..fmt
    local ret = {}
    local f = io.popen("curl -L --connect-timeout 1 -fsm 3 '"..url.."'")
    for line in f:lines() do
        table.insert(ret, parse_stock_line(line))
    end
    f:close()
    return ret
end

local function format_stock_change(change)
    if change:sub(1,1) == '-' then
        return '<span color="' .. beautiful.fg_netup_widget .. '">' .. change .. '</span>'
    else
        return '<span color="' .. beautiful.fg_netdn_widget .. '">' .. change .. '</span>'
    end
end

local function format_stock_detail(stock_info)

end

stock_widget = wibox.widget.textbox()
stock_widget_t = awful.tooltip({objects = {stock_widget}, })
vicious.register(stock_widget, stock,
    function (widget, args)
        local ret = ""
        for i, stock in ipairs(args) do
            local change = stock['change']
            ret = ret .. ' ' .. stock['code'] .. ': ' .. format_stock_change(change)
        end
        return ret
    end, 11, {'399001.sz'}
)
---}}}

-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Create a wibox for each screen and add it
mywibox = {}
mywibox_titles = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  --c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    --mywibox[s] = awful.wibox({ position = "top", screen = s })
    mywibox[s] = awful.wibox({ position = "bottom", screen = s })


    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(seperator)
    left_layout:add(mytaglist[s])
    left_layout:add(seperator)
    left_layout:add(mypromptbox[s])
    left_layout:add(seperator)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(seperator)
    right_layout:add(stock_widget)
    right_layout:add(seperator)
    right_layout:add(word_of_day)
    right_layout:add(seperator)
    right_layout:add(baticon);right_layout:add(batwidget)
    right_layout:add(seperator)
    --right_layout:add(cpuicon);right_layout:add(cpugraph);right_layout:add(tzswidget)
    right_layout:add(cpuicon)
    right_layout:add(cpuwidget)
    right_layout:add(thermalwidgit)
    right_layout:add(seperator)
    right_layout:add(memicon);right_layout:add(membar)
    right_layout:add(seperator)
    --right_layout:add(dnicon)
    right_layout:add(netwidget)
    --right_layout:add(upicon)
    right_layout:add(seperator)
    right_layout:add(diskicon)
    right_layout:add(diskwidget)
    right_layout:add(seperator)
    right_layout:add(volicon);right_layout:add(volwidget)
    right_layout:add(seperator)
    right_layout:add(weatherwidget)
    right_layout:add(seperator)
    right_layout:add(mytextclock)
    right_layout:add(seperator)
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(seperator)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    --layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)

    -- create title wibox
    mywibox_titles[s] = awful.wibox({ position = "top", screen = s })
    local layout = wibox.layout.align.horizontal()
    layout:set_middle(mytasklist[s])
    mywibox_titles[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    --awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab", revelation),
    --awful.key({ modkey,           }, "Tab",
        --function ()
            --awful.client.focus.history.previous()
            --if client.focus then
                --client.focus:raise()
            --end
        --end),

    -- Standard program
    awful.key({ modkey, "Shift"   }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Shift"   }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- my settings
    awful.key({ modkey,           }, "e", function () awful.layout.set(awful.layout.suit.tile) end),
    awful.key({ modkey,           }, "w", function () awful.layout.set(awful.layout.suit.tile.bottom) end),
    awful.key({ modkey,           }, "s", function () awful.layout.set(awful.layout.suit.max) end),

    awful.key({ modkey, "Shift"   }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end),

    --{{ My settings
    -- volumn controls
    awful.key({         }, "F20",   function () awful.util.spawn_with_shell("amixer -q sset Master toggle") vicious.force({volwidget}) end),
    awful.key({         }, "F21",   function () awful.util.spawn_with_shell("amixer -q sset Master 5%- unmute") vicious.force({volwidget}) end),
    awful.key({         }, "F22",   function () awful.util.spawn_with_shell("amixer -q sset Master 5%+ unmute") vicious.force({volwidget}) end),
    awful.key({ modkey, }, "BackSpace",   function () awful.util.spawn_with_shell("amixer -q -D pulse sset Master toggle") vicious.force({volwidget}) end),
    awful.key({ modkey, }, "-",   function () awful.util.spawn_with_shell("amixer -q sset Master 5%- unmute") vicious.force({volwidget}) end),
    awful.key({ modkey, }, "=",   function () awful.util.spawn_with_shell("amixer -q sset Master 5%+ unmute") vicious.force({volwidget}) end),


    -- handful keys for manipulate number of masters
    awful.key({ modkey, }, "comma",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, }, "period",    function () awful.tag.incnmaster(-1)      end),

    -- xscreensaver
    awful.key({"Mod1", "Control"}, "l", function() awful.util.spawn_with_shell("if hash xscreensaver 2>/dev/null; then xscreensaver-command -lock; elif hash gnome-screensaver 2>/dev/null; then gnome-screensaver-command -l; fi") end),

    -- { hide/show wibox in awesome 3
    awful.key({ modkey }, "b",     function ()
        mywibox[mouse.screen].visible = not mywibox[mouse.screen].visible
    end),
    -- }
    -- { sdcv/stardict
    awful.key({ modkey }, "d", function ()
        local f = io.popen("xsel -o")
        local new_word = f:read("*a")
        f:close()

        if frame ~= nil then
            naughty.destroy(frame)
            frame = nil
            if old_word == new_word then
                return
            end
        end
        old_word = new_word

        pop_translate(new_word)
    end),
    awful.key({ modkey, "Shift" }, "d", function ()
        awful.prompt.run({prompt = "Dict: "}, mypromptbox[mouse.screen].widget, pop_translate, nil, awful.util.getdir("cache").."/dict")
    end)
    -- }
    --}}
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey,           }, "F4",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  function (c) awful.titlebar.toggle(c); awful.client.floating.toggle(); end),
    --awful.key({ modkey,           }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "Return", function (c)
        if c == awful.client.getmaster() then
            local screen = mouse.screen
            -- If the client is newly create, then it is the last visited
            -- window, thus we want the one before it.
            local prev = awful.client.focus.history.get(screen, 0)
            if prev == c then
                prev = awful.client.focus.history.get(screen, 1)
            end
            if prev ~= nil then
                c:swap(prev)
                client.focus = prev; prev:raise()
                awful.client.focus.history.add(c)
            end
        else
            awful.client.focus.history.add(awful.client.getmaster())
            c:swap(awful.client.getmaster())
        end
    end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     keys = clientkeys,
                     buttons = clientbuttons,

                     -- disable size hint
                     size_hints_honor = false
                 } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    --{ rule = { class = "gimp" },
      --properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
    { rule = { class = "Pidgin", role = "buddy_list" },
    properties = {switchtotag = true, floating=true,
    maximized_vertical=true, maximized_horizontal=false },
    callback = function (c)
        local cl_width = 250    -- width of buddy list window
        local def_left = false  -- default placement. note: you have to restart
        -- pidgin for changes to take effect

        local scr_area = screen[c.screen].workarea
        local cl_strut = c:struts()
        local geometry = nil

        -- adjust scr_area for this client's struts
        if cl_strut ~= nil then
            if cl_strut.left ~= nil and cl_strut.left > 0 then
                geometry = {x=scr_area.x-cl_strut.left, y=scr_area.y,
                width=cl_strut.left}
            elseif cl_strut.right ~= nil and cl_strut.right > 0 then
                geometry = {x=scr_area.x+scr_area.width, y=scr_area.y,
                width=cl_strut.right}
            end
        end
        -- scr_area is unaffected, so we can use the naive coordinates
        if geometry == nil then
            if def_left then
                c:struts({left=cl_width, right=0})
                geometry = {x=scr_area.x, y=scr_area.y,
                width=cl_width}
            else
                c:struts({right=cl_width, left=0})
                geometry = {x=scr_area.x+scr_area.width-cl_width, y=scr_area.y,
                width=cl_width}
            end
        end
        c:geometry(geometry)
    end },
    -- prohibit wine floating windows to move
    { rule = { class = "Wine" },
    properties = { border_width = 0} },
    -- QQ International's buddy pane
    { rule = { class = "Wine", name = "QQ International" },
    properties = {switchtotag = true, floating=true,
    maximized_vertical=true, maximized_horizontal=false },
    callback = function (c)
        local cl_width = 300    -- width of buddy list window
        local def_left = false  -- default placement. note: you have to restart
        -- pidgin for changes to take effect

        local scr_area = screen[c.screen].workarea
        local cl_strut = c:struts()
        local geometry = nil

        -- adjust scr_area for this client's struts
        if cl_strut ~= nil then
            if cl_strut.left ~= nil and cl_strut.left > 0 then
                geometry = {x=scr_area.x-cl_strut.left, y=scr_area.y,
                width=cl_strut.left}
            elseif cl_strut.right ~= nil and cl_strut.right > 0 then
                geometry = {x=scr_area.x+scr_area.width, y=scr_area.y,
                width=cl_strut.right}
            end
        end
        -- scr_area is unaffected, so we can use the naive coordinates
        if geometry == nil then
            if def_left then
                c:struts({left=cl_width, right=0})
                geometry = {x=scr_area.x, y=scr_area.y,
                width=cl_width}
            else
                c:struts({right=cl_width, left=0})
                geometry = {x=scr_area.x+scr_area.width-cl_width, y=scr_area.y,
                width=cl_width}
            end
        end
        c:geometry(geometry)
    end },

    { rule = { instance = "plugin-container" },
    properties = { floating = true,
    focus = yes } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    --local titlebars_enabled = false
    local titlebars_enabled = true
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        --right_layout:add(awful.titlebar.widget.floatingbutton(c))
        -- toggle titlebar when clicked.
        right_layout:add(awful.titlebar.widget.button(c,"floating", awful.client.floating.get,function (c) awful.titlebar.toggle(c); awful.client.floating.toggle(); end))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)

        if (awful.client.floating.get(c)) then
            awful.titlebar.show(c)
        else
            awful.titlebar.hide(c)
        end
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{ Autorun programs
autorun = true
autorunApps =
{
    HOME .. "/.config/awesome/autostart.sh",
}
if autorun then
    for app = 1, #autorunApps do
        awful.util.spawn_with_shell(autorunApps[app])
    end
end
-- }}
--- }}

