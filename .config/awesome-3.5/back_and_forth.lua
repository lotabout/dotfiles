-- Grab environment
local ipairs = ipairs
local pairs = pairs
local awful = require("awful")
local table = table
local capi = {
    tag = tag,
    mouse = mouse,
    client = client,
    screen = screen,
    wibox = wibox,
    timer = timer,
    keygrabber = keygrabber,
}

local getscreen = capi.tag.getscreen

-- Eminent: Effortless 3-style back-and-forth
local i3 = {}
back_and_forth = true

-- Initializing last tags.
last_tags = {}
for s = 1, screen.count() do
    local all_tags = awful.tag.gettags(s)
    last_tags[s] = all_tags[1]
end

-- Grab the original functions we're replacing
local orig = {
    viewidx = awful.tag.viewidx,
    viewonly = awful.tag.viewonly,

    taglist = awful.widget.taglist.new,
    --label = awful.widget.taglist.label.all,
    label = awful.widget.taglist.filter.all,
}


--- View only a tag.
-- @param t The tag object
awful.tag.viewonly = function(t)
    local screen = awful.tag.getscreen(t)
    local sel_tag = awful.tag.selected(screen)
    local target_tag = t
    if back_and_forth and t == sel_tag then
        target_tag = last_tags[screen]
    end
    last_tags[screen] = sel_tag
    orig.viewonly(target_tag)
end

-- View a tag by its taglist index.
-- @param i The relative index to see
-- @param screen Optional screen number
awful.tag.viewidx = function(i, screen)
    -- save the current tag into last_tags
    local screen = screen or capi.mouse.screen
    local all_tags = awful.tag.gettags(screen)
    local sel_tag = awful.tag.selected(screen)
    local sel_idx = awful.util.table.hasitem(all_tags, sel_tag)
    local last_idx = awful.util.table.hasitem(all_tags, last_tags[screen])
    
    local target_idx = i
    if back_and_forth and i == sel_idx then
        target_idx = last_idx
    end
    last_tags[screen] = sel_tag
    orig.viewidx(target_idx, screen)
end

return i3
