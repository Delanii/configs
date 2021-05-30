local wezterm = require 'wezterm';

return {
  -- Font customization
  font = wezterm.font("Fira Code"),
  font_size = 11.0,

  -- Theme customization
--  color_scheme = "cyberpunk",
--  color_scheme = "DoomOne",
--  color_scheme = "DotGov",
  color_scheme = "Duotone Dark",

  -- Windo settings
  -- window padding: not have letters all over
  window_padding = {
    left = 8,
    -- This will become the scrollbar width if you have enabled the scrollbar!
    right = 8,

    top = 8,
    bottom = 8,
  },

  -- Canvas settings: window, backgound picture
  window_background_image = "./AdeptusMechanicus.jpg",

  window_background_image_hsb = {
    -- Darken the background image by reducing it to 1/3rd
    brightness = 0.3,

    -- You can adjust the hue by scaling its value.
    -- a multiplier of 1.0 leaves the value unchanged.
    hue = 1.0,

    -- You can adjust the saturation also.
    saturation = 1.0,
  },

  -- specifies transparency of the terminal window
  window_background_opacity = 0.7,

  -- specifies transparency of text beckground -- for example output of ls command
  text_background_opacity = 0.3,

  -- Keybindings customization
  keys = {
    {key = "c", mods = "CTRL|SHIFT", action = wezterm.action{CopyTo = "Clipboard"}},
    {key = "v", mods = "CTRL|SHIFT", action = wezterm.action{PasteFrom = "Clipboard"}},
    {key = "w", mods = "CTRL", action = wezterm.action{CloseCurrentTab = {confirm = false}}},
    {key = "t", mods = "CTRL", action = wezterm.action{SpawnTab = "CurrentPaneDomain"}},
    {key = "=", mods = "CTRL", action = wezterm.action{SplitVertical = {domain = "CurrentPaneDomain"}}},
    {key = "-", mods = "CTRL", action = wezterm.action{SplitHorizontal = {domain = "CurrentPaneDomain"}}},
    {key = "m", mods = "SUPER", action = "Nop"}, -- disable keybinding for hiding terminal window. I dont like that.
    {key = "n", mods = "CTRL", action = "SpawnWindow"},
    {key = "m", mods = "CTRL", action = "ToggleFullScreen"} -- according to github issue #177 https://github.com/wez/wezterm/issues/177 is not working on windows. Crrently it is neccessary, because there is not option for that, see github issue #284 https://github.com/wez/wezterm/issues/284
  }
}
