local tavi = {}

-- Init
buffer.caret_style = _SCINTILLA.constants.CARETSTYLE_BLOCK
buffer.caret_period = 0

-- Constants
tavi.PASTE_LINE = 'visual_line'
tavi.PASTE_HERE = 'here'

-- State
tavi.state = {
  paste_mode = tavi.PASTE_HERE
}

-- Operations
tavi.char_at = function (pos)
  return string.char(buffer.char_at[pos])
end

tavi.sel = function (endp, startp)
  if endp then
    local startp = startp or tavi.pos.anchor()
    buffer:set_sel(startp, endp)
    return true
  end
end

tavi.moveto = function (pos, offset)
  if pos then
    buffer:goto_pos(pos + (offset or 0))
    buffer:choose_caret_x()  -- important for rectangular selections
    return true
  end
end

tavi.change_character_case = function()
  local c = buffer.get_sel_text()
  local newc = string.upper(c)
  if newc ~= c then
    buffer.replace_sel(newc)
  else
    buffer.replace_sel(string.lower(c))
  end
end

tavi.replace_character = function (replace_char)
  tavi.sel(tavi.pos.current(), tavi.pos.current()+1)
  buffer:replace_sel(replace_char)
end

tavi.clear_selection = function (pos)
  local pos = pos or tavi.pos.current()
  buffer:set_empty_selection(pos)
end

-- Modes
tavi.enter_mode = function (mode)
  if keys.MODE == mode then
    return
  end

  if mode == 'normal' then
    tavi.clear_selection()
    buffer:cancel()
  elseif mode == 'visual_block' then
    local pos = tavi.pos.current() -- must go before buffer.selection_mode
    buffer.selection_mode = buffer.SEL_RECTANGLE
    buffer.virtual_space_options = buffer.VS_RECTANGULARSELECTION
    buffer.rectangular_selection_anchor = pos
    buffer.rectangular_selection_caret = pos
  end

  keys.MODE = mode
  events.emit(events.UPDATE_UI)
end

tavi.key_mode_text = {
  ['insert'] = '-- INSERT --',
  ['visual_block'] = '-- VISUAL BLOCK --',
  ['visual_line'] = '-- VISUAL LINE --',
  ['visual'] = '-- VISUAL --',
  ['normal'] = '',
}

events.connect(events.UPDATE_UI, function()
  ui.statusbar_text = tavi.key_mode_text[keys.MODE] or tavi.key_mode_text['insert']
end)

local set_normal_mode_events = {
  events.BUFFER_NEW,
  events.VIEW_AFTER_SWITCH,
  events.BUFFER_AFTER_SWITCH,
  events.RESET_AFTER,
}
for _, event in ipairs(set_normal_mode_events) do
  events.connect(event, function ()
    tavi.enter_mode('normal')
  end)
end

-- Positions
tavi.pos = {}

local find_character = function (direction, char, pos)
  local pos = pos or tavi.pos.current()
  pos = pos + direction    -- This i_s a sham. find('s') == tavi.pos.current() w/o +1

  local end_pos
  if direction == 1 then
    end_pos = tavi.pos.end_line()
  elseif direction == -1 then
    end_pos = tavi.pos.start_line()
  else
    return nil
  end

  for p=pos,end_pos,direction do
    if tavi.char_at(p) == char then
      return p
    end
  end
  return nil
end

local find_brace_capture = function (brace_char, pos)
  local pos = pos or tavi.pos.current()

  local open_brace_map = {
    ['{'] = '}',
    ['('] = ')',
    ['['] = ']',
    ['<'] = '>',
  }

  local close_brace_map = {
    ['}'] = '{',
    [')'] = '(',
    [']'] = '[',
    ['>'] = '<',
  }

  local open_brace = close_brace_map[brace_char] or brace_char
  local close_brace = open_brace_map[brace_char] or brace_char

  if not (open_brace and close_brace) then
    return nil, nil
  end

  local find_brace = function (brace_add, direction)
    local count = 0
    local p = pos
    local bol, eol = tavi.pos.start_line(), tavi.pos.end_line()
    while count ~= 1 and bol <= p and p <= eol do
      local c = tavi.char_at(p)
      count = count + (brace_add[c] or 0)
      p = p + direction
    end
    return count == 1 and (p - direction) or nil
  end

  local open_brace_pos = find_brace({ [open_brace] = 1, [close_brace] = -1 }, -1)
  local close_brace_pos = find_brace({ [close_brace] = 1, [open_brace] = -1 }, 1)

  return close_brace_pos, open_brace_pos
end

local pos_from_line_change = function (line_offset, pos)
  local pos = pos or tavi.pos.current()
  local start_line = tavi.pos.start_line(pos)
  local offset = pos - start_line
  local line = buffer:line_from_position(pos)
  local n_line = line + line_offset
  n_line = n_line < 0 and 0 or n_line
  n_line = n_line > buffer.line_count and buffer.line_count or n_line
  local n_start_line = tavi.pos.line(n_line)
  local n_end_line = tavi.pos.end_line(n_start_line)
  local n_pos = n_start_line + offset
  n_pos = n_pos < n_start_line and n_start_line or n_pos
  n_pos = n_pos > n_end_line and n_end_line+1 or n_pos  -- start of next line
  return n_pos
end

tavi.pos.line = function (l)
  if l < 0 or l > buffer.line_count then return nil end
  return buffer:position_from_line(l)
end
tavi.pos.current = function () return buffer.current_pos end
tavi.pos.anchor = function () return buffer.anchor end
tavi.pos.character_right =  function (c) return tavi.pos.current()+(c or 1) end
tavi.pos.character_left =  function (c) return tavi.pos.current()-(c or 1) end
tavi.pos.line_up = function (c) return pos_from_line_change(-(c or 1)) end
tavi.pos.line_down = function (c) return pos_from_line_change(c or 1) end
tavi.pos.document_end = function () return tavi.pos.line(buffer.line_count) end

tavi.pos.end_line = function (pos, with_newline)
  local pos = pos or tavi.pos.current()
  local line = buffer:line_from_position(pos)
  if with_newline then
    -- ...end of line\n_
    return buffer:line_length(line) + buffer:position_from_line(line)
  else
    -- line_end_position before end of line characters
    -- with -1 -> ...end of lin_e\n makes block caret show on last char
    return buffer.line_end_position[line]-1
  end
end

tavi.pos.start_line = function (pos)
  local pos = pos or tavi.pos.current()
  local line = buffer:line_from_position(pos)
  return buffer:position_from_line(line)
end

tavi.pos.soft_start_line = function (pos)
  local pos = pos or tavi.pos.current()
  local endp = tavi.pos.end_line(pos)
  local p = tavi.pos.start_line(pos)
  while p <= endp and (tavi.char_at(p) == ' ' or tavi.char_at(p) == '\t') do p = p + 1 end
  return p
end

tavi.pos.word_end = function (pos)
  local pos = pos or tavi.pos.current()
  return buffer:word_end_position(pos)
end

tavi.pos.word_start = function (pos)
  local pos = pos or tavi.pos.current()
  return buffer:word_start_position(pos)
end

tavi.pos.inside_brace_capture = function (brace_char, pos)
  local close_brace_pos, open_brace_pos = find_brace_capture(brace_char, pos)
  return close_brace_pos and close_brace_pos - 1, open_brace_pos and open_brace_pos + 1
end

tavi.pos.outside_brace_capture = function (brace_char, pos)
  local close_brace_pos, open_brace_pos = find_brace_capture(brace_char, pos)
  return close_brace_pos, open_brace_pos
end

tavi.pos.inside_character = function (char, pos)
  local pos = pos or tavi.pos.current()
  local begin_pos = find_character(-1, char, pos)
  local end_pos = find_character(1, char, pos)
  return end_pos and end_pos - 1, begin_pos and begin_pos + 1
end

tavi.pos.outside_character = function (char, pos)
  local pos = pos or tavi.pos.current()
  local begin_pos = find_character(-1, char, pos)
  local end_pos = find_character(1, char, pos)
  return end_pos, begin_pos
end

tavi.pos.inside_word = function (pos)
  local pos = pos or tavi.pos.current()
  return tavi.pos.word_end(pos) - 1, tavi.pos.word_start(pos)
end

tavi.pos.outside_word = function (pos)
  local pos = pos or tavi.pos.current()
  return tavi.pos.word_end(pos), tavi.pos.word_start(pos) - 1
end

-- Thi_s can be
-- right_to(a) -> This ca_n be
-- left_to(h) -> T_his can be
-- right_til(a) -> This c_an be
-- left_til(h) -> Th_is can be
-- right_til_til(a) -> This _can be
-- left_til_til(h) -> Thi_s can be
local make_to_character = function (direction, offset)
  local function to_char_fn(char, n, pos)
    local n = n or 1
    local pos = pos or tavi.pos.current()
    if n == 0 then
      return pos and offset + pos
    else
      local nextpos = find_character(direction, char, pos)
      return nextpos and to_char_fn(char, n-1, nextpos)
    end
  end
  return to_char_fn
end

tavi.pos.right_to_character = make_to_character(1, 1)
tavi.pos.left_to_character = make_to_character(-1, 0)
tavi.pos.right_til_character = make_to_character(1, 0)
tavi.pos.left_til_character = make_to_character(-1, 1)
tavi.pos.right_til_til_character = make_to_character(1, -1)
tavi.pos.left_til_til_character = make_to_character(-1, 2)

-- Actions
local make_action = function (action)
  act = {}
  setmetatable(act, {
    __call = function (act, ...) return action(...) end,
    __index = function (act, key)
      if tavi.pos[key] then
        return function (...) return action(tavi.pos[key](...)) end
      end
    end
  })
  return act
end

tavi.move = make_action(function (pos) return tavi.moveto(pos) end)

local cut_action = function (endp, startp)
  local pos = tavi.pos.current()
  if endp and tavi.sel(endp < pos and endp or endp+1, startp) then
    buffer.cut()
    return true
  end
end
tavi.cut = make_action(cut_action)

local copy_action = function (endp, startp)
  local pos = tavi.pos.current()
  if endp and tavi.sel(endp < pos and endp or endp+1, startp) then
    buffer.copy()
    tavi.clear_selection()
    return true
  end
end
tavi.copy = make_action(copy_action)

local select_action = function (endp, startp)
  local anchor = buffer.anchor
  local pos = tavi.pos.current()
  -- Emulate vim/zsh/tmux block caret selection behavior
  if pos < anchor and endp >= anchor then
    buffer.anchor = anchor - 1
  end
  if pos >= anchor and endp < anchor then
    buffer.anchor = anchor + 1
  end
  tavi.sel(endp, startp)
end
tavi.select = make_action(select_action)

local change_action = function(...)
  return tavi.cut(...) and tavi.enter_mode(nil)
end
tavi.change = make_action(change_action)

-- Key Bindings
local make_char_functor_table = function (f)
  local character_table = {}
  for c=string.byte(' '), string.byte('~') do
    local char = string.char(c)
    character_table[char] = f(char)
  end
  return character_table
end

-- ['a'] = function () fn_a(1) end
-- ['b'] = function () fn_b(1) end
-- ['1'] = {
--   ['a'] = function () fn_a(0) end
--   ['b'] = function () fn_b(0) end
--   ['0'] = { ... }
--   ['1'] = { ... }
-- ['2'] = {
--   ['a'] = function () fn_a(1) end
--   ... }
local function make_number_functor_table(function_table, number)
  local function apply_number(ft, number)
    if type(ft) == 'function' then
      return ft(number)
    else
      local t = {}
      for k,f in pairs(ft) do
        t[k] = apply_number(f, number)
      end
      return t
    end
  end

  local t = apply_number(function_table, number or 1)

  setmetatable(t, {
    __index = function(t, key)
      local n = tonumber(key)
      if not n then
        return
      end
      return make_number_functor_table(function_table, (number or 0)*10+n)
    end
  })

  return t
end

local make_canonical_movements = function (act)
  local movements = make_number_functor_table({
    ['g'] = {
      ['g'] = function (l) return function () act.line(l-1) end end
    },
    ['h'] = function (n) return function () act.character_left(n) end end,
    ['left'] = function (n) return function () act.character_left(n) end end,
    ['j'] = function (n) return function () act.line_down(n) end end,
    ['down'] = function (n) return function () act.line_down(n) end end,
    ['k'] = function (n) return function () act.line_up(n) end end,
    ['up'] = function (n) return function () act.line_up(n) end end,
    ['l'] = function (n) return function () act.character_right(n) end end,
    ['right'] = function (n) return function () act.character_right(n) end end,
    ['f'] = make_char_functor_table(function (c) return function (n) return function () act.right_til_character(c, n) end end end),
    ['t'] = make_char_functor_table(function (c) return function (n) return function () act.right_til_til_character(c, n) end end end),
    ['F'] = make_char_functor_table(function (c) return function (n) return function () act.left_to_character(c, n) end end end),
    ['T'] = make_char_functor_table(function (c) return function (n) return function () act.left_til_character(c, n) end end end),
  })
  movements['$'] = act.end_line
  movements['^'] = act.soft_start_line
  movements['0'] = act.start_line
  movements['w'] = act.word_end
  movements['b'] = act.word_start
  movements['G'] = act.document_end
  movements['i'] = make_char_functor_table(function (c) return function () act.inside_brace_capture(c) end end)
  movements['i']["'"] = function () act.inside_character("'") end
  movements['i']['"'] = function () act.inside_character('"') end
  movements['a'] = make_char_functor_table(function (c) return function () act.outside_brace_capture(c) end end)
  movements['a']["'"] = function () act.outside_character("'") end
  movements['a']['"'] = function () act.outside_character('"') end
  movements['i']['w'] = act.inside_word
  movements['a']['w'] = act.outside_word
  return movements
end

-- Normal
keys.normal = make_canonical_movements(tavi.move)
keys.normal['~'] = function () tavi.sel(tavi.pos.current()+1) tavi.change_character_case() end
keys.normal['r'] = make_char_functor_table(function (c) return function () tavi.replace_character(c) end end)
keys.normal['%'] = textadept.editing.match_brace
keys.normal['n'] = function () if ui.find.find_entry_text then events.emit(events.FIND, ui.find.find_entry_text, true) end end
keys.normal['N'] = function () if ui.find.find_entry_text then events.emit(events.FIND, ui.find.find_entry_text, false) end end

-- Cut, Copy, Paste
keys.normal['x'] = function () tavi.state.paste_mode = tavi.PASTE_HERE tavi.cut.current() end
local visual_cut_action = make_action(function (...)
  tavi.state.paste_mode = tavi.PASTE_HERE
  return tavi.cut(...)
end)
keys.normal['d'] = make_canonical_movements(visual_cut_action)
keys.normal['d']['d'] = function () tavi.state.paste_mode = tavi.PASTE_LINE buffer.line_cut() end
local visual_copy_action = make_action(function (...)
  tavi.state.paste_mode = tavi.PASTE_HERE
  return tavi.copy(...)
end)
keys.normal['y'] = make_canonical_movements(visual_copy_action)
keys.normal['y']['y'] = function () tavi.state.paste_mode = tavi.PASTE_LINE buffer.line_copy() end
keys.normal['p'] = function ()
  if tavi.state.paste_mode == tavi.PASTE_LINE then
    tavi.move.line_down()
    tavi.move.start_line()
    buffer:paste()
  else
    tavi.move.character_right()
    buffer.paste()
  end
end
keys.normal['P'] = function ()
  if tavi.state.paste_mode == tavi.PASTE_LINE then
    tavi.move.start_line()
    buffer:paste()
  else
    buffer:paste()
  end
end

  -- Undo/Redo
keys.normal['u'] = buffer.undo
keys.normal['cr'] = buffer.redo

  -- Mode Switching
keys.normal[':'] = function () ui.command_entry.enter_mode('lua_command', 'lua') end
keys.normal['/'] = ui.find.find_incremental
keys.normal['cv'] = function () tavi.enter_mode('visual_block') end
keys.normal['v'] = function () tavi.enter_mode('visual') end
keys.normal['V'] = function () tavi.enter_mode('visual_line') end
keys.normal['i'] = function () tavi.enter_mode(nil) end
keys.normal['I'] = function()
  buffer.home()
  tavi.enter_mode(nil)
end
keys.normal['O'] = function()
  local pos = pos or tavi.pos.current()
  local line = buffer:line_from_position(pos)
  if line == 0 then
    buffer:home()
    buffer:new_line()
    buffer:line_up()
  else  -- use this approach if not first line for better indentation behavior
    buffer:line_up()
    buffer:line_end()
    buffer:new_line()
  end
  tavi.enter_mode(nil)
end
keys.normal['o'] = function()
  buffer.line_end()
  buffer.new_line()
  tavi.enter_mode(nil)
end
keys.normal['a'] = function()
  buffer:char_right()
  tavi.enter_mode(nil)
end
keys.normal['A'] = function()
  buffer.line_end()
  tavi.enter_mode(nil)
end
keys.normal['C'] = function()
  tavi.state.paste_mode = tavi.PASTE_HERE
  buffer:del_line_right()
  tavi.enter_mode(nil)
end
local visual_change_action = make_action(function (...)
  tavi.state.paste_mode = tavi.PASTE_HERE
  return tavi.change(...)
end)
keys.normal['c'] = make_canonical_movements(visual_change_action)

-- Visual Block
-- TODO: Needs work
keys.visual_block = {}
keys.visual_block['esc'] = function () tavi.enter_mode('normal') end
keys.visual_block['c['] = function () tavi.enter_mode('normal') end
keys.visual_block['down'] = buffer.line_down_rect_extend
keys.visual_block['up'] = buffer.line_up_rect_extend
keys.visual_block['h'] = buffer.char_left_rect_extend
keys.visual_block['j'] = buffer.line_down_rect_extend
keys.visual_block['k'] = buffer.line_up_rect_extend
keys.visual_block['l'] = buffer.char_right_rect_extend
keys.visual_block['0'] = buffer.home
keys.visual_block['^'] = function () tavi.sel(tavi.pos.soft_start_line()) end
keys.visual_block['$'] = buffer.line_end
keys.visual_block['x'] = function ()
  buffer:cut()
  tavi.enter_mode('normal')
end
keys.visual_block['i'] = function ()
  tavi.enter_mode(nil)
end
keys.visual_block['v'] = function () tavi.enter_mode('visual') end
keys.visual_block['V'] = function () tavi.enter_mode('visual_line') end

-- Visual
local pre_adjust_selection = function ()
  local end_sel = buffer.selection_end
  local pos = tavi.pos.current()
  if end_sel == pos then
    tavi.select.character_right()
  end
end

keys.visual = make_canonical_movements(tavi.select)
keys.visual[':'] = function () ui.command_entry.enter_mode('lua_command', 'lua') end
keys.visual['<'] = buffer.back_tab
keys.visual['>'] = buffer.tab
keys.visual['esc'] = function () tavi.enter_mode('normal') end
keys.visual['c['] = function () tavi.enter_mode('normal') end
keys.visual['cv'] = function () tavi.enter_mode('visual_block') end
keys.visual['V'] = function () tavi.enter_mode('visual_line') end
keys.visual['~'] = function ()
  pre_adjust_selection()
  tavi.change_character_case()
  tavi.enter_mode('normal')
end
keys.visual['x'] = function ()
  pre_adjust_selection()
  tavi.state.paste_mode = tavi.PASTE_HERE
  buffer:cut()
  tavi.enter_mode('normal')
end
keys.visual['d'] = keys.visual['x']
keys.visual['y'] = function ()
  pre_adjust_selection()
  tavi.state.paste_mode = tavi.PASTE_HERE
  buffer:copy()
  tavi.enter_mode('normal')
end
keys.visual['c'] = function ()
  pre_adjust_selection()
  tavi.state.paste_mode = tavi.PASTE_HERE
  buffer:cut()
  tavi.enter_mode(nil)
end

-- Visual Line
local extend_selection_to_line = function ()
  local start_pos = buffer.selection_start
  local end_pos = buffer.selection_end
  local start_line_pos = tavi.pos.start_line(start_pos)
  local end_line_pos = tavi.pos.end_line(end_pos, true)
  tavi.sel(start_line_pos, end_line_pos)
end

keys.visual_line = make_canonical_movements(tavi.select)
keys.visual_line[':'] = function () ui.command_entry.enter_mode('lua_command', 'lua') end
keys.visual_line['<'] = buffer.back_tab
keys.visual_line['>'] = buffer.tab
keys.visual_line['~'] = function ()
  extend_selection_to_line()
  tavi.change_character_case()
  tavi.enter_mode('normal')
end
keys.visual_line['esc'] = function () tavi.enter_mode('normal') end
keys.visual_line['c['] = function () tavi.enter_mode('normal') end
keys.visual_line['cv'] = function () tavi.enter_mode('visual_block') end
keys.visual_line['v'] = function () tavi.enter_mode('visual') end
keys.visual_line['x'] = function ()
  tavi.state.paste_mode = tavi.PASTE_LINE
  extend_selection_to_line()
  buffer:cut()
  tavi.enter_mode('normal')
end
keys.visual_line['d'] = keys.visual_line['x']
keys.visual_line['y'] = function ()
  tavi.state.paste_mode = tavi.PASTE_LINE
  extend_selection_to_line()
  buffer:copy()
  tavi.enter_mode('normal')
end
keys.visual_line['c'] = function ()
  tavi.state.paste_mode = tavi.PASTE_LINE
  extend_selection_to_line()
  buffer:cut()
  tavi.enter_mode(nil)
end

-- Prevent Fallthrough to Insert Mode
local prevent_fallthrough = function (t)
  local mt = getmetatable(t) or {}
  local mt_index = mt.__index or function () end
  setmetatable(t, {
    __index = function (t, k)
      return mt_index(t,k) or function () end
    end,
  })
end
local modes = { 'normal', 'visual', 'visual_line', 'visual_block' }
for k, m in ipairs(modes) do prevent_fallthrough(keys[m]) end

-- Insert Mode
keys['esc'] = function () tavi.enter_mode('normal') end
keys['c['] = function () tavi.enter_mode('normal') end

return tavi
