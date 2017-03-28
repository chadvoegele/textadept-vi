require('textadept_headless').init({ '--userhome', os.getenv('USERHOME') or os.getenv('HOME') })
local textadept = require('textadept')
local tavi = require('textadept-vi')
local luaunit = require('luaunit')

local emit_keys = function (keys)
  for _,k in ipairs(keys) do
    events.emit(events.KEYPRESS, string.byte(k))
  end
end

local setup_buffer = function (text)
  buffer:clear_all()
  events.emit(events.RESET_AFTER)
  buffer:insert_text(-1, text)
end

local run_keys_test = function (pretext, startp, keys, posttext)
  setup_buffer(pretext)
  tavi.moveto(startp)
  emit_keys(keys)
  luaunit.assertEquals(buffer:get_text(), posttext)
end

local make_test_table = function ()
  local test_table = setmetatable({}, {
    __call = function (tests, key, ...)
      local args = table.pack(...)
      tests['test_'..key] = function () run_keys_test(table.unpack(args)) end
    end
  })
  return test_table
end

local to_chars = function (str)
  arr = {}
  for i=1,#str do
    arr[i] = string.sub(str,i,i)
  end
  return arr
end

local test_text0 = function ()
  local text = 'Potent potables.\n'
  return text
end

local test_text1 = function ()
  local text =
'The first principle is that you must not fool \n'..
'yourself and you are the easiest person to fool.\n'
  return text
end

local test_text2 = function ()
  local text =
'We hold these truths to be self-evident, \n'..
'that all men are created equal, that they \n'..
'are endowed by their Creator with certain \n'..
'unalienable Rights, that among these are Life \n'..
'Liberty and the pursuit of Happiness.\n'
  return text
end

test_normal = make_test_table()
test_normal('right', test_text0(), 4, to_chars('lx'), 'Poten potables.\n')
test_normal('left', test_text0(), 4, to_chars('hx'), 'Potnt potables.\n')
test_normal('paste_lastline', test_text2(), 129, to_chars('ddp'), 'We hold these truths to be self-evident, \nthat all men are created equal, that they \nare endowed by their Creator with certain \nLiberty and the pursuit of Happiness.\nunalienable Rights, that among these are Life \n')
test_normal('inner_word', test_text0(), 7, to_chars('diw'), 'Potent .\n')
test_normal('inner_word2', test_text0(), 8, to_chars('diw'), 'Potent .\n')

test_visual = make_test_table()
test_visual('cut', test_text0(), 4, to_chars('vx'), 'Potet potables.\n')
test_visual('right_left', test_text0(), 4, to_chars('vllhhhx'), 'Pott potables.\n')
test_visual('left_right', test_text0(), 4, to_chars('vhhlllx'), 'Pote potables.\n')
test_visual('5left', test_text0(), 4, to_chars('v5lx'), 'Poteables.\n')
test_visual('find_right', test_text0(), 1, to_chars('vfax'), 'Pbles.\n')
test_visual('right_find_right', test_text0(), 1, to_chars('vlfax'), 'Pbles.\n')
test_visual('right_find_left', test_text0(), 8, to_chars('vlFnx'), 'Potetables.\n')

test_visual_line = make_test_table()
test_visual_line('down', test_text2(), 43, to_chars('Vjx'), 'We hold these truths to be self-evident, \nunalienable Rights, that among these are Life \nLiberty and the pursuit of Happiness.\n')
test_visual_line('3down', test_text2(), 0, to_chars('V3jx'), 'Liberty and the pursuit of Happiness.\n')
test_visual_line('up', test_text2(), 43, to_chars('Vkx'), 'are endowed by their Creator with certain \nunalienable Rights, that among these are Life \nLiberty and the pursuit of Happiness.\n')
test_visual_line('2up', test_text2(), 129, to_chars('V2kx'), 'We hold these truths to be self-evident, \nLiberty and the pursuit of Happiness.\n')

os.exit(luaunit.LuaUnit.run())
