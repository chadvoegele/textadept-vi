local tavi = require('textadept-vi')
local luaunit = require('luaunit')

testtavi = {}

function parse_key (k, pk)
  local pk = pk or { ['shift'] = false, ['control'] = false, ['alt'] = false, ['meta'] = false }
  if #k == 1 then
    pk.key = k
    return pk
  end

  code_map = {}
  code_map['s'] = 'shift'
  code_map['c'] = 'control'
  code_map['a'] = 'alt'
  code_map['m'] = 'meta'

  local fk = string.sub(k, 1, 1)
  pk[code_map[fk]] = true
  return parse_key(string.sub(k, 2), pk)
end

local to_chars = function (str)
  arr = {}
  for i=1,#str do
    arr[i] = string.sub(str,i,i)
  end
  return arr
end

local function flatten (...)
  local s = {...}
  if #s == 1 then
    s = s[1]
  end
  local t = {}
  local ti = 1
  for _,v in ipairs(s) do
    if type(v) == 'table' then
      for _,fv in ipairs(flatten(v)) do
        t[ti] = fv
        ti = ti +1
      end
    else
      t[ti] = v
      ti = ti +1
    end
  end
  return t
end

local emit_keys = function (keys)
  for _,k in ipairs(keys) do
    local pk = parse_key(k)
    events.emit(events.KEYPRESS, string.byte(pk.key), pk.shift, pk.control, pk.alt, pk.meta)
  end
end

local setup_buffer = function (text)
  buffer:clear_all()
  events.emit(events.RESET_AFTER, { buffers_zorder = 0 })
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

-- LuaUnit TextOutput but output to string
local CaptureTextOutput = luaunit.genericOutput.new() -- derived class
local CaptureTextOutput_MT = { __index = CaptureTextOutput } -- metatable
CaptureTextOutput.__class__ = 'CaptureTextOutput'

function CaptureTextOutput.new(runner)
  local t = luaunit.genericOutput.new(runner, luaunit.VERBOSITY_DEFAULT)
  t.msg = ''
  return setmetatable( t, CaptureTextOutput_MT )
end

function CaptureTextOutput:startSuite() end
function CaptureTextOutput:startTest(testName) end

function CaptureTextOutput:endTest( node )
  if node:isPassed() then
    self.msg = self.msg..'.'
  else
    self.msg = self.msg..node.msg..string.sub(node.status, 1, 1)
  end
end

function CaptureTextOutput:displayOneFailedTest( index, fail )
  self.msg = self.msg..index..') '..fail.testName..'\n'
  self.msg = self.msg..fail.msg..'\n'
  self.msg = self.msg..fail.stackTrace..'\n'
  self.msg = self.msg..'\n'
end

function CaptureTextOutput:displayFailedTests()
  if self.result.notPassedCount ~= 0 then
    self.msg = self.msg..'Failed tests:\n'
    self.msg = self.msg..'-------------\n'
    for i, v in ipairs(self.result.notPassed) do
      self:displayOneFailedTest(i, v)
    end
  end
end

function CaptureTextOutput:endSuite()
  self.msg = self.msg..'\n'
  self:displayFailedTests()
  self.msg = self.msg..luaunit.LuaUnit.statusLine( self.result )..'\n'
  if self.result.notPassedCount == 0 then
    self.msg = self.msg..'OK\n'
  end
end

testtavi.run = function (filename)
  testtavi.filename = filename or os.tmpname()
  io.open_file(testtavi.filename)
  local runner = luaunit.LuaUnit.new()
  runner.outputType = CaptureTextOutput
  runner:runSuiteByNames(luaunit.LuaUnit.collectTests())
  buffer:set_text(runner.output.msg)
  io.save_file()
end

events.connect(events.QUIT, function ()
  if testtavi.filename then
    os.remove(testtavi.filename)
  end
end)

test_flatten = {}
test_flatten['test1'] = function ()
  luaunit.assertEquals(flatten('c', 'c'), {'c', 'c'})
end
test_flatten['test2'] = function ()
  luaunit.assertEquals(flatten({'c', 'c'}), {'c', 'c'})
end
test_flatten['test3'] = function ()
  luaunit.assertEquals(flatten('c', {'c', 'c'}), {'c', 'c', 'c'})
end
test_flatten['test4'] = function ()
  luaunit.assertEquals(flatten({'c', 'c'}, 'c'), {'c', 'c', 'c'})
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

local test_text3 = function ()
  local text =
'Unicode works ☺ too!\n'..
'Though it ☹ is a pain!\n'
  return text
end

local test_text4 = function ()
  local text =
'Tab spacing is handled\n'..
'\t\ton up and down lines.\n'..
'\tThis test checks!\n'
  return text
end

local test_text5 = function ()
  local text =
'Newton\'s Laws:\n'..
'\n'..
'An object at rest stays at rest and an object \n'..
'in motion stays in motion with the same speed \n'..
'and in the same direction unless acted upon by \n'..
'an unbalanced force.\n'..
'\n'..
'Acceleration is produced when a force acts on a mass. (F=ma)\n'..
'\n'..
'For every action there is an equal and opposite re-action.\n'
  return text
end

test_normal = make_test_table()
test_normal('right', test_text0(), 4, to_chars('lx'), 'Poten potables.\n')
test_normal('left', test_text0(), 4, to_chars('hx'), 'Potnt potables.\n')
test_normal('paste_lastline', test_text2(), 129, to_chars('ddp'), 'We hold these truths to be self-evident, \nthat all men are created equal, that they \nare endowed by their Creator with certain \nLiberty and the pursuit of Happiness.\nunalienable Rights, that among these are Life \n')
test_normal('inner_word', test_text0(), 7, to_chars('diw'), 'Potent .\n')
test_normal('inner_word2', test_text0(), 8, to_chars('diw'), 'Potent .\n')
test_normal('a_word', test_text1(), 5, to_chars('daw'), 'The principle is that you must not fool \nyourself and you are the easiest person to fool.\n')
test_normal('3word', test_text1(), 4, to_chars('d3w'), 'The is that you must not fool \nyourself and you are the easiest person to fool.\n')
test_normal('paste_here_blank_line', test_text2(), 42, to_chars('d$kywjp'), 'We hold these truths to be self-evident, \nWe \nare endowed by their Creator with certain \nunalienable Rights, that among these are Life \nLiberty and the pursuit of Happiness.\n')
test_normal('unicode_right', test_text3(), 14, to_chars('lx'), 'Unicode works ☺too!\nThough it ☹ is a pain!\n')
test_normal('unicode_right2', test_text3(), 13, to_chars('lx'), 'Unicode works  too!\nThough it ☹ is a pain!\n')
test_normal('unicode_find', test_text3(), 12, to_chars('ftx'), 'Unicode works ☺ oo!\nThough it ☹ is a pain!\n')
test_normal('unicode_find2', test_text3(), 15, to_chars('t x'), 'Unicode works  too!\nThough it ☹ is a pain!\n')
test_normal('unicode_backwards_find', test_text3(), 20, to_chars('Fsx'), 'Unicode work ☺ too!\nThough it ☹ is a pain!\n')
test_normal('unicode_replace', test_text3(), 14, to_chars('ra'), 'Unicode works a too!\nThough it ☹ is a pain!\n')
test_normal('tab_space_down', test_text4(), 3, to_chars('ljx'), 'Tab spacing is handled\n\t\tn up and down lines.\n\tThis test checks!\n')
test_normal('tab_space_2down', test_text4(), 3, to_chars('l2jx'), 'Tab spacing is handled\n\t\ton up and down lines.\n\tThs test checks!\n')
test_normal('tab_space_up', test_text4(), 62, to_chars('lkx'), 'Tab spacing is handled\n\t\ton up and dow lines.\n\tThis test checks!\n')
test_normal('repeat_find', test_text0(), 0, to_chars('ft2;x'), 'Potent poables.\n')
test_normal('repeat_backwards_find', test_text0(), 0, to_chars('$Ft;x'), 'Poten potables.\n')
test_normal('repeat_til_find', test_text0(), 0, to_chars('tt2;x'), 'Potent ptables.\n')
test_normal('repeat_backwards_til_find', test_text0(), 15, to_chars('Tt2;x'), 'Potnt potables.\n')
test_normal('repeat_reverse_find', test_text0(), 7, to_chars('fo,x'), 'Ptent potables.\n')
test_normal('repeat_reverse_backwards_find', test_text0(), 7, to_chars('Ft,x'), 'Potent poables.\n')
test_normal('repeat_reverse_til', test_text0(), 7, to_chars('tt2,x'), 'Potnt potables.\n')
test_normal('repeat_reverse_backwards_til', test_text0(), 7, to_chars('Tt,x'), 'Potent ptables.\n')
test_normal('paragraph_down', test_text5(), 0, to_chars('2}x'), 'Newton\'s Laws:\n\nAn object at rest stays at rest and an object \nin motion stays in motion with the same speed \nand in the same direction unless acted upon by \nan unbalanced force.\nAcceleration is produced when a force acts on a mass. (F=ma)\n\nFor every action there is an equal and opposite re-action.\n')

test_visual = make_test_table()
test_visual('cut', test_text0(), 4, to_chars('vx'), 'Potet potables.\n')
test_visual('right_left', test_text0(), 4, to_chars('vllhhhx'), 'Pott potables.\n')
test_visual('left_right', test_text0(), 4, to_chars('vhhlllx'), 'Pote potables.\n')
test_visual('5left', test_text0(), 4, to_chars('v5lx'), 'Poteables.\n')
test_visual('find_right', test_text0(), 1, to_chars('vfax'), 'Pbles.\n')
test_visual('right_find_right', test_text0(), 1, to_chars('vlfax'), 'Pbles.\n')
test_visual('right_find_left', test_text0(), 8, to_chars('vlFnx'), 'Potetables.\n')
test_visual('unicode_cut', test_text3(), 14, to_chars('vx'), 'Unicode works  too!\nThough it ☹ is a pain!\n')
test_visual('unicode_inside_paren', 'a(b☺)c', 4, to_chars('vi)x'), 'a()c')

test_visual_line = make_test_table()
test_visual_line('down', test_text2(), 43, to_chars('Vjx'), 'We hold these truths to be self-evident, \nunalienable Rights, that among these are Life \nLiberty and the pursuit of Happiness.\n')
test_visual_line('3down', test_text2(), 0, to_chars('V3jx'), 'Liberty and the pursuit of Happiness.\n')
test_visual_line('up', test_text2(), 43, to_chars('Vkx'), 'are endowed by their Creator with certain \nunalienable Rights, that among these are Life \nLiberty and the pursuit of Happiness.\n')
test_visual_line('2up', test_text2(), 129, to_chars('V2kx'), 'We hold these truths to be self-evident, \nLiberty and the pursuit of Happiness.\n')

test_visual_block = make_test_table()
test_visual_block('cutrightdown', test_text2(), 0, flatten('cv', to_chars('llljjjx')), 'old these truths to be self-evident, \n all men are created equal, that they \nendowed by their Creator with certain \nienable Rights, that among these are Life \nLiberty and the pursuit of Happiness.\n')
test_visual_block('cutupleft', test_text2(), 152, flatten('cv', to_chars('hhhkkkx')), 'We hold these truths e self-evident, \nthat all men are creaequal, that they \nare endowed by their tor with certain \nunalienable Rights, tamong these are Life \nLiberty and the pursuit of Happiness.\n')
test_visual_block('unicode_cut', test_text3(), 14, flatten('cv', to_chars('lx')), 'Unicode works too!\nThough it ☹ is a pain!\n')
test_visual_block('replace_char', test_text2(), 0, flatten(to_chars('4l'), 'cv', to_chars('4jra')), 'We hald these truths to be self-evident, \nthataall men are created equal, that they \nare andowed by their Creator with certain \nunalaenable Rights, that among these are Life \nLibeaty and the pursuit of Happiness.\n')

return testtavi
