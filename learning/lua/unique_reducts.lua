local function test_compression(chars)
  local to_null = "[" .. chars:upper() .. chars:lower() .. "]"
  local reducts_meta = {__index = function() return 0 end}
  local reducts = setmetatable({}, reducts_meta)
  for word in io.lines("/usr/share/dict/cracklib-small") do
    local reduct = word:gsub(to_null, "")
    reducts[reduct] = reducts[reduct] + 1
  end

  local total_length = 0
  local n = 0
  for k,v in pairs(reducts) do
    if v > 50 then
      reducts[k] = nil
    else
      total_length = total_length + #k * v
      n = n + v
    end
  end

  local entropy = 0
  for reduct, freq in pairs(reducts) do
    local p = freq/n
    entropy = entropy + p * math.log(p)
  end
  entropy = -entropy/math.log(2)
  local expected_length = total_length/n
  return entropy, expected_length, entropy / expected_length
end

print("Base:")
print(test_compression("-"))
print("")

-- print("Vowels")
-- print(test_compression("a"))
-- print(test_compression("ae"))
-- print(test_compression("aei"))
-- print(test_compression("aeio"))
-- print(test_compression("aeiou"))
-- print(test_compression("aeiouy"))
-- print("")

-- print("Frequency")
-- print(test_compression("e"))
-- print(test_compression("et"))
-- print(test_compression("eta"))
-- print(test_compression("etao"))
-- print(test_compression("etaoi"))
-- print(test_compression("etaoin"))
-- print(test_compression("etaoinshrdl"))
-- print("")

print("Both")
print(test_compression("etaoinu"))
print("")

print("Rares")
print(test_compression("xzqjk"))

print("Compression")
print(test_compression("etaoinu"))
print("")

-- print("Expected crack time")
-- print(expected_cracking_time("etaoinu"))
-- print("")
