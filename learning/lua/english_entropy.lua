frequencies_meta = {__index = function () return 0 end }
frequencies = setmetatable({}, frequencies_meta)

n = 0

for word in io.lines("/usr/share/dict/american-english") do
  first_char = word:sub(1,1)
  if "a" <= first_char and first_char <= "z" or "A" <= first_char and first_char <= "Z" then
    frequencies[first_char:lower()] = frequencies[first_char:lower()] + 1
    n = n+1
  end
end

entropy = 0
for _,freq in pairs(frequencies) do
  p = freq/n
  entropy = entropy + -p * math.log(p)
end

entropy = entropy / math.log(2)
print(entropy)
