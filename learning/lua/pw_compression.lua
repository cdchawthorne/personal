local pw_compression = {}

local function base_freqs(compress, dictionary)
  local reducts_meta = {__index = function() return 0 end}
  local reducts = setmetatable({}, reducts_meta)
  for word in io.lines(dictionary) do
    local reduct = compress(word)
    reducts[reduct] = reducts[reduct] + 1
  end

  local freqs_meta = {__index = function() return 0 end}
  local freqs = setmetatable({}, freqs_meta)
  for _,freq in pairs(reducts) do
    freqs[freq] = freqs[freq] + 1
  end
  return freqs
end

local function freqs_product(freqs1, freqs2)
  product_meta = {__index = function() return 0 end}
  product = setmetatable({}, product_meta)
  for freq1, multiplicity1 in pairs(freqs1) do
    for freq2, multiplicity2 in pairs(freqs2) do
      new_freq = freq1 * freq2
      new_multiplicity = multiplicity1 * multiplicity2
      product[freq1 * freq2] = product[new_freq] + new_multiplicity
    end
  end

  return product
end

local function freqs_power(freqs, n)
  ret = {[1] = 1}
  for i = 0,n-1 do
    print(i)
    ret = freqs_product(ret, freqs)
  end
  return ret
end

function pw_compression.freqs(n, compress, dictionary)
  local base_freqs = base_freqs(compress, dictionary)
  return freqs_power(base_freqs, n)
end

return pw_compression
