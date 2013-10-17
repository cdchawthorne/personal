local pw_compression = require "pw_compression"
local cdf = require "cdf"

local function compress(word)
  local reduct = word:gsub('[AaEeIiOoUuTtNn]', '')
  return reduct
end

local dictionary = '/usr/share/dict/cracklib-small'
local freqs = pw_compression.freqs(4, compress, dictionary)
crack_cdf = cdf.crack_time_cdf(freqs)
