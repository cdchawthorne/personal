local cdf_module = {}

function cdf_module.crack_time_cdf(freqs)
  freqs_array = {}
  n = 0
  for freq, multiplicity in pairs(freqs) do
    table.insert(freqs_array, {freq, multiplicity})
    n = n + freq * multiplicity
  end
  table.sort(freqs_array, function(a,b) return a[1] > b[1] end)

  local function cdf(tries)
    prob = 0
    for _,arr in ipairs(freqs_array) do
      if tries == 0 then
        break
      end
      freq, multiplicity = arr[1], arr[2]
      num_to_try = math.min(tries, multiplicity)
      prob = prob + freq * num_to_try
      tries = tries - num_to_try
    end
    prob = prob/n
    return prob
  end

  return cdf
end

return cdf_module
