local function binary_rep(k)
  if k == 0 then
    return {}
  else
    rep = binary_rep((k-(k%2))/2)
    table.insert(rep, k%2)
    return rep
  end
end

function power(a, n)
  rep = binary_rep(n)
  ret = 1
  for _,bit in ipairs(rep) do
    if bit == 0 then
      ret = ret*ret
    else
      ret = ret*ret*a
      n = (n-1)/2
    end
  end
  return ret
end
