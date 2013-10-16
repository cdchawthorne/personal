local function random_dist(n)
  local points = {}
  for i = 0, n-2 do
    table.insert(points, math.random())
  end
  table.sort(points)

  local probs = {}
  local prev_entry = 0
  for _, point in ipairs(points) do
    table.insert(probs, point - prev_entry)
    prev_entry = point
  end
  table.insert(probs, 1 - prev_entry)
  table.sort(probs, function(a,b) return a > b end)

  return probs
end

local function get_entropy(probs)
  ret = 0
  for _, p in ipairs(probs) do
    ret = ret + p * math.log(p)
  end

  ret = -ret / math.log(2)
  return ret
end

local function get_expected_crack_time(probs)
  ret = 0
  for i, p in ipairs(probs) do
    ret = ret + i * p
  end
  return ret
end

local function random_check(n)
  local probs = random_dist(n)
  local entropy = get_entropy(probs)
  local crack_time = get_expected_crack_time(probs)

  if crack_time < 2^(entropy-1) then
    print("Problem:")
    print(entropy, crack_time)
    os.exit(1)
  end
end

for i = 0, 1000 do
  random_check(1000)
  print(i)
end
