local max_heap_module = {}

function max_heap_module.empty()
  return {empty = true}
end

function max_heap_module.new(items)
  new_heap = {empty = true}
  for item in ipairs(items) do
    max_heap_module.insert(new_heap, item)
  end
end

function max_heap_module.insert(heap, item)
  if max_heap_module.is_empty(heap) then
    return {empty = false, datum = item
