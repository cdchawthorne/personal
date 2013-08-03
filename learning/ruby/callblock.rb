#!/usr/bin/env ruby

def callBlock
    yield #1
    yield #2
end

def putNum(num)
    puts num
end

callBlock
callBlock { | num | puts num }
