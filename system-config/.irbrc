require 'irb/ext/save-history'
#History configuration
IRB.conf[:SAVE_HISTORY] = 1000000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"
