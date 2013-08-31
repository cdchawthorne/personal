require 'curses'

begin
  Curses.init_screen
  Curses.start_color
  Curses.addstr("Hello, world!")
  Curses.addstr(Curses.cols.to_s)
  while Curses.getstr != "foo"
  end
  Curses.attron(Curses::A_BOLD)
  Curses.addstr(Curses.cols.to_s)
  Curses.addstr("Hello, world!" * 10)
  Curses.scrl(-2)
  Curses.getstr
ensure
  Curses.close_screen
end
