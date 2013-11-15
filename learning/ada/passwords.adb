with Ada.float_Text_IO;
procedure passwords is
    alph_weak : constant := 26.0*2.0 + 10.0;
    alph_strong : constant := (26.0 + 21.0)*2.0 + 1.0;
begin
    Ada.float_Text_IO.put(alph_strong ** 8.0 / alph_weak ** 8.0);
end;
