with ada.text_io;

procedure modulo is
    type mod_256 is mod 256;
    package mod_256_text_io is new Ada.Text_IO.modular_io(mod_256);
    use mod_256_text_io;
    x : mod_256;
begin
    x := 3;
    Put(x);
    x := x + 255;
    Put(x);
end modulo;
