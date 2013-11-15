with ada.integer_text_io;

procedure mod_array is
    type mod_256 is mod 256;

    A : array (mod_256 range 200..8) of integer;
begin
    for i in A'RANGE loop
        A(i) := 0;
    end loop;
    ada.integer_text_io.put(A(5));
end mod_array;
