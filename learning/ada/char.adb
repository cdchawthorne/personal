with ada.text_io;

procedure char is
    x : character := 'a';
begin
    for y in CHARACTER loop
        ada.text_io.put_line(CHARACTER'IMAGE(y));
    end loop;
    ada.text_io.put_line(CHARACTER'IMAGE(x));
end char;
