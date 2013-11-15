with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;

procedure entries is
    task print_int is
        entry first(X : integer);
        entry second(X: integer);
    end;

    -- task print_string is
        -- entry first(Y : string);
        -- entry second(Y : string);
    -- end;

    task body print_int is
    begin
        put_line("Started print_int");
        accept first(X : integer) do
            put(X);
            new_line;
        end first;
        put_line("Mid-print_int");
        accept second(X : integer) do
            put(X);
            new_line;
        end second;
        put_line("End print-int");
    end print_int;
begin
    put_line("Begin main code");

    print_int.first(3);
    put_line("Mid-main code");
    print_int.second(5);
end entries;
