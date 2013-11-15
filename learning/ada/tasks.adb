with ada.text_io;

procedure tasks is
    use ada.text_io;

    task PRINT_X;
    task body PRINT_X is
    begin
        delay 10.0;
        put_line("Hello from task X!");
    end PRINT_X;

    task PRINT_Y;
    task body PRINT_Y is
    begin
        put_line("Hello from task Y!");
    end PRINT_Y;
begin
    put_line("Hello from the main task!");
end tasks;
