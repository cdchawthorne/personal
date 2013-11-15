with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure years is
    use Ada.Text_IO;
    use Ada.Integer_Text_IO;

    YEAR_BORN : constant natural := 1992;
    STARTED_SCHOOL : constant natural := 5;
    GRADUATED_SCHOOL : constant natural := 18;
begin
    for year in YEAR_BORN..YEAR_BORN + 21 loop
        put("In ");
        put(year);
        put(", I was ");
        put(year-YEAR_BORN);
        if year = STARTED_SCHOOL then
            put(" years old, and started school.");
            New_Line;
        elsif year = GRADUATED_SCHOOL then
            put(" years old, and graduated high school.");
            New_Line;
        else
            put(" years old.");
            New_Line;
        end if;
    end loop;
end years;
