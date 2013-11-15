with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure year_case is
    type year_t is range 0..3000;
    type age_t is range 0..99;
    package year_text_io is new Ada.Text_IO.Integer_IO(year_t);
    package age_text_io is new Ada.Text_IO.Integer_IO(age_t);
    use age_text_io;
    use year_text_io;
    YEAR_BORN : constant year_t := 1992;
begin
    for year in YEAR_BORN..YEAR_BORN+21 loop
        put("In");
        put(year);
        put(", I was");
        put(age_t(year-YEAR_BORN));
        case year is
            when YEAR_BORN+5 =>
                put(" years old, and started school.");
            when YEAR_BORN+18 =>
                put(" years old, and graduated high school.");
            when others =>
                put(" years old.");
        end case;
        New_Line;
    end loop;
end year_case;
