with ada.integer_text_io; use ada.integer_text_io;

procedure struct_sharing is
    type Date is
        record
            day : integer range 1..31;
            month : integer range 1..12;
            year : integer;
        end record;

    type access_date is access date;

    type Person is
        record
            identifier : integer;
            birthday : access_date;
        end record;

    fiber_day : access_date := new date;
    me : person := (28, fiber_day);
    fiber : person := (496, fiber_day);
begin
    fiber_day.all := (15, 6, 1992);
    me.birthday.day := 1;
    put(fiber_day.day);
    put(fiber_day.month);
    put(fiber_day.year);
end struct_sharing;
