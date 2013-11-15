with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure types_test is
    type int_type is range -1..4;
    subtype sub_int_type is integer range -4..0;
    package int_type_io is new Ada.Text_IO.Integer_IO(int_type);
    use int_type_io;
    use Ada.Integer_Text_IO;

    sub_int_type_val : sub_int_type := -1;
    int_val : integer := 0;
begin
    sub_int_type_val := 0;
    exit when true;
end types_test;
