with Ada.Integer_Text_IO;

procedure overflow is
    var : Integer := INTEGER'last;
begin
    Ada.Integer_Text_IO.Put(NATURAL'last + 1);
end overflow;
