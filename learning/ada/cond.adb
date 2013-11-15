with Ada.Text_IO;

procedure cond is
    use Ada.Text_IO;
begin
    if TRUE then
        Put_Line("True");
    else
        Put_Line("False");
    end if;
end cond;
