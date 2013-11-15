with Ada.Text_IO;

procedure enums is
    type DOW is (MON, TUE, WED);
    package DOW_IO is new Ada.Text_IO.enumeration_IO(DOW);
    use DOW_IO;

    type French_Actions is (APPELLE, TUE, AIME);
    package French_Actions_IO is new Ada.Text_IO.Enumeration_IO(French_Actions);
    use French_Actions_IO;

    day : DOW;
    action : French_Actions;
begin
    day := MON;
    DOW_IO.put(day);
    day := DOW'(TUE);
    DOW_IO.put(day);

    action := DOW'(TUE);
    French_Actions_IO.put(action);
end;
