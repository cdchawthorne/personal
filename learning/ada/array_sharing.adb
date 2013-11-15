with ada.integer_text_io;

procedure array_sharing is
    type DAY is (SATURDAY, SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY,
                 FRIDAY);
    type SIX_INTS is array (FRIDAY..WEDNESDAY) of integer;
    A, B : SIX_INTS;
begin
    A := (0,1,2,3,4,5);
    B := A;
    A(SUNDAY) := 128;
    ada.integer_text_io.put(A(SUNDAY));
    ada.integer_text_io.put(B(SUNDAY));
end array_sharing;
