IDENTIFICATION DIVISION.
PROGRAM-ID. TABLETEST.

DATA DIVISION.
WORKING-STORAGE SECTION.

    01 DEPARTMENT-TABLE.
        05 DEPARTMENT-ENTRY OCCURS 5 TIMES
        INDEXED BY DEPT-INDEX.

        10 DEPT-CODE PIC X(3).
        10 DEPT-NAME PIC X(20).

    01 SEARCH-DEPT-CODE PIC X(3).

PROCEDURE DIVISION.
    MOVE "IT" TO DEPT-CODE(1).
    MOVE "Information technology" TO DEPT-NAME(1).

    MOVE "HR" TO DEPT-CODE(2).
    MOVE "Human Resources" TO DEPT-NAME(2).

    MOVE "FIN" TO DEPT-CODE(3).
    MOVE "Finance" TO DEPT-NAME(3).

    MOVE "SAL" TO DEPT-CODE(4).
    MOVE "Sales" TO DEPT-NAME(4).

    MOVE "ADM" TO DEPT-CODE(5).
    MOVE "Administration" TO DEPT-NAME(5).

    DISPLAY "Department List".
    DISPLAY "==============================".

    DISPLAY "Code : " DEPT-CODE(1)
        " Name : " DEPT-NAME(1).

    DISPLAY "Code : " DEPT-CODE(2)
        " Name : " DEPT-NAME(2).

    DISPLAY "Code : " DEPT-CODE(3)
        " Name : " DEPT-NAME(3).

    DISPLAY "Code : " DEPT-CODE(4)
        " Name : " DEPT-NAME(4).

    DISPLAY "Code : " DEPT-CODE(5)
        " Name : " DEPT-NAME(5).


    DISPLAY "Enter Department Code: "
        WITH NO ADVANCING.

    ACCEPT SEARCH-DEPT-CODE.

    SET DEPT-INDEX TO 1.

    SEARCH DEPARTMENT-ENTRY

        AT END
            DISPLAY "Department not found."

        WHEN DEPT-CODE(DEPT-INDEX) =
            SEARCH-DEPT-CODE

            DISPLAY "Department Found: "
                DEPT-NAME(DEPT-INDEX)

    END-SEARCH.

STOP RUN.

