     D NDX             S             10I 0
     D ROWSFETCHED     S             10I 0
     D ipcount         S              7s 0

     D ARRAYSIZE       S             10I 0 INZ(150)
     D INTPROD         DS                  QUALIFIED INZ OCCURS(150)
     D   IP_COMP                      1
     D   IP_LOCN                      5
     D   IP_BIRTH                      D
     D   IP_LASTUPD                    D
     D   IP_SUPL                     10
     D   IP_SUPLSUB                  10
     D   IP_SUPLUSR                  10
     D   IP_SUPLUSB                  10
     D   IP_PROD                     25
     D   IP_PRODSEQ                  40
     D   IP_DESC1                    40
     D   IP_DESC2                    40
     D   IP_MFG                      25
     D   IP_NDC_UPC                  25
     D   IP_UOM                       4
     D   IP_PACKSIZ                  10
     D   IP_TIHI                     10
     D   IP_STATUS                    1
     D   IP_MINQTY                    7  0
     D   IP_BUYMULT                   7  0
     D   IP_GROUP1                   10
     D   IP_GROUP2                   10
     D   IP_GROUP3                   10
     D   IP_GROUP4                   10
     D   IP_GROUP5                   10
     D   IP_WHSLOCN                  10
     D   IP_COSTREG                  11  4
     D   IP_COSTDIV                   5  0
     D   IP_SALES                    11  4
     D   IP_SALESW                   11  4
     D   IP_QTYOHND                   7  0
     D   IP_QTYOORD                   7  0
     D   IP_QTYBACK                   7  0
     D   IP_WEIGHT                    7  3
     D   IP_WEIGHTD                   5  0
     D   IP_VOLUME                    7  3
     D   IP_VOLUMED                   5  0
     D   IP_DLYSALE                   7  0
     D   IP_DLYOUTS                   7  0
     D   IP_TRNOORD                   7  0
     D   IP_ALTOORD                   7  0
     D   IP_FORCTYP                   1  0
     D   IP_CONVPAK                   7  0
     D   IP_PURINCR                   5  0
     D   IP_CONTFLG                   1  0
     D   IP_REBATE                    3  1
     D   IP_PROCALT                   1  0
     D   IP_MFGOUT                    1  0
     D   IP_DLYTYP1                   7  0
     D   IP_DLYTYP2                   7  0
     D   IP_DLYTYP3                   7  0
     D   IP_DLYTYP4                   7  0
     D   IP_DLYTYP5                   7  0
     D   IP_DLYTYP6                   7  0
     D   IP_DLYTYP7                   7  0
     D   IP_DLYTYP8                   7  0
     D   IP_DLYTYP9                   7  0
     D   IP_CARCOUN                   1  0
     D   IP_USERA1                    1
     D   IP_USERA2                    3
     D   IP_USERA3                   10
     D   IP_USERN1                    5  0
     D   IP_USERN2                    7  2
     D   IP_USERN3                   11  4
     D   IP_DISOTHR                   9  4
     D   IP_DISUNT7                   9  4
     D   IP_DISUNT8                   9  4
     D   IP_DISUNT9                   9  4


      * -------------------------------------------------------
     D SQLSTATEOK      C                   CONST('00000')
     D ROWNOTFOUND     C                   CONST('02000')
      * -------------------------------------------------------
     D STMTSTRING      S           5000A   VARYING INZ
     D STRING          S             40A   INZ
     D INPSRCHCND      S           3000A   VARYING INZ
      * -------------------------------------------------- PARAMETER PASSED PROTOTYPE
     D DMIPTST2        PR
     D  COMP                          1

     D DMIPTST2        PI
     D  COMP                          1

      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       // clear fields to total number of records processed for each log type
       ipcount = 0;

       exsr dclipcursor;

       exsr opnipcursor;

       // ---------------------------------------------------------- Main Loop
       // main loop
       dow SQLState = SQLStateOk;

       //fetch intprod row
           exec sql
             fetch next
               from ipcursor
               for :ArraySize rows
               into :intprod;

           RowsFetched = SqlEr3;
       // SqlEr3 holds number of records SQL fetched
           for Ndx = 1 to RowsFetched;

              ipcount +=  1;      //Increment total records processed counter

              %occur(intprod) = Ndx;

           endfor;

           if RowsFetched < ArraySize;
              leave;
           endif;

           SQLState = SQLStateOk;
       enddo;

       exsr clsipcursor;

       *inlr = '1';

       begsr dclipcursor;

       exec sql
        declare ipcursor Cursor
         for
         select *
         from k_intprod
         where ip_comp = :comp
         order by ip_comp,
                  ip_locn,
                  ip_supl,
                  ip_suplsub,
                  ip_prod
         for read only;

       endsr;

       begsr opnipcursor;

       exec sql
          open ipcursor;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;








