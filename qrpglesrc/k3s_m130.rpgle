      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2008 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_M130
      **   Type: ILE RPG Program
      **   Desc: Convert 10 character field into *ISO date format.
      **         Check field for validity.
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/23/2014.
      **  Remarks. Converted this code from a stand-alone program to
      **           an exportable free-format RPGLE subprocedure. Added
      **           this subprocedure to service program DATEPROCS.
      *****************************************************************
     h OPTION(*NODEBUGIO) NOMAIN
      * ----------------------------------------------Parameters passed
      /copy qprotosrc
      *****************************************************************
     p K3S_M130        B                   EXPORT
      *procedure interface
     d                 PI
     d  date_in                      10    Const
     d  date_type                     4    Const
     d  date_six                      1p 0 Const
     d  date_send                      d   datfmt(*iso)
     d  date_error                    1p 0

     d rembr_err       s              1p 0

     d                 ds
     d what_year                      2
     d  what_year1                    1    overlay(what_year:1)
     d  what_year2                    1    overlay(what_year:2)

     d year_numbr      s              2  0
     d century         s              2

     d                 ds
     d date_work                     10
     d  date_wrk01                    1    overlay(date_work:01)
     d  date_wrk02                    1    overlay(date_work:02)
     d  date_wrk03                    1    overlay(date_work:03)
     d  date_wrk04                    1    overlay(date_work:04)
     d  date_wrk05                    1    overlay(date_work:05)
     d  date_wrk06                    1    overlay(date_work:06)
     d  date_wrk07                    1    overlay(date_work:07)
     d  date_wrk08                    1    overlay(date_work:08)
     d  date_wrk09                    1    overlay(date_work:09)
     d  date_wrk10                    1    overlay(date_work:10)

     d date_out        s             10

      *****************************************************************
      /free
       clear rembr_err;
       clear date_out;

       date_work = %triml(date_in);

       //  use correct date format for editing

       select;

       //-------------------------------------------------------------- *MDY

       when      date_type = '*MDY';

       // six digit date entered   MMDDYY

          if        date_six = 1;

                    what_year = date_wrk05 +
                                date_wrk06;

                    exsr $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century = '20';
                    else;
                              century = '19';
                    endif;

                    date_out  = century    +
                                date_wrk05 +
                                date_wrk06 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04;

          // not six digit date entered MM/DD/YY
          else;

                    what_year = date_wrk07 +
                                date_wrk08;

                    exsr $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century = '20';
                    else;
                              century = '19';
                    endif;

                    date_out  = century    +
                                date_wrk07 +
                                date_wrk08 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk04 +
                                date_wrk05;
          endif;

       //-------------------------------------------------------------- *DMY

       when      date_type = '*DMY';

       // six digit date entered   DDMMYY

          if        date_six = 1;

                    what_year = date_wrk05 +
                                date_wrk06;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk05 +
                                date_wrk06 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02;

       // not six digit date entered  DD/MM/YY
          else;

                    what_year = date_wrk07 +
                                date_wrk08;
                    exsr $_edit_yr;

                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk07 +
                                date_wrk08 +
                                '-'        +
                                date_wrk04 +
                                date_wrk05 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02;
          endif;

       //-------------------------------------------------------------- *YMD

       when      date_type = '*YMD';

       // six digit date entered   YYMMDD

          if        date_six = 1;

                    what_year = date_wrk01 +
                                date_wrk02;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk05 +
                                date_wrk06;

       // not six digit date entered  YY/MM/DD
          else;

                    what_year = date_wrk01 +
                                date_wrk02;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk04 +
                                date_wrk05 +
                                '-'        +
                                date_wrk07 +
                                date_wrk08;
          endif;

       //-------------------------------------------------------------- *ISO

       when      date_type = '*ISO';

       // six digit date entered   YYMMDD

          if        date_six = 1;

                    what_year = date_wrk01 +
                                date_wrk02;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk05 +
                                date_wrk06;

       // not six digit date entered  CCYY-MM-DD
          else;

                    date_out  = date_wrk01 +
                                date_wrk02 +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk06 +
                                date_wrk07 +
                                '-'        +
                                date_wrk09 +
                                date_wrk10;
                         endif;

        //------------------------------------------------------------- *USA

       when      date_type = '*USA';

       // six digit date entered   MMDDYY

          if        date_six = 1;

                    what_year = date_wrk05 +
                                date_wrk06;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk05 +
                                date_wrk06 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04;

       // not six digit date entered  MM/DD/CCYY
          else;

                    what_year = date_wrk09 +
                                date_wrk10;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk09 +
                                date_wrk10 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk04 +
                                date_wrk05;
          endif;

       //-------------------------------------------------------------- *EUR

       when      date_type = '*EUR';

       // six digit date entered   DDMMYY

          if        date_six = 1;

                    what_year = date_wrk05 +
                                date_wrk06;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk05 +
                                date_wrk06 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02;

       // not six digit date entered  DD/MM/CCYY
          else;

                    what_year = date_wrk09 +
                                date_wrk10;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk09 +
                                date_wrk10 +
                                '-'        +
                                date_wrk04 +
                                date_wrk05 +
                                '-'        +
                                date_wrk01 +
                                date_wrk02;
          endif;

       //-------------------------------------------------------------- *JIS

       when      date_type = '*JIS';

       // six digit date entered   YYMMDD

          if        date_six = 1;

                    what_year = date_wrk01 +
                                date_wrk02;

                    exsr      $_edit_yr;
                    if        rembr_err = 0;
                              year_numbr = %dec(what_year:2:0);
                    endif;

                    if        year_numbr < 70;
                              century   = '20';
                    else;
                              century   = '19';
                    endif;

                    date_out  = century    +
                                date_wrk01 +
                                date_wrk02 +
                                '-'        +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk05 +
                                date_wrk06;

          // not six digit date enterd CCYY-MM-DD
          else;

                    date_out  = date_wrk01 +
                                date_wrk02 +
                                date_wrk03 +
                                date_wrk04 +
                                '-'        +
                                date_wrk06 +
                                date_wrk07 +
                                '-'        +
                                date_wrk09 +
                                date_wrk10;
          endif;

       //-------------------------------------------------------------------

       endsl;

       //-------------------------------------------------------------------

       //   if error from above, send back answer of date error
       if        rembr_err = 1;
                 clear date_send;
                 date_error = 1;
       else;

       //   Test input, and format into *ISO if valid
                 test(de) *iso date_out;

                 if %error;
                    date_error = 1;
                    clear date_send;
                 else;
                    date_error = 0;
                    date_send = %date(date_out:*iso);
                 endif;

       endif;

       return;

       //   Edit both positions of year for validity
       begsr $_edit_yr;
       if        what_year1 < '0' or what_year1 > '9';
                 rembr_err = 1;
       endif;
       if        what_year2 < '0' or what_year2 > '9';
                 rembr_err = 1;
       endif;
       endsr;
      /end-free
     pK3S_M130         E
