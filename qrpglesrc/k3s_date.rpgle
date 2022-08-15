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
      **   Subprocedures for date processing
      *****************************************************************
     h NOMAIN
     h OPTION(*NODEBUGIO)
      * --------------------------------------------------- parameter passed prototype
     d/copy qprotosrc
       //****************************************************************
       //  Subprocedure: K3S_Retrieve_Timestamp
       //  Type: ILE RPG Subprocedure
       //  Desc: Retrieve date and time: Timestamp
       //  (Copied from K3S_M090)
       //****************************************************************
     p K3S_Retrieve_Timestamp...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  time_stamp                     z
      *****************************************************************
      /free
       time_stamp = %Timestamp();
      /end-free
     P K3S_Retrieve_Timestamp...
     P                 E

      *****************************************************************
      **   Name: K3S_Convert_ISO_Date
      **   Type: ILE RPG Subprocedure
      **   Desc: Convert *ISO date into any date format, including a
      **         six digit format for data entry
      **   (Copied from K3S_M100)
      *****************************************************************
     p K3S_Convert_ISO_Date...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  date_in                        d   Const datfmt(*iso)
     d  date_type                     4    Const
     d  date_six                      1p 0 Const
     d  date_send                    10

      * --------------------------------------------------- Date fields
     d date_mdy        s               d   datfmt(*mdy)
     d date_dmy        s               d   datfmt(*dmy)
     d date_ymd        s               d   datfmt(*ymd)
     d date_iso        s               d   datfmt(*iso)
     d date_jul        s               d   datfmt(*jul)
     d date_usa        s               d   datfmt(*usa)
     d date_eur        s               d   datfmt(*eur)
     d date_jis        s               d   datfmt(*jis)

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
     d date_check      s             10
     d date_interm     s               d

      /free
       clear date_out;
       date_check = %char(date_in:*iso);
       if        (date_check >= '1940-01-01') and
                 (date_check <= '2039-12-31');

                 date_mdy = date_in;
                 date_dmy = date_in;
                 date_ymd = date_in;
                 date_iso = date_in;
                 date_jul = date_in;
                 date_usa = date_in;
                 date_eur = date_in;
                 date_jis = date_in;

       //   Put into full edited format
                 select;

                    when      date_type = '*MDY';
                              date_out = %char(date_mdy:*mdy);

                    when      date_type = '*DMY';
                              date_out = %char(date_dmy:*dmy);

                    when      date_type = '*YMD';
                              date_out = %char(date_ymd:*ymd);

                    when      date_type = '*ISO';
                              date_out = %char(date_iso:*iso);

                    when      date_type = '*JUL';
                              date_out = %char(date_jul:*jul);

                    when      date_type = '*USA';
                              date_out = %char(date_usa:*usa);

                    when      date_type = '*EUR';
                              date_out = %char(date_eur:*eur);

                    when      date_type = '*JIS';
                              date_out = %char(date_jis:*jis);

                 endsl;
       //   Put into six digit format
                 if        date_six = 1;

                           date_work = %triml(date_out);

                           clear date_out;

                           select;

                              when      date_type = '*MDY';
                                        date_interm = %date(date_work:*mdy);
                                        date_out  = %char(date_interm:*mdy0);

                              when      date_type = '*DMY';
                                        date_interm = %date(date_work:*dmy);
                                        date_out  = %char(date_interm:*dmy0);

                              when      date_type = '*YMD';
                                        date_interm = %date(date_work:*ymd);
                                        date_out  = %char(date_interm:*ymd0);


                              when      date_type = '*ISO';
                                        date_interm = %date(date_work:*ymd);
                                        date_out  = %char(date_interm:*ymd0);

                              when      date_type = '*JUL';
                                        date_interm = %date(date_work:*jul);
                                        date_out  = %char(date_interm:*jul0);

                              when      date_type = '*USA';
                                        date_interm = %date(date_work:*mdy);
                                        date_out  = %char(date_interm:*mdy0);

                              when      date_type = '*EUR';
                                        date_interm = %date(date_work:*dmy);
                                        date_out  = %char(date_interm:*dmy0);

                              when      date_type = '*JIS';
                                        date_interm = %date(date_work:*ymd);
                                        date_out  = %char(date_interm:*ymd0);

                           endsl;

                 endif;

                 clear date_work;

                 date_work = %triml(date_out);

                 evalr date_send = date_work;

       else;

          date_send = *blanks;
       endif;

      /end-free
     p K3S_Convert_ISO_Date...
     p                 E

      *****************************************************************
      **   Name: K3S_Convert_ISO_Time
      **   Type: ILE RPG Subprocedure
      **   Desc: Convert *ISO time into any time format
      **   (Copied from K3S_M110)
      *****************************************************************
     p K3S_Convert_ISO_Time...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  time_in                        t   Const timfmt(*iso)
     d  time_type                     4    Const
     d  time_out                      8

     d time_hms        s               t   timfmt(*hms)
     d time_iso        s               t   timfmt(*iso)
     d time_usa        s               t   timfmt(*usa)
     d time_eur        s               t   timfmt(*eur)
     d time_jis        s               t   timfmt(*jis)

      /free
       time_hms = time_in;
       time_iso = time_in;
       time_usa = time_in;
       time_eur = time_in;
       time_jis = time_in;

       select;

          when      time_type = '*HMS';
                    time_out = %char(time_hms:*hms);

          when      time_type = '*ISO';
                    time_out = %char(time_iso:*iso);

          when      time_type = '*USA';
                    time_out = %char(time_usa:*usa);

          when      time_type = '*EUR';
                    time_out = %char(time_eur:*eur);

          when      time_type = '*JIS';
                    time_out = %char(time_jis:*jis);

       endsl;

      /end-free
     p K3S_Convert_ISO_Time...
     p                 E

      *****************************************************************
      **   Name: K3S_Retrieve_Date_Time
      **   Type: ILE RPG Subprocedure
      **   Desc: Retrieve user specified date and time from timestamp
      **   (Copied from K3S_M120)
      *****************************************************************
     p K3S_Retrieve_Date_Time...
     p                 B                   EXPORT
      *procedure interface
     d                 PI
     d  date_fmt                      4    Const
     d  time_fmt                      4    Const
     d  time_adj                      3  0 Const
     d  date_char                    10
     d  time_char                     8
      * --------------------------------------------------- Timestamp fields
     d time_stamp      s               z   inz
     d time            s               t   timfmt(*iso)
     d time_work       s              8    inz(*blanks)
     d date            s               d   datfmt(*iso)
     d date_work       s             10    inz(*blanks)

     d not_six         s              1p 0 inz(0)
      * ----------------------------------------------- Format date and time
      /free
       //call subprocedure to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);
       //user preferred time adjustment
       time_stamp += %hours(time_adj);
       //time = %time(%char(time_stamp):*iso);
       time = %time(%subst(%char(time_stamp):12:8):*iso);
       date = %date(%subst(%char(time_stamp):1:10):*ISO);

       //call subprocedure to convert *iso date to user specified format
       callp K3S_Convert_ISO_Date(date:date_fmt:not_six:date_work);
       date_char = %triml(date_work);

       //call subprocedure to convert *iso time to user specified format
       callp K3S_Convert_ISO_Time(time:time_fmt:time_work);

       time_char = %triml(time_work);

      /end-free
     p K3S_Retrieve_Date_Time...
     p                 e

      *****************************************************************
      **   Name: K3S_Date_To_ISO
      **   Type: ILE RPG Subprocedure
      **   Desc: Convert 10 character field into *ISO date format.
      **         Check field for validity.
      **   (Copied from K3S_M130)
      *****************************************************************
     p K3S_Date_To_ISO...
     p                 B                   EXPORT
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
     p K3S_Date_To_ISO...
     p                 E
