      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')
      *****************************************************************
      **
      **   Name: K3S_M100
      **   Type: ILE RPG Program
      **   Desc: Convert *ISO date into any date format, including a
      **         six digit format for data entry
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/23/2014.
      **  Remarks. Converted this code from a stand-alone program to
      **           an exportable free-format RPGLE subprocedure. Added
      **           this subprocedure to service program DATEPROCS.
      **           Also, reduced size of program by utilizing RPG IV
      **           built-in functions instead of stringing fields
      **           together.
      *****************************************************************
     h OPTION(*NODEBUGIO) NOMAIN
      * ----------------------------------------------Parameters passed
      /copy qprotosrc
      *****************************************************************
     p K3S_M100        B                   EXPORT
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

       return;
      /end-free
     p K3S_M100        e
