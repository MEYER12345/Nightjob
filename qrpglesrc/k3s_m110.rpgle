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
      **   Name: K3S_M110
      **   Type: ILE RPG Program
      **   Desc: Convert *ISO time into any time format
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
     p K3S_M110        B                   EXPORT
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

       return;
      /end-free
     p K3S_M110        e
