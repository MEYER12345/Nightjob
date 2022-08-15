      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
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
      **   Name: K3S_9900
      **   Type: ILE RPG Program
      **   Desc: Capture entries for Guest Book
      **
      *****************************************************************
      **
      **  This program will perform 2 functions:
      **
      **        1) As users sign on to K3S-Replenish, capture information
      **           about who signed on, which workstation was used,
      **           a timestamp when job started, and the job name.
      **
      **        2) As users leave K3S-Replenish, capture information
      **           about when they left. A timestamp of when
      **           they ended will need to be captured, along with the
      **           type of exit.
      **
      **
      **        End types are:
      **
      **       'O' = Open         As users sign on to K3S-Replenish,
      **                          a record is written to file k_guestbk,
      **                          and given an 'End Type' of Open. This
      **                          field will be updated if the user
      **                          ends the job normally. However, if
      **                          the user does not end normally, then
      **                          this field will contain an 'O' forever.
      **
      **       'C' = Closed       This would be the normal value we would
      **                          expect to see in a k_guestbk record, after
      **                          the user exits from K3S-Replenish.
      **
      *****************************************************************
      **
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/28/2014.
      **  Remarks. Altered program to utilize embedded SQL statements
      **           to access and update or write to K_GUESTBK instead
      **           of using native RPGLE code. Also, added /COPY for
      **           prototypes.
      **
      *****************************************************************
      * ----------------------------------- D-specs for common workfields
     d/copy k3s_c270
                                                                                # valid locations
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9900        PI
     d  workstation                  10
     d  job_nbr                       6
      * --------------------------------------------------------- Workfields
     d*Inztimstp initialize to value '0001-01-01-00.00.00.000000'
     d inztimstp       s               z   inz(z'0001-01-01-00.00.00.000000')
     d
     d
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d guestbk_rec   e ds                  ExtName(k_guestbk)
      * ---------------------------------------------------- Local Data Area
     d/copy k3s_c030

      * ---------------------------------------------------------- Main line
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       //retrieve data area *lda
         exsr $_get_lda;

       //call module to retrieve time stamp
         callp K3S_Retrieve_Timestamp(time_stamp);

       //determine which type of mode program is in
       //  is user exiting or signing on?

       //    user is exiting from K3S-Replenish
         if workstation = '*K3S_END*';
            exec sql
              select *
                into :guestbk_rec
                from k_guestbk
                where gb_job = :job_nbr;
            if SQLState = SQLStateOk;        //row found
              exec sql
                update k_guestbk
                set gb_timeend = :time_stamp,
                    gb_endtype = 'C'
                where gb_job = :job_nbr;
            endif;
       //    user is signing into K3S-Replenish
         else;

           exec sql
             select *
               into :guestbk_rec
               from K_guestbk
               where gb_workstn = :workstation and
                     gb_job = :job_nbr;
           If SQLState = RowNotFound;    //No record found
             exec sql
               insert into k_guestbk
                         (gb_comp,
                          gb_user,
                          gb_workstn,
                          gb_job,
                          gb_endtype,
                          gb_timestr,
                          gb_timeend)
               Values    (:lda_comp,
                          :lda_userid,
                          :workstation,
                          :job_nbr,
                          'O',
                          :time_stamp,
                          :inztimstp);
           endif;

         endif;

         *inlr = *on;
      /end-free

      * --------------------------------------------------get lda subroutine
     c/copy k3s_c031
