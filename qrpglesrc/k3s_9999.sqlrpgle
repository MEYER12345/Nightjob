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
      **   Name: K3S_9999
      **   Type: ILE RPG Program
      **   Desc: Night job log timestamp record
      **
      *****************************************************************
      **
      **  This program is used to create a snapshot of time for the
      **  night job program, at each important step.
      **
      *****************************************************************

      *_nitelogauf a e           k disk    rename(rk_nitelog : r1_nitelog)      suppliers
      *                                    prefix(n0_:3)
      * night job log control

      *_nitelogbo  a e             disk                                         table codes
      * night job log entries

      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- parameter passed
     d K3S_9999        PI
     d  comp                          1
     d  task                          1
     d  restart                       6
     d  series                        7  0
     d  altproc                       1
     d  logstrt                      26
     d  logend                       26
     d  suplchg                       1
     d  daystrt                      26
     d  dayend                       26
     d  prend12                       1
     d  prend13                       1
     d  prend52                       1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -----------------------------------------------------------------------
     d nitelog_rec   e ds                  ExtName(k_nitelog)
     d*
     d save_stamp      s               z   inz
     d time_stamp      s               z   inz
     d timestp         s               z   inz
     d duration        s             15p 0                                      message ID

     d misec_aft       s              6                                         message ID
     d misec_bef       s              6                                         message ID
     d aft_time        s             26                                         message ID
     d bef_time        s             26                                         message ID
     d #series         s              7  0

      * -----------------------------------------------------------------------
      /free
       exec sql
         set option commit = *none,
                    datfmt = *iso,
                    closqlcsr = *endactgrp;

       //call module to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);

        //Control **********************************************************
       exsr dclnjcursor;
       exsr opnnjcursor;
       if SQLState = SQLStateOk;
          exec sql
            fetch next
              from njcursor
              into :nitelog_rec;
       endif;
       If  SqlStt = SQLStateOk;
        //---  Good start to new night job. Last night job completed OK.
           if task = 'B' AND nl_restart = *blanks;
              nl_series += 1;
              series = nl_series;
           endif;
        //---  Send restart value back to CL program
           if task = 'B';
              restart = nl_restart;
           endif;
        //---  Send CL PARMs since this is either Restart or Single run
           if task = 'B' AND nl_restart <> *blanks;
              altproc = nl_altproc;
              logstrt = nl_logstrt;
              logend  = nl_logend;
              suplchg = nl_suplchg;
              daystrt = nl_daystrt;
              dayend  = nl_dayend;
              prend12 = nl_prend12;
              prend13 = nl_prend13;
              prend52 = nl_prend52;
              series  = nl_series;
           endif;
        //---  Record where you are and store CL PARMs
           if task <> 'B';
              nl_restart = restart;
              nl_altproc = altproc;
              nl_logstrt = logstrt;
              nl_logend = logend;
              nl_suplchg = suplchg;
              nl_daystrt = daystrt;
              nl_dayend = dayend;
              nl_prend12 = prend12;
              nl_prend13 = prend13;
              nl_prend52 = prend52;
           endif;
           save_stamp = nl_timestp;
           nl_timestp = time_stamp;
           exsr update_cnitelog;
           exsr clsnjcursor;
       else;
           exsr insert_cnitelog;
       endif;
       #series = nl_series;
        //******************************************************************

        //Common fields
             nl_comp = comp;
             nl_series = #series;
             nl_timestp = time_stamp;
             nl_restart = restart;
             nl_altproc = altproc;
             nl_logstrt = logstrt;
             nl_logend  = logend;
             nl_suplchg = suplchg;
             nl_daystrt = daystrt;
             nl_dayend  = dayend;
             nl_prend12 = prend12;
             nl_prend13 = prend13;
             nl_prend52 = prend52;
             nl_dursecs = 0;
             if nl_timestp > save_stamp;
             duration = %diff(nl_timestp:save_stamp:*seconds);
         //nl_timestp    subdur    save_stamp    duration:*s
             if duration > 0 AND duration <= 9999999;
                nl_dursecs = duration;
             endif;
             //timestamp format to alpha field
             bef_time = %char(save_stamp);
             //move rightmost 6 chars to misec_bef
             evalr misec_bef = bef_time;
             //timestamp format to alpha field
             aft_time = %char(nl_timestp);
             evalr misec_aft = aft_time;
             if misec_bef > misec_aft;
               nl_dursecs = nl_dursecs + 1;
             endif;
          endif;

        //Begin
          if task = 'B';
             nl_logtype = 'B';
             nl_dursecs = 0;
          endif;
        //Process
          if task = 'P';
             nl_logtype = 'P';
             evalr nl_logstep = nl_restart;
             Select;
               When nl_logstep = '00010';
                   nl_program = 'K3S_X050CL';
               When nl_logstep = '00020';
                   nl_program = 'K3S_9020  ';
               When nl_logstep = '00025';
                   nl_program = 'K3S_3360  ';
               When nl_logstep = '00030';
                   nl_program = 'K3S_X055CL';
               When nl_logstep = '00040';
                   nl_program = 'K3S_2005  ';
               When nl_logstep = '00050';
                   nl_program = 'K3S_X060CL';
               When nl_logstep = '00060';
                   nl_program = 'K3S_3005CL';
               When nl_logstep = '00070';
                   nl_program = 'K3S_X070CL';
               When nl_logstep = '00074';
                   nl_program = 'CPYF      ';
               When nl_logstep = '00077';
                   nl_program = 'K3S_X077CL';
               When nl_logstep = '00080';
                   nl_program = 'K3S_3460  ';
               When nl_logstep = '00090';
                   nl_program = 'K3S_X080CL';
               When nl_logstep = '00100';
                   nl_program = 'K3S_9016CL';
               When nl_logstep = '00110';
                   nl_program = 'K3S_X090CL';
               When nl_logstep = '00120';
                   nl_program = 'K3S_9015CL';
               When nl_logstep = '00130';
                   nl_program = 'K3S_X100CL';
               When nl_logstep = '00140';
                   nl_program = 'K3S_8530  ';
               When nl_logstep = '00150';
                   nl_program = 'K3S_X110CL';
               When nl_logstep = '00160';
                   nl_program = 'K3S_9010  ';
               When nl_logstep = '00170';
                   nl_program = 'K3S_X120CL';
               When nl_logstep = '00180';
                   nl_program = 'K3S_8550  ';
               When nl_logstep = '00190';
                   nl_program = 'K3S_8560  ';
               When nl_logstep = '00200';
                    nl_program = 'K3S_X130CL';
               When nl_logstep = '00210';
                    nl_program = 'K3S_1120  ';
               When nl_logstep = '00220';
                    nl_program = 'K3S_X134CL';
               When nl_logstep = '00230';
                    nl_program = 'K3S_3042  ';
               When nl_logstep = '00240';
                    nl_program = 'K3S_X137CL';
               When nl_logstep = '00250';
                    nl_program = 'K3S_X140CL';
               When nl_logstep = '00260';
                    nl_program = 'K3S_3600CL';
               When nl_logstep = '00270';
                    nl_program = 'K3S_X145CL';
               When nl_logstep = '00280';
                    nl_program = 'K3S_3011  ';
               When nl_logstep = '00290';
                    nl_program = 'K3S_X150CL';
               When nl_logstep = '00300';
                    nl_program = 'K3S_1500  ';
               When nl_logstep = '00303';
                    nl_program = 'K3S_X155CL';
               When nl_logstep = '00306';
                    nl_program = 'K3S_3045  ';
               When nl_logstep = '00310';
                    nl_program = 'K3S_X160CL';
               When nl_logstep = '00320';
                    nl_program = 'K3S_9110CL';
               When nl_logstep = '00330';
                    nl_program = 'K3S_X170CL';
               When nl_logstep = '00340';
                    nl_program = 'K3S_9160  ';
               When nl_logstep = '00350';
                    nl_program = 'K3S_9170  ';
               When nl_logstep = '00360';
                    nl_program = 'K3S_X180CL';
               When nl_logstep = '00370';
                    nl_program = 'K3S_1520  ';
               When nl_logstep = '00380';
                    nl_program = 'K3S_X190CL';
               When nl_logstep = '00390';
                    nl_program = 'K3S_X200CL';
               When nl_logstep = '00400';
                    nl_program = 'K3S_4020  ';
               When nl_logstep = '00410';
                    nl_program = 'K3S_X210CL';
               When nl_logstep = '00420';
                    nl_program = 'K3S_3110  ';
               When nl_logstep = '00430';
                    nl_program = 'K3S_X220CL';
               When nl_logstep = '00440';
                    nl_program = 'K3S_3110  ';
               When nl_logstep = '00450';
                    nl_program = 'K3S_X230CL';
               When nl_logstep = '00460';
                    nl_program = 'K3S_3110  ';
               When nl_logstep = '00470';
                    nl_program = 'K3S_X240CL';
               When nl_logstep = '00475';
                    nl_program = 'K3S_7100CL';
               When nl_logstep = '00480';
                    nl_program = 'K3S_X250CL';
               When nl_logstep = '00490';
                    nl_program = 'K3S_9047  ';
               When nl_logstep = '00500';
                    nl_program = 'K3S_X280CL';
               When nl_logstep = '00510';
                    nl_program = 'K3S_3910CL';
               When nl_logstep = '00520';
                    nl_program = 'K3S_X290CL';
               When nl_logstep = '00530';
                    nl_program = 'K3S_9021  ';
               When nl_logstep = '00540';
                    nl_program = 'K3S_9200  ';
               When nl_logstep = '00545';
                    nl_program = 'K3S_1053CL';
               When nl_logstep = '00550';
                    nl_program = 'K3S_X300CL';
               When nl_logstep = '00560';
                    nl_program = 'K3S_9046  ';
              When nl_logstep = '00570';
                    nl_program = 'K3S_3125CL';
               When nl_logstep = '00575';
                    nl_program = 'K3S_3920  ';
               When nl_logstep = '00577';
                    nl_program = 'K3S_3433  ';
               When nl_logstep = '00580';
                    nl_program = 'CHGDTAARA ';
               When nl_logstep = '00590';
                    nl_program = 'K3S_X310CL';
               Endsl;
         Endif;
        //Error
         If task = 'E';
            nl_logtype = 'E';
         Endif;
        //Finished
         If task = 'F';
            exec sql
              select nl_timestp
                into :timestp
                from k_nitelog
                where nl_comp = :comp and
                      nl_logtype = 'B' and
                      nl_series = :#series;
            // read rl_nitelog                          27
            if SQLState = SQLStateOk;
               nl_dursecs = 0;
               if time_stamp > timestp;
                  duration = %diff(time_stamp:timestp:*seconds);
         //nl_timestp    subdur    n0_timestp    duration:*s
                  if duration > 0 AND duration <= 9999999;
                     nl_dursecs = duration;
                  endif;
             //timestamp format to alpha field
                  bef_time = %char(timestp);
             //move rightmost 6 chars to misec_bef
                  evalr misec_bef = bef_time;
             //timestamp format to alpha field
                  aft_time = %char(time_stamp);
                  evalr misec_aft = aft_time;
                  if misec_bef > misec_aft;
                     nl_dursecs = nl_dursecs + 1;
                  endif;
               endif;
            endif;
            nl_logtype = 'F';
            clear nl_restart;
            clear nl_logstep;
            clear nl_program;
         endif;

         exsr insert_nitelog;

         if nl_logtype = 'F';
            *inlr = *on;
         else;
            return;
         endif;

       begsr dclnjcursor;
       //Get control row (log type = 'C') for this company from K_NITELOG into result set
       exec sql
        declare njcursor cursor
          for
         select *
           from k_nitelog
               where nl_comp = :comp and
                     nl_logtype = 'C'
               for update of nl_series,
                             nl_restart,
                             nl_altproc,
                             nl_logstrt,
                             nl_logend,
                             nl_suplchg,
                             nl_daystrt,
                             nl_dayend,
                             nl_prend12,
                             nl_prend13,
                             nl_prend52,
                             nl_timestp;
       endsr;

       begsr opnnjcursor;
       exec sql
        open njcursor;
        if SQLState <> SQLStateOk;
           exsr clsnjcursor;
           exec sql
            open njcursor;
        endif;
       endsr;

       begsr update_cnitelog;

       exec sql
         Update k_nitelog
            Set nl_series  = :nl_series,
                nl_restart = :nl_restart,
                nl_altproc = :nl_altproc,
                nl_logstrt = :nl_logstrt,
                nl_logend  = :nl_logend,
                nl_suplchg = :nl_suplchg,
                nl_daystrt = :nl_daystrt,
                nl_dayend  = :nl_dayend,
                nl_prend12 = :nl_prend12,
                nl_prend13 = :nl_prend13,
                nl_prend52 = :nl_prend52,
                nl_timestp = :nl_timestp
                where current of njcursor;

       endsr;

       begsr insert_cnitelog;

       exec sql
         Insert into k_nitelog
           (nl_comp,
            nl_logtype,
            nl_series,
            nl_timestp,
            nl_logstep,
            nl_program,
            nl_dursecs,
            nl_restart,
            nl_altproc,
            nl_logstrt,
            nl_logend,
            nl_suplchg,
            nl_daystrt,
            nl_dayend,
            nl_prend12,
            nl_prend13,
            nl_prend52)
         values(:comp,
                'C',
                1,
                :time_stamp,
                ' ',
                ' ',
                0,
                :restart,
                :altproc,
                :logstrt,
                :logend,
                :suplchg,
                :daystrt,
                :dayend,
                :prend12,
                :prend13,
                :prend52);
       endsr;

       begsr insert_nitelog;

       exec sql
         Insert into k_nitelog
           (nl_comp,
            nl_logtype,
            nl_series,
            nl_timestp,
            nl_logstep,
            nl_program,
            nl_dursecs,
            nl_restart,
            nl_altproc,
            nl_logstrt,
            nl_logend,
            nl_suplchg,
            nl_daystrt,
            nl_dayend,
            nl_prend12,
            nl_prend13,
            nl_prend52)
       values(:nl_comp,
              :nl_logtype,
              :nl_series,
              :nl_timestp,
              :nl_logstep,
              :nl_program,
              :nl_dursecs,
              :nl_restart,
              :nl_altproc,
              :nl_logstrt,
              :nl_logend,
              :nl_suplchg,
              :nl_daystrt,
              :nl_dayend,
              :nl_prend12,
              :nl_prend13,
              :nl_prend52);
       endsr;

       begsr clsnjcursor;
       exec sql
         close njcursor;
       endsr;

      /end-free
